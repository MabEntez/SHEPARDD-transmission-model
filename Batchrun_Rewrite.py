"""
Batchrunner
===========

A single class to manage a batch run or parameter sweep of a given model.

"""
import copy
import itertools
import random
import collections
from collections import OrderedDict
from functools import partial
from itertools import count, product
from multiprocessing import Pool, cpu_count
from warnings import warn
from typing import (
    Any,
    Dict,
    Iterable,
    List,
    Mapping,
    Optional,
    Tuple,
    Type,
    Union,
)

import pandas as pd
from tqdm import tqdm

from mesa.model import Model


def batch_run(
    model_cls: Type[Model],
    parameters_list: List[Mapping[str, Union[Any, Iterable[Any]]]],
    number_processes: Optional[int] = 1,
    iterations: int = 1,
    data_collection_period: int = -1,
    offset: int = 0,  # Add the offset parameter here
    max_steps: int = 1000,
    display_progress: bool = True,
) -> List[Dict[str, Any]]:
    """Batch run a mesa model with a set of parameter values.

    Parameters
    ----------
    model_cls : Type[Model]
        The model class to batch-run
    parameters_list : list[Mapping[str, Union[Any, Iterable[Any]]]],
        Dictionary list with model parameters over which to run the model. You can either pass single values or iterables.
    number_processes : int, optional
        Number of processes used, by default 1. Set this to None if you want to use all CPUs.
    iterations : int, optional
        Number of iterations for each parameter combination, by default 1
    data_collection_period : int, optional
        Number of steps after which data gets collected, by default -1 (end of episode)
    max_steps : int, optional
        Maximum number of model steps after which the model halts, by default 1000
    display_progress : bool, optional
        Display batch run process, by default True

    Returns
    -------
    List[Dict[str, Any]]
        [description]
    """

    runs_list = []
    run_id = 0
    for parameters in parameters_list:
        for iteration in range(iterations):
            for kwargs in _make_model_kwargs(parameters):
                runs_list.append((run_id, iteration, kwargs))
                run_id += 1

    process_func = partial(
        _model_run_func,
        model_cls,
        max_steps=max_steps,
        data_collection_period=data_collection_period,
        offset=offset,  # Pass the offset parameter here
    )

    results: List[Dict[str, Any]] = []

    with tqdm(total=len(runs_list), disable=not display_progress) as pbar:
        if number_processes == 1:
            for run in runs_list:
                data = process_func(run)
                results.extend(data)
                pbar.update()
        else:
            with Pool(number_processes) as p:
                for data in p.imap_unordered(process_func, runs_list):
                    results.extend(data)
                    pbar.update()

    return results



def _make_model_kwargs(
    parameters: Mapping[str, Union[Any, Iterable[Any]]]
) -> List[Dict[str, Any]]:
    """Create model kwargs from parameters dictionary.

    Parameters
    ----------
    parameters : Mapping[str, Union[Any, Iterable[Any]]]
        Single or multiple values for each model parameter name

    Returns
    -------
    List[Dict[str, Any]]
        A list of all kwargs combinations.
    """
    parameter_list = []
    for param, values in parameters.items():
        if isinstance(values, collections.abc.Iterable) and not isinstance(values, str):
            all_values = [(param, value) for value in values]
        else:
            all_values = [(param, values)]
        parameter_list.append(all_values)
    all_kwargs = itertools.product(*parameter_list)
    kwargs_list = [dict(kwargs) for kwargs in all_kwargs]
    return kwargs_list


def _model_run_func(
    model_cls: Type[Model],
    run: Tuple[int, int, Dict[str, Any]],
    max_steps: int,
    data_collection_period: int,
    offset: int,  # Add the offset parameter here
) -> List[Dict[str, Any]]:
    """Run a single model run and collect model and agent data.

    Parameters
    ----------
    model_cls : Type[Model]
        The model class to batch-run
    run: Tuple[int, int, Dict[str, Any]]
        The run id, iteration number, and kwargs for this run
    max_steps : int
        Maximum number of model steps after which the model halts, by default 1000
    data_collection_period : int
        Number of steps after which data gets collected

    Returns
    -------
    List[Dict[str, Any]]
        Return model_data, agent_data from the reporters
    """
    run_id, iteration, kwargs = run
    model = model_cls(**kwargs)
    while model.running and model.schedule.steps <= max_steps:
        model.step()

    data = []

    steps = list(range(offset, model.schedule.steps, data_collection_period))
    if not steps or steps[-1] != model.schedule.steps - 1:
        steps.append(model.schedule.steps - 1)

    for step in steps:
        model_data, all_agents_data = _collect_data(model, step, offset)  # Pass the offset parameter here

        # If there are agent_reporters, then create an entry for each agent
        if all_agents_data:
            stepdata = [
                {
                    "RunId": run_id,
                    "iteration": iteration,
                    "Step": step,
                    **kwargs,
                    **model_data,
                    **agent_data,
                }
                for agent_data in all_agents_data
            ]
        # If there is only model data, then create a single entry for the step
        else:
            stepdata = [
                {
                    "RunId": run_id,
                    "iteration": iteration,
                    "Step": step,
                    **kwargs,
                    **model_data,
                }
            ]
        data.extend(stepdata)

    return data

def _collect_data(
    model: Model,
    step: int,
    offset: int,  # Add the offset parameter here
) -> Tuple[Dict[str, Any], List[Dict[str, Any]]]:
    """Collect model and agent data from a model using mesas datacollector."""
    dc = model.datacollector

    adjusted_step = step + offset  # Add the offset to the step here

    model_data = {param: values[adjusted_step] for param, values in dc.model_vars.items()}

    all_agents_data = []
    raw_agent_data = dc._agent_records.get(adjusted_step, [])  # Use the adjusted_step here
    for data in raw_agent_data:
        agent_dict = {"AgentID": data[1]}
        agent_dict.update(zip(dc.agent_reporters, data[2:]))
        all_agents_data.append(agent_dict)
    return model_data, all_agents_data


class ParameterError(TypeError):
    MESSAGE = (
        "Parameters must map a name to a value. "
        "These names did not match parameters: {}"
    )

    def __init__(self, bad_names):
        self.bad_names = bad_names

    def __str__(self):
        return self.MESSAGE.format(self.bad_names)



class VariableParameterError(ParameterError):
    MESSAGE = (
        "Variable_parameters must map a name to a sequence of values. "
        "These parameters were given with non-sequence values: {}"
    )
