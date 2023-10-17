import sys
sys.path.append("/users/me00286/Script/Testing")
import os
os.chdir("/users/me00286/Script/Testing")
import numpy as np
import pandas as pd
import itertools
from Batchrun_Rewrite import batch_run
from ICE_V7 import *
import gc

np.random.seed()
years = 40 + 100
steps = (24 * years) - 10
int_dog = [0.65, 0.9]
int_sheep = [0.8, 0.9]
inter = [list(combination) for combination in itertools.product(int_dog, int_sheep)]

run_counter = 0
merged_list = []
collected_data = []

for i in range(80):
    param_set = []
    for j in range(32):
        sheep_mean = np.random.uniform(low=0.00001, high=0.001)
        sheep_var = np.random.uniform(low=0.3, high=0.75)
        dog_mean = np.random.uniform(low=0.03, high=1)
        dog_var = np.random.uniform(low=0.02, high=0.1)
        seed = np.random.randint(low=0, high=100000)
        for k in range(len(inter)):
            run_counter += 1
            param_dict = {'sheep_consumption_rate': sheep_mean,
                          'dog_consumption_rate': dog_mean,
                          'dog_het': dog_var,
                          'sheep_het': sheep_var,
                          'steps': steps,
                          'sheep_vaccine_cov' : 0.8,
                          'sheep_vaccine_cov_alt' : inter[k][1],
                          'dog_deworming_cov' : 0.65,
                          'dog_deworming_cov_alt' : inter[k][0],
                          'dog_deworming_annualfreq' : 4,
                          'intervention': True,
                          'scaling': 1,
                          'seed': seed,
                          'dew_burning_period' : 40,
                          'vacc_burning_period' : 69,
                          'alt_burning_period' : 83}
            param_set.append(param_dict)
            
    param_run = batch_run(CEModel, parameters_list=param_set, max_steps=steps, number_processes=None, data_collection_period=24, offset = -10)
            
    results_df = pd.DataFrame(param_run)
    results_df["State"].replace({1: 0, 2: 1}, inplace=True)
    results_df["RunId"] = results_df["RunId"] + (i * 32 * len(inter))  # Use 32 instead of (j+1)
    
    collected_data.append(results_df)  # Append data to collected_data list
    
        
    dt_dt = results_df[results_df['Species'] == "dog"].groupby(['RunId', 'Step', 'dog_consumption_rate', 'sheep_consumption_rate', 'dog_het', 'sheep_het', 'sheep_vaccine_cov_alt', 'dog_deworming_cov_alt', 'dog_deworming_annualfreq', 'intervention', 'seed', 'Vaccine Dosage Count', 'Deworm Dosage Count']).agg({"State": "mean"}).reset_index().rename(columns={'State': 'dog_prev'})
        
    dt_st = results_df[(results_df['Species'] == "sheep") & (results_df['Age'] >= 1)].groupby(['RunId', 'Step', 'dog_consumption_rate', 'sheep_consumption_rate', 'dog_het', 'sheep_het', 'sheep_vaccine_cov_alt', 'dog_deworming_cov_alt', 'dog_deworming_annualfreq', 'intervention', 'seed', 'Vaccine Dosage Count', 'Deworm Dosage Count']).agg({"State": "mean"}).reset_index().rename(columns={'State': 'sheep_prev'})

    merged_df = pd.merge(dt_dt, dt_st, on=['RunId', 'Step', 'dog_consumption_rate', 'sheep_consumption_rate', 'dog_het', 'sheep_het', 'sheep_vaccine_cov_alt','dog_deworming_cov_alt', 'dog_deworming_annualfreq', 'intervention', 'seed', 'Vaccine Dosage Count', 'Deworm Dosage Count'])
    
    merged_list.append(merged_df)  # Append data to merged_list
    
    del dt_dt, dt_st, param_run, results_df
    gc.collect()
    
    final_df = pd.concat(merged_list)
    final_df.to_csv("intervention_int.csv", index=False)