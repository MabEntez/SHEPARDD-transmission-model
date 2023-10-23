# Import necessary libraries
import sys
import os
import numpy as np
import pandas as pd
import itertools
from Batchrun_Rewrite import batch_run
from ICE_V7 import *
import gc

# Add the specified path to the system path to be able to import Python modules from it
sys.path.append("/users/me00286/Script/Testing")

# Change the current working directory
os.chdir("/users/me00286/Script/Testing")

# Set a random seed for reproducibility
np.random.seed()

# Define some constants and parameters
years = 40 + 100
steps = (24 * years) - 10
int_dog = [0.65, 0.9]
int_sheep = [0.8, 0.9]

# Create combinations of the interaction parameters
inter = [list(combination) for combination in itertools.product(int_dog, int_sheep)]

# Initialize counters and empty lists to collect data
run_counter = 0
merged_list = []
collected_data = []

# Start simulations
for i in range(80):
    param_set = []
    for j in range(32):
        
        # Generate random parameters for each run
        sheep_mean = np.random.uniform(low=0.00001, high=0.001)
        sheep_var = np.random.uniform(low=0.3, high=0.75)
        dog_mean = np.random.uniform(low=0.03, high=1)
        dog_var = np.random.uniform(low=0.02, high=0.1)
        seed = np.random.randint(low=0, high=100000)
        
        for k in range(len(inter)):
            run_counter += 1
            
            # Define the parameter set for the batch_run
            param_dict = {'sheep_consumption_rate': sheep_mean,
                          'dog_consumption_rate': dog_mean,
                          'dog_het': dog_var,
                          'sheep_het': sheep_var,
                          'steps': steps,
                          'sheep_vaccine_cov': 0.8,
                          'sheep_vaccine_cov_alt': inter[k][1],
                          'dog_deworming_cov': 0.65,
                          'dog_deworming_cov_alt': inter[k][0],
                          'dog_deworming_annualfreq': 4,
                          'intervention': True,
                          'scaling': 1,
                          'seed': seed,
                          'dew_burning_period': 40,
                          'vacc_burning_period': 69,
                          'alt_burning_period': 83}
            
            # Append the parameter dictionary to the list of parameters
            param_set.append(param_dict)
            
    # Run the batch run with the defined parameters
    param_run = batch_run(CEModel, parameters_list=param_set, max_steps=steps, number_processes=None, data_collection_period=24, offset=-10)
    
    # Process results
    results_df = pd.DataFrame(param_run)
    
    # Replace values in "State" column
    results_df["State"].replace({1: 0, 2: 1}, inplace=True)
    results_df["RunId"] = results_df["RunId"] + (i * 32 * len(inter))
    
    # Store results in the list
    collected_data.append(results_df)
    
    # Filter and group data for dogs
    dt_dt = results_df[results_df['Species'] == "dog"].groupby(['RunId', 'Step', 'dog_consumption_rate', 'sheep_consumption_rate', 'dog_het', 'sheep_het', 'sheep_vaccine_cov_alt', 'dog_deworming_cov_alt', 'dog_deworming_annualfreq', 'intervention', 'seed', 'Vaccine Dosage Count', 'Deworm Dosage Count']).agg({"State": "mean"}).reset_index().rename(columns={'State': 'dog_prev'})
        
    # Filter and group data for sheep
    dt_st = results_df[(results_df['Species'] == "sheep") & (results_df['Age'] >= 1)].groupby(['RunId', 'Step', 'dog_consumption_rate', 'sheep_consumption_rate', 'dog_het', 'sheep_het', 'sheep_vaccine_cov_alt', 'dog_deworming_cov_alt', 'dog_deworming_annualfreq', 'intervention', 'seed', 'Vaccine Dosage Count', 'Deworm Dosage Count']).agg({"State": "mean"}).reset_index().rename(columns={'State': 'sheep_prev'})

    # Merge the two dataframes on common columns
    merged_df = pd.merge(dt_dt, dt_st, on=['RunId', 'Step', 'dog_consumption_rate', 'sheep_consumption_rate', 'dog_het', 'sheep_het', 'sheep_vaccine_cov_alt','dog_deworming_cov_alt', 'dog_deworming_annualfreq', 'intervention', 'seed', 'Vaccine Dosage Count', 'Deworm Dosage Count'])
    
    # Store the merged dataframe in the list
    merged_list.append(merged_df)
    
    # Delete temporary dataframes to free memory
    del dt_dt, dt_st, param_run, results_df
    
    # Run garbage collector to further free up memory
    gc.collect()
    
# Concatenate all the dataframes stored in the list
final_df = pd.concat(merged_list)

# Save the final dataframe to a CSV file
final_df.to_csv("intervention_int.csv", index=False)