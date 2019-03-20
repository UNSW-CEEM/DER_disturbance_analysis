import pandas as pd
import numpy as np
import csv

test_input_data = "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
test_input_data = pd.read_csv(test_input_data, dtype=str)
test_input_data['site_performance_factor'] = pd.to_numeric(test_input_data['power_kW'])
test_input_data['sum_ac'] = pd.to_numeric(test_input_data['sum_ac'])
test_input_1 = test_input_data.groupby(['clean', 'site_id', 'ts'], as_index=False)['site_performance_factor'].sum()
test_input_1 = pd.merge(test_input_data, test_input_1, 'left', on=['clean', 'site_id', 'ts'])
test_input_data['site_performance_factor'] = test_input_data['site_performance_factor'] / test_input_data['sum_ac']
test_input_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_33.csv", index=False)
