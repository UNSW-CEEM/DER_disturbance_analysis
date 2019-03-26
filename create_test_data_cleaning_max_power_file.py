import pandas as pd

test_input_data = "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_cleaning_data.csv"
test_input_data = pd.read_csv(test_input_data, dtype=str)
test_input_data['max_power_kW'] = pd.to_numeric(test_input_data['power_kW'])
test_input_data = test_input_data.groupby(['ts', 'site_id'], as_index=False).agg({'max_power_kW': 'sum'})
test_input_data = test_input_data.groupby(['site_id'], as_index=False).agg({'max_power_kW': 'max'})
test_input_data.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_data_max_site_power.csv", index=False)
