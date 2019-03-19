import pandas as pd

test_input_data = "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
test_input_data = pd.read_csv(test_input_data, dtype=str)

# Test grouping power by clean, Standard_Version and time
test_input_data_power_as_number = test_input_data
test_input_data_power_as_number['power_kW'] = pd.to_numeric(test_input_data_power_as_number['power_kW'])
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'Standard_Version'], as_index=False)['power_kW'].sum()
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time", "power_kW": "Power_kW"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_1.csv", index=False)

# Test grouping power by clean, Standard_Version, manufacturer and time
test_input_data_power_as_number = test_input_data
test_input_data_power_as_number['power_kW'] = pd.to_numeric(test_input_data_power_as_number['power_kW'])
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'Standard_Version', 'manufacturer'],
                                                     as_index=False)['power_kW'].sum()
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version'] + '-' + test_out_1['manufacturer']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time", "power_kW": "Power_kW"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_2.csv", index=False)

# Test grouping power by clean, site_id, c_id and time
test_input_data_power_as_number = test_input_data
test_input_data_power_as_number['power_kW'] = pd.to_numeric(test_input_data_power_as_number['power_kW'])
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'site_id', 'c_id'],
                                                     as_index=False)['power_kW'].sum()
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['site_id'] + '-' + test_out_1['c_id']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time", "power_kW": "Power_kW"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_3.csv", index=False)

# Test grouping power by clean and time
test_input_data_power_as_number = test_input_data
test_input_data_power_as_number['power_kW'] = pd.to_numeric(test_input_data_power_as_number['power_kW'])
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean'], as_index=False)['power_kW'].sum()
test_out_1['series'] = test_out_1['clean']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time", "power_kW": "Power_kW"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_4.csv", index=False)

# Test grouping F and V by clean, Standard_Version and time
test_input_data_power_as_number = test_input_data
test_input_data_power_as_number['f'] = pd.to_numeric(test_input_data_power_as_number['f'])
test_input_data_power_as_number['v'] = pd.to_numeric(test_input_data_power_as_number['v'])
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'Standard_Version'], as_index=False).agg(
    {'f': 'mean', 'v': 'mean'})
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time", "f": "Frequency", "v": "Voltage"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_5.csv", index=False)

# Test grouping F and V by clean, Standard_Version, manufacturer and time
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'Standard_Version', 'manufacturer'],
                                                     as_index=False).agg({'f': 'mean', 'v': 'mean'})
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version'] + '-' + test_out_1['manufacturer']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time", "f": "Frequency", "v": "Voltage"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_6.csv", index=False)

# Test grouping F and V by clean, site_id, c_id and time
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'site_id', 'c_id'],
                                                     as_index=False).agg({'f': 'mean', 'v': 'mean'})
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['site_id'] + '-' + test_out_1['c_id']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time", "f": "Frequency", "v": "Voltage"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_7.csv", index=False)

# Test grouping F and V by clean and time
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean'], as_index=False).agg({'f': 'mean', 'v': 'mean'})
test_out_1['series'] = test_out_1['clean']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time", "f": "Frequency", "v": "Voltage"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_8.csv", index=False)

# Test grouping site_performance_factor by clean, Standard_Version and time
test_input_data_power_as_number = test_input_data
test_input_data_power_as_number['site_performance_factor'] = pd.to_numeric(test_input_data_power_as_number['site_performance_factor'])
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'Standard_Version'], as_index=False).agg(
    {'site_performance_factor': 'mean'})
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_9.csv", index=False)

# Test grouping site_performance_factor by clean, Standard_Version, manufacturer and time
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'Standard_Version', 'manufacturer'],
                                                     as_index=False).agg({'site_performance_factor': 'mean'})
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version'] + '-' + test_out_1['manufacturer']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_10.csv", index=False)

# Test grouping site_performance_factor by clean, site_id, c_id and time
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean', 'site_id'],
                                                     as_index=False).agg({'site_performance_factor': 'mean'})
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['site_id']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_11.csv", index=False)

# Test grouping site_performance_factor by clean and time
test_out_1 = test_input_data_power_as_number.groupby(['ts', 'clean'], as_index=False).agg({'site_performance_factor': 'mean'})
test_out_1['series'] = test_out_1['clean']
test_out_1 = test_out_1.rename(index=str, columns={"ts": "Time"})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_12.csv", index=False)

# Test grouping count by clean, Standard_Version and time
test_input_data_power_as_number = test_input_data
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'Standard_Version'], as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_13.csv", index=False)

# Test grouping count by clean, Standard_Version, manufacturer and time
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'Standard_Version', 'manufacturer'],
                                                     as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_14.csv", index=False)

# Test grouping count by clean, site_id, c_id and time
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'site_id'],
                                                     as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_15.csv", index=False)

# Test grouping count by clean and time
test_out_1 = test_input_data_power_as_number.groupby(['clean'], as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_16.csv", index=False)