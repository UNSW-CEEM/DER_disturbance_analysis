import pandas as pd
import numpy as np
import csv

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

# Test grouping count response by clean, Standard_Version and time
test_input_data_power_as_number = test_input_data
test_input_data_power_as_number['response_category'] = \
    np.where(test_input_data_power_as_number['response_category'].isnull(), 'NA',
             test_input_data_power_as_number['response_category'])
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'Standard_Version', 'response_category'], as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1['series_x'] = test_out_1['response_category'] + '-' + test_out_1['clean']
test_out_1['series_y'] = test_out_1['Standard_Version']
test_out_1['sample_count'] = test_out_1['sample_count']/test_out_1['sample_count'].sum()
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_17.csv", index=False)

# Test grouping count response by clean, Standard_Version, manufacturer
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'Standard_Version', 'manufacturer', 'response_category'],
                                                     as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1['series_x'] = test_out_1['response_category'] + '-' + test_out_1['clean']
test_out_1['series_y'] = test_out_1['Standard_Version'] + '-' + test_out_1['manufacturer']
test_out_1['sample_count'] = test_out_1['sample_count']/test_out_1['sample_count'].sum()
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_18.csv", index=False)

# Test grouping count response by clean, site_id, c_id
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'site_id', 'response_category'],
                                                     as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1['series_x'] = test_out_1['response_category'] + '-' + test_out_1['clean']
test_out_1['series_y'] = test_out_1['site_id'].astype(str)
test_out_1['sample_count'] = test_out_1['sample_count']/test_out_1['sample_count'].sum()
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_19.csv", index=False,
                  quoting=csv.QUOTE_ALL)

# Test grouping count response by clean
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'response_category'], as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1['series_x'] = test_out_1['response_category'] + '-' + test_out_1['clean']
test_out_1['series_y'] = 'All'
test_out_1['sample_count'] = test_out_1['sample_count']/test_out_1['sample_count'].sum()
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_20.csv", index=False)

# Test grouping count zone by clean, Standard_Version and time
test_input_data_power_as_number = test_input_data
test_input_data_power_as_number['zone'] = \
    np.where(test_input_data_power_as_number['zone'].isnull(), 'NA',
             test_input_data_power_as_number['zone'])
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'Standard_Version', 'zone'], as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1['series_x'] = test_out_1['clean'] + '-' + test_out_1['zone']
test_out_1['series_y'] = test_out_1['Standard_Version']
test_out_1['sample_count'] = test_out_1['sample_count']/test_out_1['sample_count'].sum()
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_21.csv", index=False)

# Test grouping count response by clean, Standard_Version, manufacturer
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'Standard_Version', 'manufacturer', 'zone'],
                                                     as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1['series_x'] = test_out_1['clean'] + '-' + test_out_1['zone']
test_out_1['series_y'] = test_out_1['Standard_Version'] + '-' + test_out_1['manufacturer']
test_out_1['sample_count'] = test_out_1['sample_count']/test_out_1['sample_count'].sum()
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_22.csv", index=False)

# Test grouping count response by clean, site_id, c_id
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'site_id', 'zone'],
                                                     as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1['series_x'] = test_out_1['clean'] + '-' + test_out_1['zone']
test_out_1['series_y'] = test_out_1['site_id'].astype(str)
test_out_1['sample_count'] = test_out_1['sample_count']/test_out_1['sample_count'].sum()
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_23.csv", index=False,
                  quoting=csv.QUOTE_ALL)

# Test grouping count response by clean
test_out_1 = test_input_data_power_as_number.groupby(['clean', 'zone'], as_index=False)['c_id'].agg({'c_id': pd.Series.nunique})
test_out_1 = test_out_1.rename(index=str, columns={'c_id': 'sample_count'})
test_out_1['series_x'] = test_out_1['clean'] + '-' + test_out_1['zone']
test_out_1['series_y'] = 'All'
test_out_1['sample_count'] = test_out_1['sample_count']/test_out_1['sample_count'].sum()
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_24.csv", index=False)

# Test grouping distance response by clean, Standard_Version
test_input_temp = test_input_data
test_input_temp['distance'] = pd.to_numeric(test_input_temp['distance'])
test_input_temp = test_input_temp.groupby(['clean', 'site_id', 'c_id'], as_index=False).first()
test_input_temp['response_category'] = np.where(test_input_temp['response_category'].isnull(), 'NA', test_input_temp['response_category'])
test_input_temp['num_disconnects'] = np.where(test_input_temp['response_category'].isin(["4 Disconnect", "3 Drop to Zero"]), 1, 0)
test_input_temp['system_count'] = 1
test_out_1 = test_input_temp.groupby(['clean', 'Standard_Version', 's_postcode'], as_index=False).agg(
    {'distance': 'first',  'num_disconnects': 'sum', "system_count": 'sum'})
test_out_1 = test_out_1.sort_values('distance', ascending=True)
test_out_1['num_disconnects'] = test_out_1.groupby(['clean', 'Standard_Version'], as_index=False)['num_disconnects'].cumsum()
test_out_1['system_count'] = test_out_1.groupby(['clean', 'Standard_Version'], as_index=False)['system_count'].cumsum()
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version']
test_out_1['percentage'] = test_out_1['num_disconnects'] / test_out_1['system_count']
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_25.csv", index=False)

# Test grouping distance response by clean, Standard_Version, manufacturer
test_out_1 = test_input_temp.groupby(['clean', 'Standard_Version', 'manufacturer', 's_postcode'], as_index=False).agg(
    {'distance': 'first',  'num_disconnects': 'sum', "system_count": 'sum'})
test_out_1 = test_out_1.sort_values('distance', ascending=True)
test_out_1['num_disconnects'] = test_out_1.groupby(['clean', 'Standard_Version', 'manufacturer'], as_index=False)['num_disconnects'].cumsum()
test_out_1['system_count'] = test_out_1.groupby(['clean', 'Standard_Version', 'manufacturer'], as_index=False)['system_count'].cumsum()
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version'] + '-' + test_out_1['manufacturer']
test_out_1['percentage'] = test_out_1['num_disconnects'] / test_out_1['system_count']
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_26.csv", index=False)

# Test grouping distance  response by clean, site_id, c_id
test_out_1 = test_input_temp.groupby(['clean', 's_postcode'], as_index=False).agg({'distance': 'first',  'num_disconnects': 'sum', "system_count": 'sum'})
test_out_1 = test_out_1.sort_values('distance', ascending=True)
test_out_1['num_disconnects'] = test_out_1.groupby(['clean'], as_index=False)['num_disconnects'].cumsum()
test_out_1['system_count'] = test_out_1.groupby(['clean'], as_index=False)['system_count'].cumsum()
test_out_1['series'] = test_out_1['clean']
test_out_1['percentage'] = test_out_1['num_disconnects'] / test_out_1['system_count']
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_27.csv", index=False)

# Test grouping distance response by clean
test_out_1 = test_input_temp.groupby(['clean', 's_postcode'], as_index=False).agg({'distance': 'first',  'num_disconnects': 'sum', "system_count": 'sum'})
test_out_1 = test_out_1.sort_values('distance', ascending=True)
test_out_1['num_disconnects'] = test_out_1.groupby(['clean'], as_index=False)['num_disconnects'].cumsum()
test_out_1['system_count'] = test_out_1.groupby(['clean'], as_index=False)['system_count'].cumsum()
test_out_1['series'] = test_out_1['clean']
test_out_1['percentage'] = test_out_1['num_disconnects'] / test_out_1['system_count']
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_28.csv", index=False)

# Test grouping distance response by clean, Standard_Version
test_input_temp = test_input_data
test_out_1 = test_input_temp.groupby(['clean', 'site_id'], as_index=False).first()
test_out_1 = test_out_1.loc[:, ('clean', 'site_id', 'Standard_Version', 's_postcode', 'Grouping', 'manufacturer',
                                'model', 'response_category', 'zone', 'sum_ac', 'lat', 'lon')]
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version']
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_29.csv", index=False)

# Test grouping distance response by clean, Standard_Version, manufacturer
test_input_temp = test_input_data
test_out_1 = test_input_temp.groupby(['clean', 'site_id'], as_index=False).first()
test_out_1 = test_out_1.loc[:, ('clean', 'site_id', 'Standard_Version', 's_postcode', 'Grouping', 'manufacturer',
                                'model', 'response_category', 'zone', 'sum_ac', 'lat', 'lon')]
test_out_1['series'] = test_out_1['clean'] + '-' + test_out_1['Standard_Version'] + '-' + test_out_1['manufacturer']
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_30.csv", index=False)

# Test grouping distance  response by clean, site_id, c_id
test_input_temp = test_input_data
test_out_1 = test_input_temp.groupby(['clean', 'site_id'], as_index=False).first()
test_out_1 = test_out_1.loc[:, ('clean', 'site_id', 'Standard_Version', 's_postcode', 'Grouping', 'manufacturer',
                                'model', 'response_category', 'zone', 'sum_ac', 'lat', 'lon')]
test_out_1['series'] = test_out_1['clean']
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_31.csv", index=False)

# Test grouping distance response by clean
test_input_temp = test_input_data
test_out_1 = test_input_temp.groupby(['clean', 'site_id'], as_index=False).first()
test_out_1 = test_out_1.loc[:, ('clean', 'site_id', 'Standard_Version', 's_postcode', 'Grouping', 'manufacturer',
                                'model', 'response_category', 'zone', 'sum_ac', 'lat', 'lon')]
test_out_1['series'] = test_out_1['clean']
test_out_1.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_32.csv", index=False)
