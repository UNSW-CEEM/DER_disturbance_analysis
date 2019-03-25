import pandas as pd
import numpy as np
import csv

test_input_data = "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_cleaning_data.csv"
test_input_data = pd.read_csv(test_input_data, dtype=str)
test_input_data['sunrise'] = '2018-08-25 06:00:00'
test_input_data['sunset'] = '2018-08-25 18:00:00'
test_input_data['sunrise'] = pd.to_datetime(test_input_data['sunrise'])
test_input_data['sunset'] = pd.to_datetime(test_input_data['sunset'])
test_input_data['ts'] = pd.to_datetime(test_input_data['ts'])
test_input_data['e'] = pd.to_numeric(test_input_data['e'])
test_input_data['e'] = test_input_data['e'].abs()
test_input_data['min_power'] = pd.to_numeric(test_input_data['power_kW'])
test_input_data['max_power'] = pd.to_numeric(test_input_data['power_kW'])
test_input_data['e'] = test_input_data['e'] / 3.6e+6
test_input_data['daylight'] = np.where((test_input_data['ts'] > test_input_data['sunrise']) &
                                       (test_input_data['ts'] < test_input_data['sunset']), 1, 0)
test_input_data['night'] = np.where((test_input_data['ts'] > test_input_data['sunrise']) &
                                       (test_input_data['ts'] < test_input_data['sunset']), 0, 1)
test_input_data['energy_day'] = test_input_data['e'] * test_input_data['daylight']
test_input_data['energy_night'] = test_input_data['e'] * test_input_data['night']
test_input_data = test_input_data.groupby(['c_id'], as_index=False).agg({
 'energy_day': 'sum', 'energy_night': 'sum', 'con_type': 'first', 'sunrise': 'first', 'sunset': 'first',
 'first_ac': 'first', 'min_power': 'min', 'max_power': 'max', 'polarity': 'first'})
test_input_data = test_input_data.round({'energy_day': 2, 'energy_night': 2})
test_input_data['frac_day'] = test_input_data['energy_day'] / (test_input_data['energy_day'] + test_input_data['energy_night'])
test_input_data = test_input_data.round({'frac_day': 2})
test_input_data['frac_day'] = np.where((test_input_data['energy_day'] < 0.001) & (test_input_data['energy_night'] < 0.001), "NaN", test_input_data['frac_day'])
test_input_data.to_csv("C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_c_id_cleaning.csv",
                    index=False)
