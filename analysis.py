# -*- coding: utf-8 -*-

# %%
import pandas as pd
import numpy as np

# %%
cgrt = pd.read_csv('OxCGRT_latest.csv')
covid = pd.read_csv('owid-covid-data.csv')

cgrt.Date = pd.to_datetime(cgrt.Date, format='%Y%m%d')
covid.date = pd.to_datetime(covid.date, format='%Y-%m-%d')

# %%
countries_with_regions = cgrt.loc[cgrt['RegionName'].notnull()].CountryName.unique()
cgrt_nr = cgrt.loc[~cgrt['CountryName'].isin(countries_with_regions)]

# join StringencyIndex with total deaths
l1 = cgrt_nr[['CountryCode', 'CountryName', 'Date', 'StringencyIndex']]
l2 = covid[['continent', 'iso_code', 'date', 'total_deaths_per_million']]

l1l2 = pd.merge(l1, l2, how='inner', left_on=['CountryCode','Date'], right_on=['iso_code', 'date'])

# %% average_si_vs_dpm 

average_si_vs_dpm = l1l2.groupby('CountryCode').agg(
    country_name=('CountryName','first'), 
    mean_SI=('StringencyIndex', 'mean'), 
    tdpm=('total_deaths_per_million', 'max')
).dropna()

# %%
stringency_labels = ["{0} - {1}".format(i, i + 9) for i in range(0, 100, 10)]
average_si_vs_dpm.loc[:, 'stringency_group'] = pd.cut(average_si_vs_dpm.mean_SI, range(0, 105, 10), right=False, labels=stringency_labels)

tdpm_labels = ["{0} - {1}".format(i, i + 499) for i in range(0, 2000, 500)]
average_si_vs_dpm['tdpm_group'] = pd.cut(average_si_vs_dpm.tdpm, range(0, 2000+1, 500), right=False, labels=tdpm_labels)

# %%
from collections import OrderedDict
data = {(si_label, tdpm_label): 0 for si_label in stringency_labels for tdpm_label in tdpm_labels}
for index, row in average_si_vs_dpm.iterrows():
    si_label = row['stringency_group']
    tdpm_label = row['tdpm_group']    
    key = (si_label, tdpm_label)        
    data[key] = data.get(key, 0) + 1

# %%    
from statsmodels.graphics.mosaicplot import mosaic

color_map = {
    '0 - 499':'whitesmoke',
    '500 - 999': 'lightgray',
    '1000 - 1499': 'darkgray',
    '1500 - 1999': 'gray'
}

def color(key):
    _, tdpm = key
    return color_map.get(tdpm, 'red')
    
props = lambda key: {'color': color(key)}

mosaic(data,  properties=props, labelizer = lambda key: '')


# %%

# data = np.zeros((len(stringency_labels), len(tdpm_labels)))

# for index, row in average_si_vs_dpm.iterrows():
#     si_label = row['stringency_group']
#     tdpm_label = row['tdpm_group']
#     si_label_idx = stringency_labels.index(si_label)
#     tdpm_label_idx = tdpm_labels.index(tdpm_label)
    
#     data[si_label_idx, tdpm_label_idx] = data[si_label_idx, tdpm_label_idx] + 1
    
# # normalize data row wise to sum to 1
# total_per_si_group = np.sum(data, axis = 1)

# # %%
# data = data[total_per_si_group > 0,:]
# stringency_labels = [value for idx, value in enumerate(stringency_labels) if total_per_si_group[idx] > 0]
# total_per_si_group = total_per_si_group[total_per_si_group > 0]
# data = data / total_per_si_group.reshape(-1,1)
