# -*- coding: utf-8 -*-

# %%
import pandas as pd
from numpy import *
from matplotlib.pyplot import *
from statsmodels.graphics.mosaicplot import mosaic
from funcs import *

# %% "CORONAVIRUS GOVERNMENT RESPONSE TRACKER" dataset

lockdowns = pd.read_csv('OxCGRT_latest.csv')
print(lockdowns.head())

# %% Fix date
lockdowns.Date = pd.to_datetime(lockdowns.Date, format='%Y%m%d')
print(lockdowns.Date.head())

# %% Which countries have regions?

countries_with_regions = lockdowns[lockdowns['RegionName'].notnull()].CountryName.unique()
print(countries_with_regions)

# %% Are there any gaps between dates

countries = lockdowns.CountryName.unique()
countries = setdiff1d(countries, countries_with_regions)

for country in countries:
    dt = lockdowns[lockdowns.CountryName == country].Date
    days = (dt.values[1:] - dt.values[:-1]).astype('timedelta64[D]')
    if any(days != timedelta64(1, 'D')):
        print(country)
        
# All good.

# %% Covid deaths

covid = pd.read_csv('owid-covid-data.csv')
covid.date = pd.to_datetime(covid.date, format='%Y-%m-%d')
print(covid.columns)

# so the data is not split by region in case of United States, United Kingdom and etc..
# for initial experimens countries with regions could be ignored and later included separately.

# %% Joining covid deaths with government response data
# Plan:
#     * Avg(StringencyIndex) ~ n_deaths_per_million
#     * Avg(StringencyIndex) ~ n_cases_per_million
#     * Go measure by measure and check how those correlate
#         - Avg(I_{j,t}) ~ n_deaths_per_million

cgrt_nr = lockdowns[~lockdowns.CountryName.str.contains("|".join(countries_with_regions))]

l1 = cgrt_nr[['CountryCode', 'CountryName', 'Date','StringencyIndex']]
l2 = covid[['continent','iso_code', 'date', 'total_deaths_per_million']]
l1l2 = pd.merge(l1, l2, how='inner', left_on=['CountryCode','Date'], right_on=['iso_code', 'date'])
stringency_vs_death = l1l2.groupby('CountryCode').agg(
    country_name=('CountryName','first'), 
    mean_stringency_index=('StringencyIndex', 'mean'), 
    total_deaths_per_million=('total_deaths_per_million', 'max')
)
stringency_vs_death = stringency_vs_death.dropna(how='all')

# %% Check for correlation between mean(StringencyIndex) ~ total_deaths_per_million
figure()
scatter(stringency_vs_death.mean_stringency_index, stringency_vs_death.total_deaths_per_million)
ylabel('total deaths per million')
xlabel('mean(StringencyIndex)')
title('mean(StringencyIndex) ~ total_deaths_per_million')

# from the plot it doesn't look like there is much correlation.
# This is a bit stupid, but I wonder if correlation is only a measure between two **normal** distributions.
# Pearson's R requires that both be normal.
figure()
hist(stringency_vs_death.mean_stringency_index)
title('mean_stringency_index')

figure()
hist(stringency_vs_death.total_deaths_per_million)
title('total_deaths_per_million')

# %% Check the same stats for EU
# total_deaths_per_million are not normally distributed.
eu_stringency_vs_death = l1l2[l1l2.continent == 'Europe'].groupby('CountryCode').agg(
    country_name=('CountryName','first'), 
    mean_stringency_index=('StringencyIndex', 'mean'), 
    total_deaths_per_million=('total_deaths_per_million', 'max')
)

# %%
figure()
scatter(eu_stringency_vs_death.mean_stringency_index, eu_stringency_vs_death.total_deaths_per_million)
ylabel('total deaths per million')
xlabel('mean(StringencyIndex)')
title('mean(StringencyIndex) ~ total_deaths_per_million (Europe)')

figure()
hist(eu_stringency_vs_death.mean_stringency_index)
title('mean_stringency_index (Europe)')

figure()
hist(eu_stringency_vs_death.total_deaths_per_million)
title('total_deaths_per_million (Europe)')

# mean_stringency_index (apart of one outlier with value = 20) seems skewed to the right
# total_deahts_per_million has three peaks

# actually, neither mean_stringency_index, nor total_deaths_per_million are continuous variables, they are categorical variables.
# graphical method to check for dependence is with moscaic plot (https://en.wikipedia.org/wiki/Bivariate_analysis), although I am not sure how one would interpret it and what assumptions about the variables should be required?

# %%

from collections import OrderedDict
    
title = 'Avg(StringencyIndex) ~ DPM (Europe)'
color_map = OrderedDict({
    '0 - 499':'whitesmoke',
    '500 - 999': 'lightgray',
    '1000 - 1499': 'darkgray',
    '1500 - 1999': 'gray'
})
plot_avg_index_vs_dpm(eu_stringency_vs_death, color_map, title)

# %%

plot_avg_index_vs_dpm(stringency_vs_death, color_map, title='Avg(StringencyIndex) ~ DPM')
