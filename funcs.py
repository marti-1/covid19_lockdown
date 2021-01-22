# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import math
import numpy as np
import pandas as pd
from statsmodels.graphics.mosaicplot import mosaic

def plot_avg_index_vs_dpm(df, color_map, title=None):

    def roundup(x):
        return int(math.ceil(x / 1000.0)) * 1000
    
    tdpm_max = df.total_deaths_per_million.max()
    tdpm_max = roundup(tdpm_max)
    
    stringency_labels = ["{0} - {1}".format(i, i + 9) for i in range(0, 100, 10)]
    df['stringency_group'] = pd.cut(df.mean_stringency_index, range(0, 105, 10), right=False, labels=stringency_labels)
    
    tdpm_labels = ["{0} - {1}".format(i, i + 499) for i in range(0, tdpm_max, 500)]
    df['tdpm_group'] = pd.cut(df.total_deaths_per_million, range(0, tdpm_max+1, 500), right=False, labels=tdpm_labels)
    
    def color(key):
        _, tdpm = key
        return color_map.get(tdpm, 'red')
        
    props = lambda key: {'color': color(key)}
    
    foo = df.groupby(['stringency_group','tdpm_group']).size().reset_index(name='counts')
    baz = foo.groupby('stringency_group').sum('counts')
    baz = baz[baz.counts > 0].index
    foobaz = foo.set_index(['stringency_group', 'tdpm_group']).loc[baz]
    mosaic(foobaz.to_dict()['counts'], properties=props, labelizer = lambda key: '')
    
    from matplotlib.lines import Line2D
    
    custom_lines = [Line2D([0], [0], color=value) for key, value in color_map.items()]
    plt.legend(custom_lines, tdpm_labels, loc='lower right')
    if title is not None:
        plt.title(title)