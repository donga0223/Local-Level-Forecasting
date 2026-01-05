# preprocess_and_plot.py
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from datetime import timedelta
import warnings

import loader
from timeseriesutils import featurize

def preprocess_data(dat, state_full_name=None, population_threshold=0):
    dat['week_end'] = pd.to_datetime(dat['week_end'])
    dat['location'] = dat['state'] + '_' + dat['hsa_nci_id'].astype(str)
    dat['population'] = np.where(
        dat['hsa_nci_id'] != 'All', dat['population_hsa'], dat['population_state'])
    
    dat2 = dat[['week_end', 'state', 'hsa_nci_id', 'inc', 'location', 'population']]
    dat2 = dat2[dat2['inc'].notna()]

    # state_full_name이 None이 아닐 때만 필터
    if state_full_name is not None:
        dat2 = dat2[dat2['state'] == state_full_name]

    dat2 = dat2[dat2['population'] >= population_threshold]
    dat2['log_pop'] = np.log(dat2['population'])

    dat2 = dat2.rename(columns={'week_end': 'wk_end_date'}).drop_duplicates()
    dat2 = dat2.drop(columns=['state'], errors='ignore')


    dat_arrange = dat2.assign(
        epiweek=dat2['wk_end_date'].dt.isocalendar().week,
        year=dat2['wk_end_date'].dt.year
    )

    dat_arrange['season_week'] = loader.convert_epiweek_to_season_week(
        dat_arrange['year'].to_numpy(),
        dat_arrange['epiweek'].to_numpy()
    )
    dat_arrange = loader.adjust_year_based_on_target_end_date(dat_arrange)

    dat_arrange = dat_arrange.assign(
        season=lambda x: (
            (x['year'] - ((x['epiweek'] <= 30) & (x['season_week'] >= 1))).astype(str)
            + "/" +
            ((x['year'] - ((x['epiweek'] <= 30) & (x['season_week'] >= 1)) + 1).astype(str).str[-2:])
        )
    )
    return dat_arrange


def transform_incidence(df):
    df['inc_4rt'] = (df['inc'] + 0.01) ** 0.5
    df['inc_4rt_scale_factor'] = df.assign(
        inc_4rt_in_season = lambda x: np.where((x['season_week'] < 10) | (x['season_week'] > 45), np.nan, x['inc_4rt'])
    ).groupby(['location'])['inc_4rt_in_season'].transform(lambda x: x.quantile(0.95))

    df['inc_4rt_cs'] = df['inc_4rt'] / (df['inc_4rt_scale_factor'] + 0.01)
    df['inc_4rt_center_factor'] = df.assign(
        inc_4rt_cs_in_season = lambda x: np.where((x['season_week'] < 10) | (x['season_week'] > 45), np.nan, x['inc_4rt_cs'])
    ).groupby(['location'])['inc_4rt_cs_in_season'].transform(lambda x: x.mean())
    df['inc_4rt_cs'] = df['inc_4rt_cs'] - df['inc_4rt_center_factor']
    return df

def plot_by_location(df):
    locations = df.drop_duplicates('location').sort_values(by='population', ascending=False)['location'].tolist()
    pop_map = df.drop_duplicates('location').set_index('location')['population'].to_dict()
    nhsn_to_plot = df.assign(season_loc = lambda x: x['season'] + '_' + x['location'].astype(str))

    fig, axes = plt.subplots(12, 8, figsize=(25, 50), sharex=True, sharey=True)
    axes = axes.flatten()

    for i, location in enumerate(locations):
        ax = axes[i]
        sns.lineplot(
            data = nhsn_to_plot[nhsn_to_plot['location'] == location],
            x = 'season_week', y = 'inc_4rt_cs',
            units = 'season_loc', hue = 'season_loc',
            estimator = None, ci = None, ax = ax
        )
        population = pop_map.get(location, 'NA')
        ax.set_title(f"{location}\n(pop={population:,})", fontsize=10)
        ax.legend(title="season", fontsize=6)

    for j in range(len(locations), len(axes)):
        fig.delaxes(axes[j])

    fig.tight_layout()
    plt.show()

    

def build_features(df, featurize, ref_date, max_horizon=5):
    df['wk_end_date'] = pd.to_datetime(df['wk_end_date'])
    df = df[df['wk_end_date'] < pd.Timestamp(ref_date)]
    df = df.sort_values(['location','wk_end_date']).reset_index(drop=True)
    feat_names = ['inc_4rt_cs', 'season_week', 'log_pop']

    #location one-hot
    for c in ['location']:
        ohe = pd.get_dummies(df[c], prefix=c)
        df = pd.concat([df, ohe], axis=1)
        feat_names = feat_names + list(ohe.columns)

    df = df.merge(
        loader.get_holidays().query("holiday == 'Christmas Day'").drop(columns=['holiday', 'date']).rename(columns={'season_week': 'xmas_week'}),
        how='left', on='season'
    ).assign(delta_xmas = lambda x: x['season_week'] - x['xmas_week'])
    feat_names += ['delta_xmas']

    # features summarizing data within each combination of source and location
    df, new_feat_names = featurize.featurize_data(
        df, group_columns=['location'],
        features=[
            {'fun': 'windowed_taylor_coefs', 
             'args': {'columns': 'inc_4rt_cs', 'taylor_degree': 2, 'window_align': 'trailing', 'window_size': [4,6], 'fill_edges': False}},
            {'fun': 'windowed_taylor_coefs', 
             'args': {'columns': 'inc_4rt_cs', 'taylor_degree': 1, 'window_align': 'trailing', 'window_size': [3,5], 'fill_edges': False}},
            {'fun': 'rollmean', 
             'args': {'columns': 'inc_4rt_cs', 'group_columns': ['location'], 'window_size': [2,4]}},
        ]
    )
    feat_names += new_feat_names

    df, lag_feat_names = featurize.featurize_data(
        df, group_columns=['location'],
        features=[
            {'fun': 'lag', 'args': {'columns': ['inc_4rt_cs'] + new_feat_names, 'lags': [1, 2]}}
        ]
    )
    feat_names += lag_feat_names
    
    df, horizon_feat_names = featurize.featurize_data(
        df, group_columns=['location'],
        features=[
            {'fun': 'horizon_targets', 
             'args': {'columns': 'inc_4rt_cs', 'horizons': [(i + 1) for i in range(max_horizon)]}}
        ]
    )
    feat_names += horizon_feat_names

    df['delta_target'] = df['inc_4rt_cs_target'] - df['inc_4rt_cs']
    df2 = df.query("season_week >= 5 and season_week <= 45")
    return df2, feat_names

def preprocess_and_plot(dat, state_full_name, featurize, ref_date, population_threshold=0, max_horizon=5):
    df = preprocess_data(dat, state_full_name, population_threshold)
    df1 = transform_incidence(df)
    plot_by_location(df1)
    df2, feat_names = build_features(df1, featurize, ref_date, max_horizon)
    return df2, feat_names
