import numpy as np
import pandas as pd
import lightgbm as lgb
import time
import warnings
import loader

def prepare_train_test(df, feat_names):
    max_week = df['wk_end_date'].max()
    df_test = df.loc[df.wk_end_date == df.wk_end_date.max()].copy()
    x_test = df_test[feat_names]

    df_train = df.loc[~df['delta_target'].isna().values]
    x_train = df_train[feat_names]
    y_train = df_train['delta_target']

    for col in ['season_week', 'delta_xmas']:
        x_train[col] = pd.to_numeric(x_train[col], errors='coerce')
        x_test[col] = pd.to_numeric(x_test[col], errors='coerce')

    return df_train, df_test, x_train, x_test, y_train




def fit_quantile_models(x_train, y_train, x_test, df_train, q_levels, num_bags, bag_frac_samples, ref_date):
    rng_seed = int(time.mktime(ref_date.timetuple()))
    rng = np.random.default_rng(seed=rng_seed)
    lgb_seeds = rng.integers(1e8, size=(num_bags, len(q_levels)))

    test_preds_by_bag = np.empty((x_test.shape[0], num_bags, len(q_levels)))
    feature_importance_df = pd.DataFrame(0, index=x_train.columns, columns=range(num_bags * len(q_levels)))
    train_seasons = df_train['season'].unique()

    warnings.simplefilter(action="ignore", category=DeprecationWarning)

    for b in range(num_bags):
        print(f'bag number {b+1}')
        bag_seasons = rng.choice(train_seasons, size=int(len(train_seasons) * bag_frac_samples), replace=False)
        bag_obs_inds = df_train['season'].isin(bag_seasons)

        for q_ind, q_level in enumerate(q_levels):
            model = lgb.LGBMRegressor(
                verbosity=-1,
                objective='quantile',
                alpha=q_level,
                random_state=lgb_seeds[b, q_ind]
            )
            model.fit(X=x_train.loc[bag_obs_inds], y=y_train.loc[bag_obs_inds])
            test_preds_by_bag[:, b, q_ind] = model.predict(x_test)

            feature_importance_df.iloc[:, b * len(q_levels) + q_ind] = model.feature_importances_

    return feature_importance_df, test_preds_by_bag

def postprocess_predictions(df_test, test_preds_by_bag, q_labels, ref_date):
    df_test.reset_index(drop=True, inplace=True)
    test_pred_qs = np.median(test_preds_by_bag, axis=1)
    test_pred_qs_sorted = np.sort(test_pred_qs, axis=1)
    test_pred_qs_df = pd.DataFrame(test_pred_qs_sorted, columns=q_labels)

    #df_test.reset_index(drop=True, inplace=True)
    df_test_w_preds = pd.concat([df_test, test_pred_qs_df], axis=1)

    cols_to_keep = ['wk_end_date', 'location', 'inc_4rt_cs', 'horizon',
                    'inc_4rt_center_factor', 'inc_4rt_scale_factor']
    preds_df = df_test_w_preds[cols_to_keep + q_labels]

    preds_df = pd.melt(preds_df, id_vars=cols_to_keep, var_name='quantile', value_name='delta_hat')

    preds_df['inc_4rt_cs_target_hat'] = preds_df['inc_4rt_cs'] + preds_df['delta_hat']
    preds_df['inc_4rt_target_hat'] = (
        (preds_df['inc_4rt_cs_target_hat'] + preds_df['inc_4rt_center_factor']) *
        (preds_df['inc_4rt_scale_factor'] + 0.01)
    )
    preds_df['value'] = np.maximum((np.maximum(preds_df['inc_4rt_target_hat'], 0.0) ** 2 - 0.01), 0.0)

    preds_df = preds_df[['wk_end_date', 'location', 'horizon', 'quantile', 'value']] \
        .rename(columns={'quantile': 'output_type_id'})

    preds_df['target_end_date'] = preds_df['wk_end_date'] + pd.to_timedelta(7 * preds_df['horizon'], unit='days')
    preds_df['reference_date'] = ref_date
    preds_df['target'] = 'Flu ED visits pct'
    preds_df['horizon'] = preds_df['horizon'] - 1
    preds_df['output_type'] = 'quantile'

    preds_df = preds_df[['reference_date', 'location', 'horizon', 'target',
                         'target_end_date', 'output_type', 'output_type_id', 'value']]

    return preds_df

def generate_quantile_forecasts(
    df,
    feat_names,
    q_levels,
    q_labels,
    num_bags,
    bag_frac_samples,
    ref_date
):
    df_train, df_test, x_train, x_test, y_train = prepare_train_test(df, feat_names)

    feature_importance_df, test_preds_by_bag = fit_quantile_models(
        x_train, y_train, x_test, df_train, q_levels, num_bags, bag_frac_samples, ref_date
    )

    preds_df = postprocess_predictions(df_test, test_preds_by_bag, q_labels, ref_date)

    return preds_df, feature_importance_df
