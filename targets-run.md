# targets-results

Results from tuning

``` r
tuned |> 
        collect_metrics() |>
        mutate(across(where(is.numeric), ~ round(.x, 3))) |>
        arrange(.metric) |>
        as.data.frame()
```

       penalty mixture     .metric .estimator  mean n std_err               .config
    1    0.001       0 mn_log_loss     binary 0.029 5   0.004 Preprocessor1_Model01
    2    0.001       0 mn_log_loss     binary 0.026 5   0.004 Preprocessor1_Model02
    3    0.002       0 mn_log_loss     binary 0.024 5   0.004 Preprocessor1_Model03
    4    0.003       0 mn_log_loss     binary 0.023 5   0.004 Preprocessor1_Model04
    5    0.004       0 mn_log_loss     binary 0.021 5   0.003 Preprocessor1_Model05
    6    0.006       0 mn_log_loss     binary 0.020 5   0.003 Preprocessor1_Model06
    7    0.009       0 mn_log_loss     binary 0.019 5   0.003 Preprocessor1_Model07
    8    0.013       0 mn_log_loss     binary 0.019 5   0.003 Preprocessor1_Model08
    9    0.019       0 mn_log_loss     binary 0.019 5   0.003 Preprocessor1_Model09
    10   0.028       0 mn_log_loss     binary 0.019 5   0.003 Preprocessor1_Model10
    11   0.040       0 mn_log_loss     binary 0.019 5   0.003 Preprocessor1_Model11
    12   0.059       0 mn_log_loss     binary 0.019 5   0.003 Preprocessor1_Model12
    13   0.085       0 mn_log_loss     binary 0.020 5   0.003 Preprocessor1_Model13
    14   0.123       0 mn_log_loss     binary 0.020 5   0.003 Preprocessor1_Model14
    15   0.178       0 mn_log_loss     binary 0.021 5   0.003 Preprocessor1_Model15
    16   0.001       0     roc_auc     binary 0.831 5   0.034 Preprocessor1_Model01
    17   0.001       0     roc_auc     binary 0.846 5   0.034 Preprocessor1_Model02
    18   0.002       0     roc_auc     binary 0.860 5   0.034 Preprocessor1_Model03
    19   0.003       0     roc_auc     binary 0.875 5   0.034 Preprocessor1_Model04
    20   0.004       0     roc_auc     binary 0.888 5   0.033 Preprocessor1_Model05
    21   0.006       0     roc_auc     binary 0.901 5   0.031 Preprocessor1_Model06
    22   0.009       0     roc_auc     binary 0.911 5   0.028 Preprocessor1_Model07
    23   0.013       0     roc_auc     binary 0.921 5   0.024 Preprocessor1_Model08
    24   0.019       0     roc_auc     binary 0.930 5   0.020 Preprocessor1_Model09
    25   0.028       0     roc_auc     binary 0.942 5   0.014 Preprocessor1_Model10
    26   0.040       0     roc_auc     binary 0.950 5   0.011 Preprocessor1_Model11
    27   0.059       0     roc_auc     binary 0.954 5   0.009 Preprocessor1_Model12
    28   0.085       0     roc_auc     binary 0.956 5   0.008 Preprocessor1_Model13
    29   0.123       0     roc_auc     binary 0.957 5   0.008 Preprocessor1_Model14
    30   0.178       0     roc_auc     binary 0.957 5   0.008 Preprocessor1_Model15

Best tuning parameters via log loss

``` r
tuned |>
        select_best(metric = 'mn_log_loss') |>
        as.data.frame()
```

         penalty mixture               .config
    1 0.02795301       0 Preprocessor1_Model10

Results on validation set

``` r
metrics_valid |>
        as.data.frame()
```

          .metric .estimator  .estimate              .config
    1 mn_log_loss     binary 0.04344517 Preprocessor1_Model1
    2     roc_auc     binary 0.94708756 Preprocessor1_Model1

Results on upcoming games

``` r
metrics_test |>
        as.data.frame()
```

       yearpublished     .metric .estimator   .estimate
    1           2022 mn_log_loss     binary 0.013310515
    2           2023 mn_log_loss     binary 0.009870775
    3           2024 mn_log_loss     binary 0.012880495
    4           2025 mn_log_loss     binary 0.017841965
    5           2026 mn_log_loss     binary 0.005120633
    6           2022     roc_auc     binary 0.966745721
    7           2023     roc_auc     binary 0.892884049
    8           2024     roc_auc     binary 0.964061096
    9           2025     roc_auc     binary          NA
    10          2026     roc_auc     binary          NA
