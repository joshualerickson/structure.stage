# Stratified LiDAR error diagnostic

This analysis asks: within each **recorded field stage**, which unused LiDAR
metrics are associated with the existing LiDAR GBM being correct versus wrong?
It also keeps `comparison_outcome` as a descriptive stratum to distinguish
LiDAR-only errors from cases where both products are wrong.

`comparison_outcome` is not included as a model predictor because it is partly
defined by whether LiDAR matches the field record. Using it as a predictor of
LiDAR correctness would be circular.

## Error composition

LiDAR is wrong at 250 of 528 eligible points. Most errors are recorded SE:

| Recorded field stage | LiDAR wrong | Main wrong calls |
| --- | ---: | --- |
| MSF | 34 | SE: 21; SI: 13 |
| SE | 192 | MSF: 144; SI: 48 |
| SI | 24 | MSF: 14; SE: 10 |

The SE errors are two different mechanisms. The SE -> MSF pathway has 144
points: 65 occur where the rule product is correct and LiDAR is wrong, while 77
occur where both products make the same incorrect MSF call. The SE -> SI pathway
has 48 points: 25 occur where both products are wrong and make different calls,
and 20 occur where the rule product is correct and LiDAR is wrong.

## Stratified candidate signals

The final GBM predictors are excluded. Every listed metric is screened one at a
time within field stage, with review stratum controlled. Effects are odds ratios
for a one-standard-deviation increase in the metric; values above one are
associated with higher odds that LiDAR is correct.

| Recorded field stage | Leading candidate signals for correct LiDAR calls | Caution |
| --- | --- | --- |
| MSF, n = 93 | `zsd` OR 2.88; `n_gt_24_1` OR 2.36; `zmean` OR 2.30; `n_strata_upper` OR 7.17 | 34 errors; several height metrics are correlated. |
| SE, n = 382 | `n_strata_low` OR 0.19; `understory_n_m2` OR 0.33; `pzabovezmean` OR 3.40; `LAD_z_max` OR 3.15; `n_gt_12_1` OR 2.98 | Largest and most informative error stratum, but metrics remain correlated. |
| SI, n = 53 | `bt_diff` OR 0.06; `n_gt_12_1` OR 0.18; `zskew` OR 8.73; `zkurt` OR 26.44 | Only 24 errors; coefficients are unstable leads for review. |

## Why SE is assigned MSF versus SI

Among the 192 incorrect field-SE points, a separate direction model contrasts
LiDAR SI calls (48) with LiDAR MSF calls (144). Greater
`understory_mean_degree` is strongly associated with the SI rather than MSF
error direction (OR 8.30 per SD). Higher `n_trees` / `trees_per_acre` is also
associated with the SI direction (OR 5.80). In contrast, greater `zmean`,
`zsd`, `topo_residual_sd`, and `zquantile_pc1` are associated with the MSF
direction. This supports treating SE -> MSF and SE -> SI as different error
populations for imagery, field-note, and disturbance review.

These are associations with the existing model's error patterns, not causal
effects and not evidence for immediate predictor additions. Candidate metrics
must be tested through nested spatial cross-validation, with feature selection
inside each training fold.

## Files

- `field_stratified_correctness_screen.csv`: all unused-metric screens within each field stage.
- `wrong_prediction_pathways.csv`: field stage, incorrect LiDAR call, and comparison-outcome counts.
- `field_outcome_metric_medians.csv`: selected metric medians by field stage and comparison outcome.
- `se_wrong_direction_screen.csv`: unused-metric screen for SE -> SI versus SE -> MSF errors.
- `wrong_prediction_pathways.png`: error-pathway figure.
