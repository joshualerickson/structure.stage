# Omitted-metric error diagnostic

This diagnostic asks whether LiDAR metrics **not used by the final five-variable
GBM** are associated with the existing GBM matching the recorded field stage.
The outcome is binary: `lidar_matches_field` (TRUE/FALSE). Every model controls
for recorded field stage and review stratum.

## Why the final screen is logistic rather than mixed

A 7.5 km spatial-block random-intercept model was attempted first. The 528
points occupy six populated blocks, but the random-intercept variance was zero
and the fit was singular. This means this sample does not support estimation of
additional broad-block variation after field stage and review status are
included. The reported metric screen therefore uses the corresponding fixed
effect logistic models. It does **not** establish that spatial dependence is
absent; the blocks are few and the data are not a random spatial sample.

## What the screen does

Each unused metric is standardized and fitted **one at a time**. The coefficient
is the change in log-odds that the LiDAR GBM matches the field call per one
standard deviation increase in that metric. Positive values indicate more odds
of a correct LiDAR call; negative values indicate less. P-values are adjusted
across the 44 screens using Benjamini-Hochberg false-discovery-rate adjustment.

The final five GBM predictors are excluded: `zmax`, `n_strata_low_mid`,
`n_gt_6_1`, `midstory_mean_degree`, and `understory_mean_betweenness`. Some
screened metrics remain related to these predictors, so this is evidence of
candidate residual structure, not independent causal importance or a new model
performance estimate.

## Main results

The strongest individual associations include lower correctness with greater
`understory_mean_degree` (odds ratio 0.50 per SD), lower correctness with greater
`understory_n_m2` (0.49), and greater correctness with `LAD_z_max` (2.02).
`zskew` is also negatively associated with correctness (0.65), while
`topo_entropy` is positively associated (1.31). The 5 x 5 Landsat summer-fall
difference mean has an odds ratio of 0.82 per SD and a BH-adjusted q-value of
0.062; it is a candidate for further testing, not a confirmed operational
predictor.

The joint diagnostic correlation-prunes screened metrics and should be read even
more cautiously. It is an exploratory description of residual error patterns.
Any candidate additions to the GBM must be evaluated through the established
nested spatial cross-validation workflow, with selection performed inside each
outer training fold.

## Files

- `univariate_omitted_metric_screen.csv`: all 44 one-metric screens.
- `univariate_omitted_metric_screen.png`: coefficient plot with 95% Wald intervals.
- `mixed_model_summary.csv`: baseline and joint diagnostic fit summaries.
- `joint_metric_selection.csv`: correlation-pruned metrics used in the joint diagnostic.
- `joint_model_fixed_effects.csv`: joint diagnostic coefficients.
