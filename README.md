# structure.stage

An R package for vegetation structural-stage classification using CAST spatial
forward feature selection and caret/gbm. This first implementation extracts the
training workflow from `dev/legacy/lynx_model.R`. Models predict classes inherited
from vegetation stands; their resampling scores are used for feature selection
and tuning, and do not constitute independent spatial assessment.

## Install and develop

From the project directory, with the DESCRIPTION dependencies installed:

```sh
R CMD INSTALL .
```

Regenerate help pages and run tests after changes:

```r
roxygen2::roxygenise()
testthat::test_local()
```

Every exported function has roxygen2 documentation and generated help in `man/`.
Calls to external packages (including base utilities) are namespaced. Internal
package helpers are called directly. No `library()` calls are needed in a workflow.

## Prepare provider data

Keep source-specific corrections in your preparation code. Supply a data frame
with character `sample_id`, `polygon_id`, `source_id`, and `acquisition_id`, numeric
`x` and `y` in a declared projected CRS, `structure_stage`, and numeric predictors.
Sample IDs must be globally unique; polygon IDs must identify one source and one
harmonized stand label. Retain genuine identifiers when adding future sources.

Provide a predictor-schema data frame, with **one row per selected candidate in
explicit order**, containing:

| Field | Meaning |
| --- | --- |
| `name` | Exact predictor column name |
| `units` | Physical units or `dimensionless` |
| `resolution` | Positive raster cell size in CRS units |
| `nodata` | Documented upstream missing-value handling |
| `interpretation` | Ecological meaning and extraction definition |
| `source_version` | Predictor product/version identifier |

The package rejects NA/NaN/Inf. Convert physical nodata sentinels and resolve
missing values explicitly upstream; the schema's nodata description is not an
executable conversion rule. Distribution-derived imputation or transformations
must be fitted within training folds. The package performs no such transformations.

By default, `bt_diff` is derived from `midstory_mean_betweenness` minus
`understory_mean_betweenness`; provide both source columns even if only the
difference enters the candidate schema. Set `derive_bt_diff = FALSE` when the
workflow deliberately does not use this derivation. An existing difference must
agree with the computed one.

The legacy class map is exposed by `structure.stage::lynx_class_map()`. It merges
Multi-Story Non-foraging and Stem Exclusion into `se`. Confirm the ecological
rationale before adopting this mapping for other projects. Class order is explicit:
`msf`, `se`, `si`. Unknown labels error by default; `unknown_labels = "drop"`
records exclusions and reproduces the original filtering choice.

For baseline reproduction, the candidate schema should exclude the original
`aws`, `tmin`, `sand`, `clay`, `psst`, `pvt4`, `pvt11`, `def30`, `slp` columns,
columns ending in `mean_focal`, and names containing `strength`, `path_length`,
`eigen_ratio`, or `graph_density`. Unlike the original script, the package does not
infer predictors from every remaining column. Review the schema when adding data.

## Train and predict

The following uses provider-prepared objects; no project data are bundled:

```r
prepared <- structure.stage::prepare_lynx_data(
  data = training_records,
  predictor_schema = predictor_schema,
  crs = training_crs,
  data_version = "training-v1",
  unknown_labels = "drop"
)

sampled <- structure.stage::sample_structure_data(
  prepared, n_per_class = 2000L, seed = 124L, small_classes = "all"
)
folds <- structure.stage::build_spatial_folds(
  sampled, samples_per_cluster = 400, k = 10L
)
model <- structure.stage::fit_structure_gbm(sampled, folds)
base::saveRDS(model, "model-v1.rds")

probabilities <- structure.stage::predict_structure_gbm(
  model, newdata = prediction_records, predictor_schema = predictor_schema
)
```

Choose cluster targets that your data support: the cluster count is
`floor(n_samples / samples_per_cluster)`, must be at least `k`, and must be smaller
than the number of clustering units. Each training and assessment fold must
contain every class. Failures require revisiting the spatial design, not row-wise
random splitting. Start with a single configuration before a costly full sweep.

For the original 50-setting sweep:

```r
run <- structure.stage::run_lynx_workflow(
  prepared,
  cluster_sizes = base::seq(300, 500, length.out = 50),
  small_classes = "all"
)
base::saveRDS(run, "lynx-run-v1.rds")
utils::write.csv(
  run$results[, base::setdiff(base::names(run$results), "selected_predictors")],
  "selection-results-v1.csv", row.names = FALSE
)
```

`run` retains unsampled and sampled records, fold memberships, fitted models,
selected variables, tuning results, class maps, predictor definitions, stage seeds,
and software versions. CAST may retain only final-tuning resample predictions
internally, despite the requested `savePredictions = "all"`; inspect
`run$models[[1]]$model$pred` and the fitted control rather than assuming all tuning
predictions exist. No winning setting is automatically chosen.

`dev/lynx_model.R` supplies a thin `train_lynx_files()` runner for canonical CSV
inputs and an RDS output. It excludes unknown labels explicitly, matching the
legacy choice. The package API offers finer control.

Prediction requires the selected predictor definitions to match the model bundle,
including source version, and reorders columns by name. Derive `bt_diff` in
prediction records if selected. Class probabilities always use the saved class
order. No raster prediction or CRS transformation is implemented in this step.

## Deliberate changes from the legacy script

- Paths, metadata, and predictors are explicit inputs. No Downloads paths or
  unused spatial layers are loaded by the package.
- The default clusters mean sample coordinates per polygon, then assigns whole
  polygons to folds. `cluster_unit = "sample"` offers the historical point
  clustering but rejects folds that split polygons. Neither method provides
  distance buffers or automatically proves landscape independence.
- Cluster counts are explicitly floored; seeds are isolated per stage and restored
  afterward. This is reproducible but does not promise the original implicit RNG
  sequence or identical historical folds.
- Supplied folds use `method = "cv"` with explicit training and assessment indices,
  rather than the original `repeatedcv` label without explicit repeats.
- Small classes error by default. `small_classes = "all"` retains all available
  rows without replacement, matching the original capped sampling behavior.
- Execution defaults to sequential. `allow_parallel = TRUE` uses a caller-managed
  caret backend; the package never sets a 40-worker plan or changes a future plan.
- CAST starts with two-variable candidates; this interface requires at least three
  predictors because CAST 1.0.3 assumes an additional forward-selection step.

Spatial feature selection is preserved. To assess performance, reserve outer
spatial partitions and run the entire selection/tuning procedure inside each outer
training partition; assess only on its held-out records. This first package step
now provides that nested assessment framework. Do not report selection metrics
from `fit_structure_gbm()` or final-model tuning metrics as independent
out-of-sample performance.

## Reportable nested spatial CV and final model

Use the same prepared dataset and spatial design for both stages. First, create
outer folds, then run the full model-selection process inside every outer training
partition. This produces exactly one held-out prediction per sample, which is the
result set to report against stand-derived sample labels.

```r
outer_folds <- structure.stage::build_spatial_folds(
  prepared, samples_per_cluster = 400, k = 10L
)

mscv <- structure.stage::run_mscv_gbm(
  prepared,
  outer_folds = outer_folds,
  inner_samples_per_cluster = 200,
  inner_k = 5L
)

structure.stage::save_mscv_results(mscv, "outputs/metrics", "lynx-v1")
mscv$metrics$overall
mscv$metrics$per_class
mscv$selected
```

With the default `selected_predictors = NULL`, CAST forward selection and GBM
tuning happen anew in each outer training partition. `mscv$selected` shows which
variables were chosen in each fold. The result set writes an RDS object, held-out
probabilities and hard classes, overall and per-class metrics, fold metrics,
confusion matrix, and selected-variable table. Existing output paths cause an
error, so reported artifacts are not silently replaced.

If you first choose a fixed variable set from a wholly independent development
dataset or a prespecified scientific design, pass it explicitly:

```r
mscv <- structure.stage::run_mscv_gbm(
  prepared, outer_folds,
  selected_predictors = c("zmax", "n_strata_low_mid", "n_gt_6_1"),
  inner_samples_per_cluster = 200
)
```

Choosing those variables by inspecting the same outer folds makes the reported
score optimistic. When variables are learned from this dataset, use nested
selection. After inspecting the nested selection frequencies and deciding the
final predictor set without using outer-fold outcome performance to choose among
alternatives, fit the deployment model on all available final-training records:

```r
final_tuning_folds <- structure.stage::build_spatial_folds(
  prepared, samples_per_cluster = 400, k = 10L
)
final_model <- structure.stage::fit_final_structure_gbm(
  prepared,
  selected_predictors = c("zmax", "n_strata_low_mid", "n_gt_6_1"),
  tuning_folds = final_tuning_folds
)
base::saveRDS(final_model, "outputs/models/lynx-v1-final.rds")
```

`final_model` tunes and then refits on all supplied records. It is a deployment
bundle, not a performance estimate. Use `mscv` metrics in reports and the final
model only for subsequent prediction.

## Three-class model behavior and SD ablation

Use the final model bundle, an equivalent caret GBM `train` object, or a path to
either RDS. The analysis is fixed to the known `msf`, `se`, and `si` outcomes.
It treats `msf`–`se` as a named competition because their LiDAR signatures can
be similar.

```r
ablation <- structure.stage::ablate_structure_gbm(
  model = "outputs/models/lynx-v1-final.rds",
  newdata = final_training_records,
  reference_data = final_training_records,
  variables = c("understory_mean_betweenness", "zmax", "n_strata_low_mid"),
  sd_multiplier = 1
)
structure.stage::save_structure_ablation(
  ablation, "outputs/interpretation", "lynx-v1-final"
)
```

For every point and selected variable, the package predicts the original
probability vector, then repeats the prediction after subtracting and adding one
reference standard deviation. All other predictors remain fixed.

| File suffix | Contents |
| --- | --- |
| `_point_changes.csv` | Point-level original/perturbed values, classes, and probability deltas |
| `_average_changes.csv` | Mean probability changes and class-change rates overall and within each baseline class |
| `_class_transitions.csv` | Baseline-to-perturbed class counts and rates |
| `_msf_se_changes.csv` | Focused `msf`–`se` contrast by variable, direction, and baseline class |
| `_baseline_predictions.csv` | Unperturbed three-class predictions, confidence, and top-two margin |
| `_pairwise_competition.csv` | Point-level `msf`–`se`, `msf`–`si`, and `se`–`si` comparisons |

In `_msf_se_changes.csv`, a positive `mean_delta_msf_minus_se` shifts model
support toward `msf` relative to `se`; a negative value shifts it toward `se`.
Small baseline margins and high class-transition rates identify points where a
variable helps the model distinguish the pair. This is conditional model behavior,
not a causal ecological effect: holding correlated LiDAR metrics fixed can create
unobserved predictor combinations.

Use `structure.stage::analyze_structure_predictions(final_model, newdata)` when
you only need the three-class and pairwise competition tables. The file runner
`dev/interpretation.R` provides `run_interpretation_files()` for model RDS and
CSV inputs.

### Betweenness at equal fixed heights

To ask how understory betweenness changes the model response when height is held
equal, use a fixed-height conditional response rather than the global ablation:

```r
fixed_height <- structure.stage::analyze_msf_se_height_betweenness(
  final_model,
  reference_data = final_training_records,
  height_variable = "zmax",
  betweenness_variable = "understory_mean_betweenness",
  height_quantiles = c(0.25, 0.50, 0.75),
  betweenness_quantiles = seq(0.05, 0.95, by = 0.05)
)
```

At each requested height quantile, the function gives every reference record the
same `zmax`, varies understory betweenness, preserves all other observed
predictors, and averages the resulting probabilities. It therefore answers how
the fitted model responds at equal heights, while retaining `P(si)` so a change
in the MSF–SE contrast is not mistaken for an MSF transition when it is actually
a shift to SI. It is a conditional model response, not held-out performance.

For actual MSF–SE performance at comparable observed heights, use outer held-out
predictions from nested CV:

```r
height_performance <- structure.stage::evaluate_msf_se_height_bins(
  mscv, prepared, height_variable = "zmax", bins = 4L
)
```

This reports MSF/SE recall and balanced accuracy within observed height bands.
Use bands with adequate counts of both stand-derived classes. The plotting helpers
in `dev/interpretation_plots.R` provide `plot_fixed_height_msf_se()`,
`plot_fixed_height_probabilities()`, and `plot_msf_se_height_performance()`.
The exact Erickson et al. citation and equivalence to the historical published
method still need confirmation.

To add data, combine unsampled provider-prepared records, validate IDs, labels and
predictor definitions, assign a new data version, and retrain. There is no
incremental GBM update or automatic source harmonization in this release.

## License

MIT. Maintainer: Josh Erickson <joshualerickson@gmail.com>.
