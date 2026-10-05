# CONTEXT.md

## Project summary and current direction

The project's enduring goal is to predict vegetation structural stage using the spatial feature-selection and cross-validation methods described by the owner as Erickson et al. The initial R package now wraps the existing workflow, using the original script now preserved at `dev/legacy/lynx_model.R` as the training reference. The exact paper, method details, and correspondence with the code remain to be recovered.

Data preparation is partly manual because user-provided inputs have different schemas and project-specific requirements. The package should preserve a place for that expert preparation while providing a stable workflow for validating, combining, and training on additional data. “Interface” currently means a callable R workflow API; a GUI has not been requested or designed.

The earlier CNN proposal remains a possible extension after the baseline is reproducible. It does not replace the original modeling objective or require migrating the core package to Python.

## Repository data and artifact policy

Spatial data stay local unless the project owner explicitly authorizes a commit or push. This includes rasters, vector data, GeoPackages, shapefiles and sidecars, and coordinate-bearing or otherwise location-reconstructable tables such as CSVs. Training and validation records, model objects, plots, maps, EDA, rendered reports, and generated run artifacts also stay local by default. Version control is for reusable code, documentation, schemas, metadata templates, and small non-spatial aggregate summaries.

## Observed training behavior in `dev/legacy/lynx_model.R`

This section audits the original script, not a verification of historical data or results. `dev/lynx_model.R` is now a thin package-based file runner.

1. **Inputs:** reads a local `lynx_df.csv`, `structure_extract(1).gpkg`, and `gt_wildlife_surveys.shp` from Downloads paths. Only the CSV feeds the shown training sequence; the spatial objects are read/inspected but not used in its model evaluation.
2. **Labels:** retains Multi-Story Foraging → `msf`, Multi-Story Non-foraging → `se`, Stem Exclusion → `se`, and Stand Initiation → `si`. Other source classes are excluded. The ecological justification for merging non-foraging with stem exclusion needs confirmation. `factor()` has no explicit level order or integer encoding.
3. **Preparation:** seed 124; derives `bt_diff = midstory_mean_betweenness - understory_mean_betweenness`; requests 2,000 rows per class with `slice_sample(n = 2000, by = structure_stage)`. Actual retained counts must be audited, especially for smaller classes; the call does not request replacement.
4. **Predictors:** removes `aws`, `tmin`, `sand`, `clay`, `psst`, `pvt4`, `pvt11`, `def30`, and `slp`; excludes columns ending in `mean_focal` and names containing `strength`, `path_length`, `eigen_ratio`, or `graph_density`. All remaining columns except `x`, `y`, and `structure_stage` become predictors. This broad selection must become an explicit schema before new source metadata is added.
5. **Spatial grouping:** for 50 values from 300 to 500, calls coordinate k-means with `centers = nrow(train) / val`. These values govern nominal samples per cluster, not spatial distances or buffer widths. The script passes potentially noninteger center counts; actual behavior and realized counts need reproduction with recorded R versions. CRS, units, and polygon membership are not checked.
6. **Folds:** sets seed 1234 for each `CAST::CreateSpacetimeFolds(..., k = 10)` call using those clusters. Supplies the first returned component as caret's training `index`. `trainControl` says `method = "repeatedcv"`, `number = 10`, but specifies no `repeats`; do not infer repeated independent evaluations from that name. Save and inspect realized training/assessment memberships.
7. **Selection and training:** sets seed 123, then maps `CAST::ffs` over the control list with `method = "gbm"` and `metric = "Accuracy"`. There is no explicit tuning grid or preprocessing specification. Recover the effective CAST/caret/gbm defaults from the original environment rather than inventing settings.
8. **Metrics:** `multiFourStats` returns Accuracy, Kappa, and macro Youden J (mean of class sensitivity + specificity − 1, ignoring missing values). Despite its comment, it does not return Dist or LogLoss. Probabilities are requested and all resampled predictions are saved in the model objects; selection optimizes Accuracy, not J or balanced accuracy.
9. **Execution and outputs:** requests a 40-worker multicore future plan. A `furrr_options(seed = TRUE, ...)` object is constructed but not passed to `future_map`, so it does not establish the intended parallel RNG settings. Results combine selected variables, model results, and cluster-setting labels into `gbm_ffs_300_500_no_focal.csv`; the script does not explicitly save a deployable model bundle or independent test assessment. It resets the future plan to sequential and ends with interactive help.

The historical `dev/mscv_lynx_model.R` used a different input CSV, replaced missing midstory betweenness with zero, applied `na.omit()`, fit five named predictors, and specified five repeats. It evaluated an external `gt_stuff` CSV rather than running nested outer spatial CV. The active file is now a package runner for `run_mscv_gbm()`; do not borrow the historical preprocessing or results without audit.

## Gaps to resolve during reproduction

- Stable sample/polygon/source identifiers, coordinate CRS, predictor units, source versions, and provider preparation decisions are not established by the training script.
- Coordinate clustering does not explicitly enforce polygon-held-out folds. Existing folds need a polygon membership audit before claims of stand independence.
- The original scripts did not establish a separate outer assessment around feature selection or cluster-setting choice. The package now implements nested outer spatial CV; original-script results remain selection-resampling results until reproduced with the nested workflow.
- No explicit missing-value policy is present in `lynx_model.R`; do not silently import the related script's zero replacement or complete-case filtering.
- Balanced subsampling changes training prevalence. Preserve it for reproduction, while documenting consequences for evaluation and probability interpretation.
- External inputs and the original software environment have not been inspected here. No performance claims have been validated by this documentation update.

## Proposed package workflow and interfaces

The broader target workflow is shown below. The initial package now implements preparation, sampling, audited spatial folds, feature selection/fitting, and tabular prediction; outer assessment and source onboarding automation remain future work.

```text
provider inputs + documented manual preparation
    -> source adapter
    -> validated canonical training records
    -> versioned combination with existing records
    -> reproducible sampling and spatial resampling
    -> spatial feature selection + tuning within outer training data
    -> outer assessment + final model fit
    -> saved model bundle and prediction interface
```

- **Source adapters:** map provider columns and labels, record exclusions and transformations, and accept explicit paths or in-memory objects. Keep provider-specific fixes outside the shared model core.
- **Training records:** require stable sample IDs, source-qualified polygon IDs, source/acquisition provenance, coordinates and CRS, harmonized labels, and named predictors. Document predictor units, resolution, missing values, and extraction provenance. Reject schema conflicts, duplicate records, incompatible labels, or missing grouping metadata before training.
- **Data additions:** preserve unsampled records, register each incoming source/version, validate compatibility, resolve duplicates explicitly, and construct a new dataset version. New data triggers a reproducible full retraining run by default. Track changes to folds and keep any designated external test data out of training.
- **Resampling and modeling:** accept explicit configuration and return fold assignments, selected predictors, tuning results, fitted models, and assessment predictions. Retain the legacy recipe as a reproduction configuration; changes to leakage controls or scientific choices must be separately documented.
- **Model bundle:** store the fitted model, predictor order/schema, class order/mapping, transformations, data version, seeds, realized folds, selection/tuning settings, dependency versions, and metrics. Prediction must validate inputs against this bundle.
- **Package structure:** use `R/`, `tests/testthat/`, generated `man/` documentation, and a vignette demonstrating onboarding and retraining. Keep `dev/` as historical evidence and preparation examples. The package is named `structure.stage`, with exported composable functions and RDS model/workflow persistence.

The first package implementation has been exercised with synthetic data through actual CAST/GBM fitting. Reproduce the provider-data workflow before generalizing to a second provider. Tests should target data-contract failures, class mapping, group isolation, deterministic resampling, and prediction schema/dimensions.

## Initial package implementation

`structure.stage` 0.1.0 now provides `lynx_class_map()`, `prepare_lynx_data()`,
`sample_structure_data()`, `build_spatial_folds()`, `structure_summary()`,
`fit_structure_gbm()`, `predict_structure_gbm()`, `run_lynx_workflow()`,
`run_mscv_gbm()`, `save_mscv_results()`, and `fit_final_structure_gbm()`.
See README.md and the generated roxygen2 help for contracts and usage. The package
uses MIT licensing and maintainer email joshualerickson@gmail.com.

The package requires explicit metadata and predictor definitions, rejects unresolved
missing predictors, and preserves unsampled records in the workflow bundle. Default
clustering uses polygon mean sample coordinates to keep stands intact. Historical
point clustering is available only when its resulting folds pass the polygon
leakage audit. Explicit integer cluster counts, isolated stage seeds, and ordinary
CV with supplied indices replace the original implicit defaults; historical
bit-for-bit reproduction is not claimed. Sampling small classes requires an
explicit capped-count choice. Prediction checks selected predictor definitions.

The full CAST/caret fitted objects, selected variables, tuning results, fold/sample
identities, data versions, and software versions are retained. In CAST 1.0.3,
`ffs()` internally changes saved resamples/predictions to final tuning results,
even when the caller requests all; the original audit above describes the script's
request rather than a guarantee of the resulting object contents.

Model metrics from `fit_structure_gbm()` are labeled `selection_resampling_only`.
`run_mscv_gbm()` returns one spatially outer-held-out probability vector and hard
class per sample, aggregate/per-class/fold metrics, and per-fold selected-variable
sets. `save_mscv_results()` persists the complete RDS and flat report files.
`fit_final_structure_gbm()` tunes then refits the chosen-variable model on all
provided records and is explicitly labeled as a deployment model, not a performance
estimate. Automated provider adapters, raster prediction, and CNN work remain
outside this implementation. Synthetic integration tests exercise actual
CAST/GBM fitting and RDS prediction round trips; original training inputs and
historical performance have not been verified.
Original training inputs and historical performance have not been verified.

## Earlier neighborhood-model rationale (deferred)

Prior project notes report a 3 × 3 focal-mean experiment with improved performance. Its implementation and quantitative results still need recovery; `lynx_model.R` explicitly excludes `mean_focal` columns. The following material retains the proposed research rationale and does not describe completed experiments.

---

## Why revisit the problem

A tabular GBM treats each sampled location as a feature vector. It can learn nonlinear relationships among metrics such as canopy height, cover, vertical complexity, rumple, or connectivity, but it does not directly preserve the arrangement of values around the point.

The earlier 3 × 3 mean filter added limited spatial context by reducing each local neighborhood to a single mean per feature. This approach assumes:

- all neighboring cells contribute equally;
- only local average conditions matter;
- direction and arrangement do not matter;
- every channel should be summarized the same way;
- edges, gaps, contrasts, and fragmentation are secondary.

A CNN relaxes those assumptions. It receives the full neighborhood and learns filters that may represent:

- local means and weighted means;
- edges and boundaries;
- isolated tall or complex cells;
- continuous versus fragmented canopy;
- patchiness and local gaps;
- cross-channel combinations;
- increasingly broad context through stacked convolution layers.

---

## Label structure

The current labels exist at the vegetation-stand polygon level.

A sample point inherits the class of the polygon containing it:

```text
stand polygon with structural-stage class
                  ↓
         randomly sampled point
                  ↓
      sample carries stand label
```

There is no independently observed wall-to-wall pixel-level class raster.

This creates a weak-label issue:

- a polygon label describes the stand generally;
- an individual pixel may be an opening, road, edge, retained tree, riparian strip, or local inclusion;
- disagreement between a model and the stand map may sometimes represent model error and sometimes represent real within-stand heterogeneity.

Therefore, the first CNN should predict the stand-derived class for sampled locations, not claim to recover true pixel-level classes.

---

## Main estimand

The initial estimand is:

> The expected out-of-sample ability to classify a sampled location according to the structural-stage class of its source stand, using LiDAR-derived predictors (and, in later experiments, raster neighborhoods), for unseen polygons or landscapes drawn from the target mapping domain.

This is different from estimating:

> The true structural-stage class of every raster pixel.

The latter would require independent pixel-scale reference data or a carefully justified weak-supervision framework.

---

## Existing GBM baseline

The prior GBM used LiDAR-derived raster predictors extracted at sampled points. The earlier project also included spatial feature selection and spatial cross-validation.

Known prior outcomes suggest approximately three structural classes, currently abbreviated as:

- `msf`;
- `se`;
- `si`.

Previous balanced-accuracy results were approximately in the moderate range, with class-specific differences. These values should be recovered from the original project rather than copied from memory into formal reporting.

Important strengths of the GBM baseline:

- strong performance with limited data;
- nonlinear interactions;
- relatively easy feature importance and partial-dependence summaries;
- practical wall-to-wall raster prediction;
- compatibility with existing spatial CV and feature-selection workflows.

Important limitations:

- local raster cells are treated as independent feature vectors;
- neighborhood arrangement is lost unless manually engineered;
- predictions may be spatially noisy or may confuse locally similar but contextually different structures;
- focal means capture only predefined spatial summaries.

---

## Proposed first CNN

The first deep-learning experiment is a sample-centered neighborhood classifier.

For sample `i`:

```text
input:  H × W × C raster patch centered on the sample
output: one probability vector over structural-stage classes
label:  structural-stage class of the source polygon
```

Example:

```text
9 × 9 cells × 12 LiDAR channels
              ↓
         convolution layers
              ↓
     neighborhood representation
              ↓
      class probability vector
```

This model does not require a complete target raster. Each training point supplies one patch and one label.

At prediction time, the trained patch classifier can be slid across all valid raster locations, or converted to a fully convolutional implementation, to produce wall-to-wall class probabilities.

The resulting wall-to-wall predictions remain predictions trained against stand-derived sample labels, not pixel-scale ground truth.

---

## Proposed experiment matrix

### Baseline A: center-pixel GBM

Input:

- LiDAR metrics at the sampled cell.

Purpose:

- reproduce the original tabular baseline.

### Baseline B: GBM with fixed neighborhood summaries

Input:

- center metrics;
- previous 3 × 3 means.

Purpose:

- quantify the value of manually engineered spatial context.

### Baseline C: center-only neural model

Input:

- center-cell feature vector only.

Purpose:

- determine whether any gain is due merely to changing model family.

### Model D: neighborhood CNN

Input:

- full multichannel patch, initially 7 × 7 or 9 × 9.

Purpose:

- quantify the value of learned spatial context.

### Model E: neighborhood CNN plus GBM probabilities

Input:

- LiDAR raster patch;
- out-of-fold GBM class probabilities;
- optional GBM entropy or margin.

Purpose:

- test whether the CNN can retain strong local GBM decisions while correcting them using neighborhood context.

The target remains the stand-derived sample class.

### Model F: dense U-Net experiments

Potential later uses:

1. **Sparse-supervision U-Net**: predict every pixel but calculate loss only at sampled locations.
2. **Weak-supervision U-Net**: rasterize stand polygons and acknowledge that the labels are polygon-derived pseudo-labels.
3. **Knowledge distillation**: train a U-Net to reproduce GBM probability rasters.
4. **Spatial refinement**: provide GBM probabilities and LiDAR metrics as U-Net inputs while using available stand-derived supervision.

These experiments answer different questions and must not be conflated.

---

## GBM probability fusion

GBM probability layers may be useful because they summarize the tabular model's local evidence.

For three classes, each sampled cell or landscape pixel has:

```text
P(msf)
P(se)
P(si)
```

Optional uncertainty features include:

- entropy;
- top-two probability margin;
- maximum probability;
- calibrated confidence.

The CNN can learn patterns such as:

- retain a high-confidence GBM prediction when the neighborhood supports it;
- revise an isolated high-canopy prediction surrounded by low vegetation;
- reduce confidence near stand boundaries or unusual spatial configurations;
- distinguish a continuous structural patch from a local outlier.

During model training, these probabilities must be out of fold. A GBM trained on the same samples and then used to generate in-sample probabilities would create leakage and unrealistically strong auxiliary inputs.

---

## Candidate LiDAR channels

The exact channel inventory must be recovered from the original workflow. Candidate families include:

### Height and canopy structure

- canopy height model summaries;
- height percentiles;
- maximum, mean, or standard-deviation height;
- canopy cover above defined thresholds.

### Vertical distribution

- density by height strata;
- LAI or LAD summaries;
- vertical entropy;
- residual entropy or residual standard deviation from fitted vertical profiles.

### Surface and complexity

- rumple;
- roughness;
- canopy surface variability;
- local gap or openness measures.

### Voxel or graph connectivity

Prior work included 3D voxel and graph-derived measures such as:

- degree or strength;
- betweenness;
- closeness;
- component counts;
- path length;
- graph density;
- eigenvalue-based ratios.

The operational value and spatial resolution of each metric should be audited before inclusion.

### Terrain covariates

Terrain predictors may be included when they are scientifically justified and available consistently at inference time. Their inclusion should be tested separately because they may improve geographic association without directly measuring vegetation structure.

---

## Patch size

Patch size determines the ecological scale of context.

If raster resolution is 30 m:

- 3 × 3 covers 90 × 90 m;
- 7 × 7 covers 210 × 210 m;
- 9 × 9 covers 270 × 270 m;
- 15 × 15 covers 450 × 450 m.

A larger patch is not automatically better. It may:

- include more relevant stand context;
- cross polygon boundaries more often;
- mix multiple vegetation conditions;
- increase weak-label noise;
- make spatial leakage more likely;
- demand more training data.

Initial tests should compare at least 3 × 3, 7 × 7, and 9 × 9 under identical folds.

The 3 × 3 CNN provides a particularly useful comparison with the earlier 3 × 3 mean-filtered GBM.

---

## Cross-validation strategy

The primary risk is spatial dependence and leakage.

### Minimum split unit

All samples from the same vegetation polygon must remain in the same fold.

### Preferred evaluation levels

1. polygon-held-out spatial CV;
2. larger spatial blocks;
3. leave-project-area or leave-acquisition-area out;
4. independent landscape test set.

### Patch leakage

Neighboring sampled points may have overlapping patches. If one enters training and the other validation, the model may see nearly identical raster neighborhoods.

Fold assignment must occur before patch extraction, and validation/test areas may require buffers based on patch radius.

### Feature engineering leakage

Scaling, feature selection, calibration, and any data-driven thresholds must be fitted within the training fold.

---

## Weak-label diagnostics

Because labels describe stands rather than individual pixels, the project should explicitly analyze label reliability.

Recommended diagnostics:

- distance from sampled point to polygon boundary;
- polygon area;
- proportion of patch falling outside the source polygon;
- within-polygon variation in LiDAR metrics;
- disagreement among samples from the same polygon;
- prediction confidence versus boundary distance;
- performance using polygon interiors only;
- performance after eroding polygons by one or more cells;
- performance for homogeneous versus heterogeneous stands.

These diagnostics may show whether the CNN is learning vegetation context or merely learning polygon-edge artifacts.

---

## Evaluation priorities

Primary metrics:

- balanced accuracy;
- macro F1;
- per-class recall and precision;
- confusion matrix;
- probability calibration;
- log loss or Brier score.

Secondary analyses:

- performance by landscape;
- performance by polygon;
- performance by patch size;
- performance by distance to boundary;
- performance by class prevalence;
- comparison of GBM and CNN uncertainty;
- maps of model disagreement.

A CNN should be considered useful only if it improves spatially held-out performance, probability quality, or ecologically important class behavior enough to justify added complexity.

---

## Interpretation of possible outcomes

### CNN clearly outperforms GBM

Interpretation:

- neighborhood arrangement contains predictive information not captured by center metrics or focal means;
- dense wall-to-wall CNN mapping may be justified;
- investigate which classes and landscapes benefit.

### CNN approximately matches GBM

Interpretation:

- current LiDAR metrics already summarize most relevant structure;
- the GBM may remain preferable for interpretability and operational simplicity;
- learned embeddings may still be useful for ensemble models.

### CNN underperforms GBM

Possible reasons:

- insufficient labeled polygons or landscapes;
- patch sizes poorly matched to stand scale;
- stand-derived labels too noisy for pixel-centered learning;
- excessive model capacity;
- spatial CV exposes domain shift;
- LiDAR channels are already highly aggregated;
- class distinctions are primarily tabular rather than spatial.

### CNN produces more detailed maps but similar validation scores

Interpretation must remain cautious. The map may reflect real sub-stand heterogeneity or unsupported spatial texture. Independent field or image interpretation would be needed to determine which.

---

## Deferred neighborhood-experiment plan

### Phase 1: recover and audit

- locate original GBM code, data dictionary, raster stack, polygon layer, and sampled points;
- document class definitions;
- reproduce prior metrics;
- map the original folds;
- identify raster resolution and channel alignment;
- quantify samples per polygon and class.

### Phase 2: patch dataset

- assign folds at polygon and spatial-block levels;
- extract patches at multiple sizes;
- store nodata masks and metadata;
- test center-cell alignment;
- visualize random patches by class;
- inspect patches near and far from boundaries.

### Phase 3: baselines

- rerun center-only GBM;
- rerun 3 × 3 mean GBM;
- train center-only neural baseline;
- create a common evaluation pipeline.

### Phase 4: CNN

- train compact 3 × 3, 7 × 7, and 9 × 9 CNNs;
- compare fold-level and class-level performance;
- tune only after establishing a valid baseline;
- generate saliency or attribution diagnostics cautiously.

### Phase 5: fusion and mapping

- generate out-of-fold GBM probabilities;
- train CNN with probability fusion;
- apply best models wall to wall;
- produce probability, entropy, and disagreement rasters;
- compare stand-level summaries and spatial patterns.

### Phase 6: decide on U-Net

Proceed to a U-Net only if one or more of the following is true:

- the neighborhood CNN improves spatially held-out classification;
- dense prediction speed becomes a bottleneck;
- sparse-supervision or weak-supervision research is a project objective;
- GBM probability refinement is operationally valuable;
- there is a plan for independent evaluation of within-stand spatial detail.

---

## Initial technical stack

Use an R package for the current workflow, preserving CAST, caret, and gbm for baseline reproduction. Existing scripts also use sf, dplyr, purrr, future, furrr, plyr, and stringr; decide which are package dependencies as functions are extracted. Record versions and effective defaults. Worker counts and future plans must be configurable.

PyTorch and Python raster tooling remain options for a later CNN extension, with a documented handoff to the R workflow if adopted.

---

## Decisions that still need to be recovered or confirmed

- authoritative class names and definitions;
- number of labeled polygons and samples;
- number of independent landscapes or acquisition units;
- LiDAR raster resolution;
- full predictor inventory;
- original focal-mean implementation;
- effective caret/gbm tuning defaults and original dependency versions;
- exact Erickson et al. reference and its correspondence to CAST forward feature selection;
- realized coordinate-cluster fold geometry, CRS, and polygon isolation;
- nodata and edge-handling rules;
- whether all predictors are available consistently wall to wall;
- whether polygon boundaries were produced independently of LiDAR;
- operational target region and expected transfer distance.

---

## Working project statement

> We are building a reusable R workflow for vegetation structural-stage prediction using LiDAR-derived predictors and the Erickson et al. spatial feature-selection and cross-validation approach, with the exact methodological reference still to be recorded. The workflow will accommodate provider-specific preparation through explicit interfaces, support additional training data and reproducible retraining, and preserve the existing GBM as the baseline. Neighborhood CNNs remain a later comparison under spatial validation. Stand-derived sample labels are not independent pixel-level truth.

## Interpretation branch

The `interpretation` branch provides a deliberately three-class framework for
final GBM models. `analyze_structure_predictions()` records all class
probabilities, prediction margins, and pairwise competitions, including a named
`msf`–`se` table. `ablate_structure_gbm()` performs one-variable-at-a-time
plus/minus SD perturbations, outputting point changes, average changes, class
transitions, and the change in `P(msf) - P(se)`. `save_structure_ablation()`
writes these results to versioned CSV files and an RDS. The perturbation analysis
holds correlated predictors fixed and can create unrealized combinations; it is
therefore a model-sensitivity diagnostic rather than a causal explanation.
