# AGENTS.md

## Project title

Vegetation Structural-Stage Modeling Workflow

## Purpose and current priority

The enduring goal is to predict vegetation structural stage using LiDAR-derived predictors and the spatial feature-selection and cross-validation methods identified by the project owner as Erickson et al. The exact publication and correspondence between its methods and this code must still be recorded; do not invent a citation or claim verified methodological equivalence.

The immediate priority is to turn the existing R modeling workflow into an extensible R package with an explicit, callable workflow interface. `dev/legacy/lynx_model.R` preserves the original training behavior; `dev/lynx_model.R` is now a thin package runner. Read `CONTEXT.md` for the observed sequence, limitations, and package direction before implementation.

Training-data preparation is intentionally partly manual because providers supply different schemas and data with project-specific intricacies. Support those differences through source-specific adapters and documented preparation decisions, followed by a common validated training-data contract. Additional data should enter through that contract and produce a versioned retraining run.

The initial R package is implemented. Extend the composable functions in `R/` with namespaced external calls and roxygen2 documentation. Do not assume a graphical interface, introduce a new model family, or silently rewrite the scientific method.

## Workflow and package rules

- Prefer R for the core package, retaining CAST, caret, and gbm behavior until an explicit comparison justifies changing it.
- Separate source ingestion, label harmonization, validation, sampling, spatial resampling, feature selection, fitting, evaluation, and prediction into composable functions with explicit inputs and returned objects.
- Keep provider-specific paths, manual edits, interactive inspection, and execution examples in `dev/` or external configuration; reusable package functions belong in `R/`.
- Require stable sample and polygon identifiers, source/acquisition provenance, coordinates with a declared CRS, an explicit class mapping, and a documented predictor schema. Audit missing legacy metadata rather than fabricating it.
- Retain unsampled prepared records so additions can be combined, checked for duplicate samples and polygon IDs, and resampled reproducibly. Adding data means retraining by default, not incremental updates to an existing GBM.
- Persist data versions, realized folds, selected predictors, tuning settings, seeds, package versions, predictions, and evaluation results with each run.
- Preserve legacy settings in a documented reproduction configuration. Changes to label mapping, sampling, missing values, predictors, fold geometry, or selection metrics must be explicit and compared with the reproduction run.
- For reported performance, use `run_mscv_gbm()` so spatial feature selection and tuning occur inside each outer training fold. Its outer held-out predictions are the reportable estimate against stand-derived sample labels. `fit_final_structure_gbm()` uses all final-training records for deployment and is not a performance estimate.
- Make parallel execution configurable; do not hard-code 40 workers or change a caller's future plan without restoring it.

## Later scientific extension

After the R workflow is reproducible, evaluate whether learned LiDAR raster neighborhoods improve on the existing GBM and the earlier 3 × 3 focal-mean approach. The CNN guidance below is a later research roadmap, not the current package implementation priority.

For that experiment, each patch is centered on a sampled location carrying its source stand's label and produces one class probability vector. No independently observed wall-to-wall pixel-level class raster is assumed.

---

## Non-negotiable interpretation rules

### Labels are stand-derived

A point label is the structural-stage class assigned to the polygon containing the point. It is not independently observed pixel-level truth.

Use the following terminology consistently:

- **stand label**: the mapped structural-stage class assigned to a polygon;
- **sample label**: the stand label attached to a sampled point;
- **pseudo-label raster**: a stand label copied to all raster cells inside a polygon;
- **GBM prediction raster**: wall-to-wall hard classes or class probabilities predicted by the existing GBM;
- **pixel truth**: reserve this term for independently observed pixel-scale reference data, which is not currently available.

Do not describe rasterized polygons or GBM predictions as pixel-level ground truth.

### The first CNN is not a U-Net

The first deep-learning model should be a patch classifier:

```text
H × W × C LiDAR patch -> CNN -> one class
```

A U-Net is a later experiment for dense wall-to-wall output, sparse supervision, weak supervision, or GBM teacher-student modeling.

### Spatial validation is mandatory

Random row-wise train/test splitting is prohibited.

At minimum, all observations from a vegetation polygon must remain in the same fold. Preferred validation includes spatial blocks or geographically independent landscapes.

---

## Initial hypotheses

### H1: neighborhood information adds signal

A CNN will improve balanced accuracy, macro F1, or per-class recall when structural stages differ in spatial configuration rather than only in local LiDAR values.

Examples include:

- continuous versus fragmented canopy;
- isolated tall residual trees versus broadly mature canopy;
- uniform regeneration versus patchy initiation;
- canopy gaps embedded within otherwise mature forest;
- stand edges or narrow linear vegetation structures.

### H2: fixed smoothing captures only part of that signal

The prior 3 × 3 mean-filtered GBM should outperform or stabilize the center-only GBM in some cases, but a CNN may improve further by learning:

- unequal spatial weights;
- nonlinear cross-channel interactions;
- edges and local contrasts;
- directional or asymmetric patterns;
- multiscale spatial context.

### H3: gains may be class-specific

Convolutional context may improve some structural classes while leaving others unchanged or worse. Report class-level effects, not only an overall score.

---

## Expected data representation

For sample `i`:

```text
X_i: H × W × C LiDAR raster patch
Y_i: structural-stage label inherited from the source polygon
```

Where:

- `H` and `W` are odd-valued patch dimensions such as 3, 7, 9, 15, or 21;
- `C` is the number of LiDAR-derived raster channels;
- the center cell corresponds to the original sampled point.

Current candidate channels may include:

- canopy height or height percentiles;
- canopy cover;
- rumple;
- LAI, LAD, or height-bin density measures;
- vertical complexity or entropy;
- spline residual standard deviation or entropy;
- voxel- or stratum-derived connectivity metrics;
- terrain or topographic covariates, if scientifically justified.

Do not silently add channels. Every channel must be documented with units, resolution, nodata rules, and ecological interpretation.

---

## Repository conventions

R package layout:

```text
.
├── AGENTS.md
├── CONTEXT.md
├── DESCRIPTION
├── NAMESPACE
├── R/                 # reusable workflow functions
├── man/               # generated function documentation
├── tests/testthat/     # data contracts, folds, selection, prediction
├── vignettes/         # onboarding a source and retraining
├── inst/              # small schemas/configuration examples
└── dev/               # legacy scripts and provider-specific preparation
```

Keep large training inputs and run artifacts outside installed package contents. A later CNN extension may introduce a separate Python component with an explicit data handoff; it does not determine the core package layout.

Do not commit raw LiDAR, large raster stacks, extracted patches, model weights, or credentials unless explicitly intended and legally permitted.

---

## Later model-comparison sequence

After reproducing and packaging the R baseline, use the same outer spatial folds for all models in a comparison.

### Experiment 0: label and fold audit

Before training:

- count stands, samples, classes, landscapes, and acquisition units;
- inspect class balance by fold;
- inspect samples per polygon;
- quantify polygon size and within-polygon heterogeneity;
- map all folds;
- verify that no polygon occurs in more than one fold;
- verify that patch footprints do not cross into validation areas in a way that creates leakage.

### Experiment 1: existing GBM baseline

Input:

- center-cell LiDAR metrics or the original tabular predictors.

Output:

- class probabilities and hard class.

This should reproduce the prior workflow as closely as possible.

### Experiment 2: manually smoothed GBM

Input:

- center-cell metrics;
- 3 × 3 neighborhood means used in the earlier workflow.

Optional secondary summaries may be tested only after the original baseline is reproduced:

- standard deviation;
- minimum and maximum;
- quantiles;
- local range;
- focal gradients.

### Experiment 3: center-only neural baseline

Use a multilayer perceptron or a 1 × 1 convolution equivalent. This distinguishes neural-network effects from spatial-context effects.

### Experiment 4: neighborhood CNN

Input:

- unsmoothed multichannel patches centered on labeled samples.

Output:

- one class probability vector per patch.

Begin with a compact architecture and patches such as 7 × 7 or 9 × 9.

### Experiment 5: CNN with GBM probabilities

Add out-of-fold GBM class probabilities and optional GBM entropy as auxiliary inputs.

The target remains the stand-derived sample class.

GBM probabilities used during training must be generated out of fold. In-sample GBM probabilities are not acceptable.

### Experiment 6: dense mapping experiments

Only after the neighborhood CNN demonstrates value, consider:

- sliding-window wall-to-wall center-pixel prediction;
- fully convolutional conversion of the patch classifier;
- sparse-loss U-Net;
- polygon-derived weak-supervision U-Net;
- GBM-to-U-Net knowledge distillation;
- U-Net refinement using GBM probabilities as input channels.

Each dense experiment must clearly state what its target represents.

---

## Cross-validation and leakage controls

### Fold hierarchy

Preferred hierarchy, from minimum to strongest:

1. polygon-held-out folds;
2. spatial-block folds;
3. acquisition-unit or project-area folds;
4. leave-landscape-out testing;
5. independent external test region.

### Patch-overlap leakage

Patch extraction occurs after fold assignment.

For a patch radius `r`, ensure that training patches do not include raster cells belonging to held-out validation or test zones where this would expose nearly identical neighborhoods.

Use spatial buffers around validation/test blocks when needed. The buffer should be at least half the patch width and may need to be larger based on ecological autocorrelation.

### Repeated samples within stands

Samples from the same polygon are correlated. Never treat them as independent when splitting folds or estimating uncertainty.

Where practical, summarize uncertainty using polygon-level or block-level resampling rather than point-level bootstrap samples.

### Preprocessing leakage

Fit all transformations using training data only, including:

- standardization;
- clipping thresholds informed by distributions;
- feature selection;
- imputation rules;
- class weights;
- probability calibration;
- dimensionality reduction.

Apply the fitted transformation unchanged to validation and test data.

---

## Patch extraction rules

Every extracted patch must retain metadata:

```text
sample_id
polygon_id
class_label
fold_id
landscape_id
x_coordinate
y_coordinate
crs
pixel_size
patch_size
channel_names
nodata_fraction
source_raster_version
```

Requirements:

- use a common grid, CRS, extent convention, resolution, and pixel alignment;
- document interpolation/resampling methods;
- use nodata masks explicitly;
- do not replace nodata with zero unless zero is physically meaningful and a mask channel is also provided;
- record whether patches cross polygon boundaries;
- consider distance-to-boundary as metadata for diagnostics;
- initially favor polygon interiors to reduce weak-label noise.

---

## Model implementation guidance

### Preferred framework

Use R for the current package and existing GBM workflow. PyTorch is a candidate for a later CNN component; any R/Python handoff must be explicit and reproducible.

### Initial CNN

Start small. A reasonable first architecture is:

```text
input H × W × C
-> Conv3×3 + normalization + activation
-> Conv3×3 + normalization + activation
-> optional downsampling
-> Conv3×3 + activation
-> global average pooling
-> dense classifier
-> softmax probabilities
```

Avoid an unnecessarily deep architecture on the first pass.

### Optional two-branch architecture

A later model may combine:

- a neighborhood branch using the full raster patch;
- a center-cell branch using raw center metrics;
- optional out-of-fold GBM probabilities;
- optional nonspatial stand or acquisition metadata, only when operationally available at prediction time.

### Losses

Begin with weighted cross-entropy if class imbalance is substantial.

Focal loss may be tested later but must not replace careful sampling, fold design, and class-level evaluation.

### Augmentation

Potentially valid spatial augmentations:

- 90-degree rotations;
- horizontal and vertical flips.

Use only if orientation is not ecologically meaningful. If slope aspect, scan direction, acquisition geometry, or north-south orientation matters, document and restrict augmentation accordingly.

Do not apply arbitrary pixel warping or intensity transformations without physical justification.

---

## Evaluation requirements

Report at minimum:

- balanced accuracy;
- macro F1;
- per-class precision, recall, and F1;
- confusion matrix;
- one-vs-rest ROC AUC or PR AUC when appropriate;
- log loss or Brier score for probability quality;
- calibration plots;
- performance by fold, landscape, polygon size, and boundary distance;
- uncertainty across spatial folds.

Do not rely on overall accuracy alone.

### Spatial quality diagnostics

For wall-to-wall predictions, inspect:

- salt-and-pepper behavior;
- improbable isolated classes;
- boundary effects;
- response to openings and retained trees;
- consistency within and across stands;
- uncertainty maps;
- disagreement maps between GBM and CNN.

Spatial smoothness is not automatically accuracy. Do not reward visually smooth predictions without independent evidence.

---

## Ablation requirements

The project must be able to answer what caused any improvement.

Recommended ablations:

- center-only GBM;
- GBM plus 3 × 3 means;
- center-only neural model;
- CNN with increasing patch sizes;
- CNN without selected channels;
- CNN with versus without GBM probabilities;
- CNN with versus without center-cell branch;
- interior-only versus all samples;
- normalized versus raw-valued channels where appropriate.

Change one major component at a time.

---

## Reproducibility

Every training run must record:

- git commit;
- config file;
- random seed;
- software environment;
- training and validation folds;
- input raster versions;
- class mapping;
- normalization parameters;
- architecture and parameter count;
- training history;
- selected checkpoint;
- final metrics;
- output paths.

Use deterministic settings where practical, while acknowledging that some GPU operations may remain nondeterministic.

---

## Coding standards

- prefer modular functions with explicit input validation;
- namespace all external function calls and document exported R functions with roxygen2;
- regenerate `NAMESPACE` and `man/` with roxygen2 rather than editing generated files;
- run `testthat::test_local()` and `R CMD check` for package changes;
- separate data preparation, training, prediction, and evaluation;
- avoid hidden state in notebooks;
- move reusable R logic into `R/` and document exported interfaces;
- include unit tests for patch alignment, fold assignment, class mapping, nodata handling, and prediction dimensions;
- fail loudly when CRS, resolution, channel order, or class mappings differ;
- never infer channel order from directory listing order;
- store channel definitions in configuration;
- add concise docstrings explaining scientific assumptions as well as software behavior.

---

## Communication standards

When reporting results, distinguish clearly among:

- predictive performance against stand-derived labels;
- spatial coherence;
- agreement with the existing GBM;
- ecological plausibility;
- true pixel-level accuracy, which cannot be claimed without pixel-scale reference data.

Use language such as:

> The CNN improved classification of sampled locations carrying stand-level labels under spatial cross-validation.

Avoid language such as:

> The CNN accurately mapped the true structural stage of every pixel.

---

## Immediate milestones

1. Recover the exact Erickson et al. reference; the original `dev/legacy/lynx_model.R` behavior is documented.
2. Inventory provider inputs, class definitions, predictor metadata, and available polygon/acquisition identifiers.
3. Define the common training-data contract and source-adapter boundary.
4. Maintain the initial R package and its composable workflow functions; document changes from legacy behavior.
5. Reproduce the original sampling, spatial clusters, CAST feature selection, and GBM results with recorded dependencies and realized folds.
6. Apply nested spatial CV for reportable results; audit polygon leakage and document the outer and inner fold geometry.
7. Demonstrate adding a second data source and running a versioned retraining workflow.
8. Revisit focal means and CNN experiments after the baseline and package interface are established.

---

## Current working class labels

`lynx_class_map()` preserves the mapping in `dev/legacy/lynx_model.R`:

| Source label | Modeling label |
| --- | --- |
| Multi-Story Foraging | `msf` |
| Multi-Story Non-foraging | `se` |
| Stem Exclusion | `se` |
| Stand Initiation | `si` |

Preserve this legacy mapping for reproduction, including the merge into `se`. Confirm its ecological rationale and authoritative definitions before treating it as the package-wide default. The script uses `factor()` without explicit levels; the package must record class order explicitly. No authoritative integer encoding has been established.
