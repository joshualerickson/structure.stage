# KNF Wildlife Steering Group briefing draft

## Purpose of the discussion

This discussion is about whether a LiDAR-derived structural-stage model can be
useful as a **supplemental screening and prioritization tool** for the Kootenai
National Forest (KNF). It is not a proposal to replace the KNF lynx habitat
layer, the National Forest Plan / NRLMD process, or project-level field review.

The first decision to make with the Steering Group is the intended operational
role. The defensible initial role is:

> Provide a repeatable, spatially explicit indication of likely stand structure
> from LiDAR metrics, identify places where it disagrees with the existing
> rule-based structural-stage layer, and prioritize where field review is most
> informative.

It should **not** be described as a compliance determination or as pixel-level
truth. The model is trained and evaluated against sample labels inherited from
mapped stands; those labels are not independent pixel-scale observations.

## Executive view

| Topic | Existing KNF rule-based product | LiDAR structural-stage model | Appropriate relationship |
| --- | --- | --- | --- |
| Primary basis | VMap structure classes, habitat-type / PVT rules, elevation, and disturbance updates | Quantitative LiDAR measures of canopy height, vertical structure, and connectivity | Use the existing layer to identify potential habitat and regulatory context; use LiDAR to supply additional structural evidence. |
| Spatial representation | Polygon / rule output based on mapped attributes and defined thresholds | Fine-grain predictions from a fitted three-class GBM | Compare predictions and flag disagreement; do not silently substitute one map for the other. |
| Strength | Transparent connection to the established KNF mapping process and habitat rules | Can recognize nonlinear combinations of observed vegetation structure that simple age, DBH, or canopy-class rules can miss | The approaches answer related but different questions and should be documented together. |
| Known limitation | Structural-stage rules depend in part on VMap tree-size / DBH categories and attribute currency | Depends on LiDAR acquisition date, training-label provenance, domain coverage, and spatial validation | Both require field review for proposed actions and explicit data-currency procedures. |
| Field verification | Required for project-level planning under existing KNF documentation | Cannot replace field verification | Use model agreement/disagreement to focus reconnaissance and QA. |

The existing 2020 KNF process document itself says the broad-scale map uses
remotely sensed inputs, calls project-level ground verification imperative, and
states that the map should be refreshed every other year for Forest Plan
monitoring, with annual updates when major fire conditions warrant them. That is
the operational standard the LiDAR work must fit within.

## How the LiDAR model works

This is **not a deep-learning model**. It is a gradient-boosted decision-tree
model (GBM): an ensemble of many simple, transparent split rules that combine
quantitative LiDAR measurements. The saved final model uses 150 shallow trees
(tree depth 1). Each tree adds a small amount of evidence for the three possible
structural-stage classes; their combined output is a probability for MSF, SE,
and SI. The reported class is the largest probability, while entropy and the
probability margin identify uncertain calls.

```text
LiDAR point cloud (1 m source data)
        |
        v
30 m structural metrics: height + low/mid canopy layering + connectivity
        |
        v
Field / stand-derived structural-stage labels at sampled locations
        |
        v
Reportable workflow: spatially separated model development and assessment
  - keep assessment areas out of a fit
  - select / tune only using training-side data
        |
        v
Final five-metric GBM --> P(MSF), P(SE), P(SI), hard class, uncertainty
        |
        v
Compare to existing rule layer + disturbance history --> field-review priority
```

The conceptual difference from a rule model is straightforward. A rule model
applies a prescribed threshold or category—such as a VMap tree-size / DBH class,
canopy class, age, habitat type, elevation, or recent disturbance—to every
polygon. The GBM instead learns which combinations of measured structure are
associated with the observed stand labels. It can learn curved or threshold-like
responses without assuming that a single DBH category is the structural stage.
It remains bounded by its input data: it cannot know a post-LiDAR harvest or fire,
and it should not be extrapolated to unrepresented forest conditions.

## Why this is a useful methodological test

The aim is not to declare all rule-based mapping invalid. It tests a narrower,
operational question: **does directly measured three-dimensional vegetation
structure add useful information beyond generalized vegetation attributes for
assigning the observed stand stage?**

Several choices reduce avoidable overfitting and make the result inspectable:

- The prediction inputs are structural LiDAR measurements. Abiotic columns such
  as temperature, soils, slope, and potential-vegetation attributes were excluded
  from the final five-variable model, so the fitted stage signal is not being
  carried primarily by broad environmental proxies.
- Class probabilities, uncertainty, confusion patterns, and class-specific
  recall are retained. The output is more informative than a single hard class.
- The reportable workflow uses supplied spatial splits rather than random
  row-wise splitting. Spatial separation is necessary because nearby LiDAR cells
  and repeated points within a stand are correlated. The geometry and
  polygon-isolation status of the ten splits in the saved final-model artifact
  still need to be audited before describing them as spatially independent.
- The package workflow can carry out feature selection and tuning inside spatial
  training folds and persist the realized splits and predictions. This prevents
  choosing variables on the same held-out records used to advertise performance.
- Feature-group ablation asks whether the structural metrics matter in practice.
  In the current held-out diagnostic, removing understory structure reduced
  broad skill and MSF/SE recall; removing the midstory/vertical group severely
  reduced SI recall. This makes the added variables testable rather than a
  black-box assertion.

The limitations are equally important: the labels are qualitative, stand-derived
labels; multiple samples within a stand are correlated; and the training table
currently lacks the source metadata needed to independently audit survey timing,
observer effects, and polygon-level isolation. These are reasons to strengthen
the evidence package, not reasons to reinterpret resampling results as field
truth.

## Reading model-performance terms

All of these statistics compare a prediction with the **sample's stand-derived
label**. They do not measure pixel-level truth, and a single statistic cannot
tell the whole story for a three-class model.

For each class, such as MSF, we temporarily ask a binary question: “was this
record MSF or not MSF?” The same calculation is then repeated for SE and SI.

| Term | Plain-language question | Calculation for MSF | Why it is useful / limited |
| --- | --- | --- | --- |
| Overall accuracy | “How often was the hard class correct?” | all correct calls / all calls | Easy to read but can be dominated by the commonest class. It should never stand alone. |
| Sensitivity / recall | “Of records labeled MSF, how many did the model call MSF?” | true MSF calls / all observed MSF records | Directly answers whether a class is being missed. A false negative MSF call goes to SE or SI. |
| Specificity | “Of records not labeled MSF, how many did the model avoid calling MSF?” | correctly rejected non-MSF / all observed non-MSF records | Measures false-positive control. In a three-class setting it can look high simply because there are many non-MSF records, so it must be read with recall and precision. |
| Precision | “When the model calls MSF, how often is the observed label MSF?” | true MSF calls / all MSF predictions | Describes the reliability of an MSF flag. It falls when SE or SI are incorrectly called MSF. |
| F1 | “Is there a reasonable balance between finding MSF and avoiding false MSF calls?” | harmonic mean of MSF precision and recall | Useful single class summary, but it does not measure probability calibration. |
| Macro recall | “On average, how often are the three classes found?” | mean of MSF, SE, and SI recall | Gives every class equal weight. In many multiclass contexts this is called class-balanced accuracy. |
| One-vs-rest balanced accuracy | “For each class, did the model both find it and reject the others?” | (MSF sensitivity + MSF specificity) / 2; then average across classes | This is the definition currently returned by the package's nested-CV metric table. It includes specificity, so it is normally higher than macro recall in a three-class problem. Label it precisely. |
| Log loss | “How much probability did the model assign to the observed class?” | average negative log of the observed-class probability | Uses the full probability vector. Lower is better; it penalizes confident wrong calls more than uncertain wrong calls. |

For example, if 100 field-labeled MSF samples are assessed and the model correctly
calls 70 MSF, its MSF sensitivity is 70%. If it calls MSF for 10 of 200 non-MSF
samples, its MSF specificity is 95%. Those values can coexist: the model may be
conservative about issuing MSF while still missing 30% of actual MSF.

### Current naming decision

For future briefing figures and reports, use both **macro recall** and **mean
one-vs-rest balanced accuracy**, rather than using “balanced accuracy” alone.
The package's nested-CV summary currently defines balanced accuracy as the mean
of class-specific `(sensitivity + specificity) / 2`. The feature-group ablation
script currently labels the mean of the three recalls as balanced accuracy. The
underlying calculations are both legitimate summaries, but they are not the same
number. Rename the ablation metric to `macro_recall` before using it in a formal
comparison; retain class-specific recall, precision, F1, and log loss alongside
it.

## KNF geographically held-out validation comparison

### Scope and class crosswalk

This is a geographically held-out comparison of the KNF rule layer and the
LiDAR GBM against the recorded `Lynx_Field` call at 1,207 survey points. The
survey-point layer and the joined LiDAR-result layer match exactly by `GlobalID`
(1,207 shared IDs). The project owner confirmed that neither these points nor
their broader project area were used during LiDAR-model training. The three-class
analysis retains 528 points with all of the following:

- a field call of Multistory, Stem Exclusion, or Stand Initiation;
- a corresponding three-class rule-model call; and
- a non-missing LiDAR GBM prediction.

The crosswalk is `Multistory -> msf`, `Stem Exclusion -> se`, and `Stand
Initiation -> si`. The model-training crosswalk retains the established Flathead
National Forest decision that Multi-Story Non-foraging maps to `se`, together
with Stem Exclusion. Non-habitat, Early Stand Initiation, Other, Not Modeled,
and missing calls are outside this three-class comparison.

The joined layer includes a `review` field. Its available comments show that
`review = TRUE` commonly means a desk review based on “IL’s notes/photos,” rather
than the same type of on-site call recorded in this layer. The results therefore
show all eligible holdout points plus explicit `review = TRUE` and `review =
FALSE` subgroups. Review status is an evidence-provenance stratum, not an
automatic exclusion or a reason to disregard the geographic holdout comparison.
Preserve the survey/review protocol with the results so users can assess the
strength and limits of each stratum.

### Equations for one class at a time

For a given class, say SI, every point is counted once in a one-versus-rest
table:

| | Predicted SI | Predicted not-SI |
| --- | ---: | ---: |
| Field-labeled SI | true positive (TP) | false negative (FN) |
| Field-labeled not-SI | false positive (FP) | true negative (TN) |

| Statistic | Equation | Meaning |
| --- | --- | --- |
| Sensitivity / recall | `TP / (TP + FN)` | Share of field-labeled records of that class that the model finds. |
| Specificity | `TN / (TN + FP)` | Share of all other field-labeled records that the model does not incorrectly call that class. |
| Precision | `TP / (TP + FP)` | Share of calls made as that class that agree with the field label. |
| F1 | `2 × precision × recall / (precision + recall)` | Balance between detecting a class and avoiding false calls of it. |
| One-vs-rest balanced accuracy for the class | `(sensitivity + specificity) / 2` | Gives equal weight to finding the class and rejecting the other two classes. |
| Overall accuracy | `(TP_msf + TP_se + TP_si) / N` | Share of all hard class calls that agree with the field label. |
| Macro recall | `(recall_msf + recall_se + recall_si) / 3` | Average class-finding rate, giving MSF, SE, and SI equal weight. |
| Mean one-vs-rest balanced accuracy | `(BA_msf + BA_se + BA_si) / 3` | Average of the three class-specific balanced-accuracy values. |

For the all-eligible LiDAR result, SI provides a concrete example. There are 53
field-labeled SI points. The LiDAR GBM calls 29 of them SI (`TP = 29`) and misses
24 (`FN = 24`), so SI sensitivity is `29 / 53 = 54.7%`. It calls 90 points SI;
61 of those are not field-labeled SI (`FP = 61`). Of the 475 non-SI points, 414
are correctly not called SI (`TN = 414`), so specificity is `414 / 475 =
87.2%`. SI balanced accuracy is therefore `(54.7% + 87.2%) / 2 = 70.9%`.

Specificity is usually higher than recall here because each class is compared
with the other two combined. That is why macro recall and the class-specific
tables are necessary alongside mean one-vs-rest balanced accuracy.

### Overall results

`review_not_recorded` contains only six eligible points and is not shown below;
its metrics are too unstable for interpretation.

| Review tier | N | Model | Overall accuracy | Macro recall | Mean one-vs-rest balanced accuracy |
| --- | ---: | --- | ---: | ---: | ---: |
| All eligible | 528 | KNF rule model | 47.3% | 41.3% | 55.4% |
| All eligible | 528 | LiDAR GBM | 52.7% | 56.0% | 66.2% |
| Review not flagged (`review = FALSE`) | 284 | KNF rule model | 55.3% | 44.2% | 58.3% |
| Review not flagged (`review = FALSE`) | 284 | LiDAR GBM | 51.4% | 56.2% | 66.1% |
| Desk review flagged (`review = TRUE`) | 238 | KNF rule model | 37.0% | 40.0% | 53.5% |
| Desk review flagged (`review = TRUE`) | 238 | LiDAR GBM | 53.8% | 54.2% | 65.4% |

The LiDAR model has higher all-point accuracy, macro recall, and mean
one-vs-rest balanced accuracy. In the `review = FALSE` subset, the rule model
has a 3.9-point higher overall accuracy, whereas the LiDAR model finds the three
classes more evenly: macro recall is 56.2% versus 44.2%. This is exactly why a
single accuracy value would be misleading.

### All eligible points: class-level results

| Field class | Support | Model | Sensitivity / recall | Specificity | Precision | F1 | One-vs-rest balanced accuracy |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| MSF | 93 | KNF rule model | 64.5% | 52.9% | 22.6% | 33.5% | 58.7% |
| MSF | 93 | LiDAR GBM | 63.4% | 63.7% | 27.2% | 38.1% | 63.6% |
| SE | 382 | KNF rule model | 48.2% | 58.2% | 75.1% | 58.7% | 53.2% |
| SE | 382 | LiDAR GBM | 49.7% | 78.8% | 86.0% | 63.0% | 64.3% |
| SI | 53 | KNF rule model | 11.3% | 97.5% | 33.3% | 16.9% | 54.4% |
| SI | 53 | LiDAR GBM | 54.7% | 87.2% | 32.2% | 40.6% | 70.9% |

The rule model rarely labels a point SI, yielding very high SI specificity but
only 11.3% SI sensitivity. The LiDAR GBM increases SI sensitivity to 54.7%; it
also makes more SI calls, so precision remains modest. That is a useful field-QA
pattern: LiDAR SI calls should be inspected, not automatically accepted, but
the model is much less likely to leave field-labeled SI unflagged.

For MSF, both approaches find roughly two-thirds of field-labeled MSF points,
but each has low precision because many points called MSF in this restricted
comparison are field-labeled SE or SI. For SE, LiDAR has slightly higher recall
and markedly better specificity and precision.

### Review not flagged (`review = FALSE`): class-level results

| Field class | Support | Model | Sensitivity / recall | Specificity | Precision | F1 | One-vs-rest balanced accuracy |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| MSF | 65 | KNF rule model | 61.5% | 63.5% | 33.3% | 43.2% | 62.5% |
| MSF | 65 | LiDAR GBM | 66.2% | 59.4% | 32.6% | 43.7% | 62.8% |
| SE | 196 | KNF rule model | 58.2% | 56.8% | 75.0% | 65.5% | 57.5% |
| SE | 196 | LiDAR GBM | 45.9% | 80.7% | 84.1% | 59.4% | 63.3% |
| SI | 23 | KNF rule model | 13.0% | 96.6% | 25.0% | 17.1% | 54.8% |
| SI | 23 | LiDAR GBM | 56.5% | 87.7% | 28.9% | 38.2% | 72.1% |

In this subset, the rule model achieves its higher overall accuracy largely from
SE performance and class prevalence, but still finds only 3 of 23 field-labeled
SI points. LiDAR finds 13 of 23 SI points, at the cost of more false SI calls.

### Desk review flagged (`review = TRUE`): class-level results

| Field class | Support | Model | Sensitivity / recall | Specificity | Precision | F1 | One-vs-rest balanced accuracy |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| MSF | 27 | KNF rule model | 74.1% | 40.8% | 13.8% | 23.3% | 57.4% |
| MSF | 27 | LiDAR GBM | 55.6% | 68.2% | 18.3% | 27.5% | 61.9% |
| SE | 181 | KNF rule model | 35.9% | 61.4% | 74.7% | 48.5% | 48.7% |
| SE | 181 | LiDAR GBM | 53.6% | 75.4% | 87.4% | 66.4% | 64.5% |
| SI | 30 | KNF rule model | 10.0% | 98.6% | 50.0% | 16.7% | 54.3% |
| SI | 30 | LiDAR GBM | 53.3% | 86.1% | 35.6% | 42.7% | 69.7% |

The desk-review-flagged subset is where LiDAR has the largest overall advantage.
That pattern should prompt a data and protocol review: it may reflect genuinely
harder locations, map-age or disturbance issues, differences between on-site and
desk review, or an issue with either map. It cannot establish that one method is
universally superior without an independent, documented validation design.

## Where the two model products disagree

The next diagnostic asks a more useful question than “which model has a higher
score?”: **when the rule layer and LiDAR GBM disagree, which one agrees with the
recorded field call, how certain is the LiDAR model, and what measured structure
characterizes the disagreement?**

The table below uses the same 528 eligible gut-check points. “Correct” means
agreement with the recorded three-class field call. Entropy is normalized from
zero to one; a higher value means LiDAR assigned more similar probabilities to
MSF, SE, and SI. Margin is the highest probability minus the second-highest;
a lower value means the leading LiDAR call was less decisive.

| Comparison outcome | N | Median LiDAR confidence | Median entropy | Median probability margin | Interpretation |
| --- | ---: | ---: | ---: | ---: | --- |
| Both correct | 142 | 0.60 | 0.80 | 0.31 | Both methods match the recorded field call. |
| LiDAR correct; rule wrong | 136 | 0.60 | 0.80 | 0.33 | Primary set for identifying structural information added by LiDAR. |
| Rule correct; LiDAR wrong | 108 | 0.54 | 0.83 | 0.21 | Primary set for LiDAR failure-mode review; LiDAR calls are comparatively less decisive. |
| Both wrong; same call | 101 | 0.57 | 0.83 | 0.26 | Possible shared limitation, label ambiguity, stale inputs, or a class not well represented by either approach. |
| Both wrong; different calls | 41 | 0.66 | 0.77 | 0.41 | Highest-priority QA cases because neither product agrees with the field call and the products also disagree. |

Two useful operational implications follow. First, the “Rule correct; LiDAR
wrong” group has the lowest median confidence and margin and the highest median entropy.
LiDAR entropy and margin are therefore defensible tools for prioritizing
uncertain LiDAR outputs for review. Second, the 41 cases where both products are
wrong but disagree are not necessarily low-confidence; they need imagery,
disturbance history, comments, and field review rather than an automatic
confidence-based dismissal.

### Important directional transitions

| Recorded field stage | Rule-model call -> LiDAR call | N | Median LiDAR confidence | Median margin | Diagnostic meaning |
| --- | --- | ---: | ---: | ---: | --- |
| SE | MSF -> SE | 86 | 0.56 | 0.25 | LiDAR agrees with the field SE call and corrects a rule-model MSF call. This is the largest LiDAR-added agreement group. |
| SI | SE -> SI | 17 | 0.84 | 0.73 | LiDAR agrees with the field SI call with strong separation from its second choice; a key set for reviewing the structural signal behind SI. |
| SE | MSF -> SI | 25 | 0.73 | 0.56 | LiDAR is confidently wrong relative to the recorded SE call. Review for genuine SI-like structure, timing mismatch, disturbance, label ambiguity, or model limitation. |
| SE | SE -> MSF | 65 | 0.53 | 0.17 | LiDAR is wrong relative to recorded SE, but usually uncertain. This is an efficient uncertainty-led field-review group. |

These counts are not causal evidence that a specific metric “caused” a class.
They identify the records and conditions where direct structural measurements
appear to improve or complicate the rule-based call in this held-out area.

### Structural-metric profiles of disagreement groups

| Comparison outcome | Median zmax (m) | Median low-mid stratum count | Median count >6.1 m | Median midstory degree | Median understory betweenness |
| --- | ---: | ---: | ---: | ---: | ---: |
| Both correct | 28.7 | 8.6 | 33.2 | 3.40 | 311.6 |
| LiDAR correct; rule wrong | 29.4 | 10.9 | 36.1 | 3.43 | 352.6 |
| Rule correct; LiDAR wrong | 29.9 | 15.4 | 40.6 | 3.44 | 503.0 |
| Both wrong; same call | 33.8 | 10.8 | 33.5 | 3.35 | 421.1 |
| Both wrong; different calls | 27.7 | 64.1 | 76.0 | 3.37 | 602.2 |

The 41 “both wrong; different calls” points are notably dense and connected in
the selected vertical/understory metrics: low-mid stratum counts, counts above
6.1 m, and understory betweenness are much higher than the both-correct group.
This supports treating them as a distinct ecological or data-QA population for
review. It does **not** establish a universal threshold or a causal mechanism.

### Presentation figures

- [Comparison outcomes](/home/josh.erickson/Documents/projects/structure.stage/outputs/gutcheck_disagreement/figures/all_eligible_comparison_outcomes.png) shows the five agreement/disagreement groups.
- [Paired confusion matrices](/home/josh.erickson/Documents/projects/structure.stage/outputs/gutcheck_disagreement/figures/all_eligible_comparison_confusion.png) show how each model distributes calls within each recorded field stage.
- [Directional transitions](/home/josh.erickson/Documents/projects/structure.stage/outputs/gutcheck_disagreement/figures/all_eligible_prediction_transitions.png) shows every rule-to-LiDAR change, separated by recorded field stage.
- [LiDAR uncertainty by outcome](/home/josh.erickson/Documents/projects/structure.stage/outputs/gutcheck_disagreement/figures/all_eligible_lidar_uncertainty.png) shows confidence, entropy, and top-two probability margin.
- [Structural metrics by outcome](/home/josh.erickson/Documents/projects/structure.stage/outputs/gutcheck_disagreement/figures/all_eligible_structural_metrics.png) shows the distributions of the five selected LiDAR predictors.

Recreate the figures with:

```r
source("dev/gutcheck_validation_record.R")
source("dev/gutcheck_disagreement_analysis.R")

audit <- analyze_knf_gutcheck_disagreement()
save_gutcheck_disagreement_figures(audit)
```

Use `tier = "review_not_flagged"` or `tier = "desk_review_flagged"` in
`analyze_knf_gutcheck_disagreement()` to generate the same figure set for each
review subset.

## What has been built

The R workflow is now an auditable, reproducible package rather than a single
interactive script. It records the selected predictors, model settings, random
seeds, training-data version, spatial folds, held-out predictions, and software
versions. New data are added through a documented data contract and trigger a
new full retraining run; the model is not incrementally altered without a new
recorded run.

The current final GBM uses five LiDAR-derived predictors:

| Predictor | Plain-language interpretation |
| --- | --- |
| `zmax` | Maximum local vegetation height |
| `n_gt_6_1` | Count / density measure of vegetation above the project's 6.1-unit threshold |
| `n_strata_low_mid` | Amount of low-to-mid vertical layering |
| `midstory_mean_degree` | Midstory connectivity / structural-complexity measure |
| `understory_mean_betweenness` | Understory connectivity / structural-pathway measure |

The exact units, raster resolution, extraction method, and LiDAR acquisition
date must accompany any formal map or performance statement. The field names
above are not sufficient metadata by themselves.

### Current evidence inventory

The following facts have been verified from the available files. They are the
right level of specificity for the briefing; do not generalize the dates or
coverage beyond these records.

| Item | Verified information | Implication |
| --- | --- | --- |
| Current prediction raster | `NR_lidar_structure_stage_30m_8826.tif`; 30 m cells; NAD83 / Idaho Transverse Mercator (EPSG:8826); six bands: `msf`, `se`, and `si` probabilities, hard `structure_stage`, entropy, and probability margin | The delivered product supports both a class map and an uncertainty / disagreement review. It is a prediction product, not a field-observed truth map. |
| LiDAR coverage metadata | `NR_lidar_polys_merged_epsg5070.gpkg`; 30 acquisition polygons, with project area, acquisition year, and quality level | The data are a multi-acquisition mosaic. Each mapped area must retain its acquisition year and quality level in the map metadata. |
| Lincoln / KNF-area acquisition visible in that metadata | `MT_Lincoln` and `MT_Lincoln2`, both 2019 and QL1 | This supports the stated 2019 date for the Lincoln County area represented in the coverage file. |
| 2024 update | Reported as available by the project owner, but no 2024 acquisition record appears in the supplied coverage GeoPackage | Obtain the 2024 footprint, metadata, and derived metric version before stating that the current 30 m raster incorporates it. A 2024 acquisition can support a future refreshed run. |
| Training table | `lynx_df.csv`; 30,170 records before class filtering; 102 columns, including raw and focal-summary LiDAR metrics, a source stage label, and coordinates | This source table does not itself retain stable sample ID, polygon ID, source/acquisition ID, survey date, observer, or field-QA fields. Those provenance records must be recovered separately for a complete audit. |
| Final five-variable model | `gbm_mod_five_vars_no_focal.rds`; caret GBM trained on 5,652 balanced sampled records: 1,997 MSF, 1,996 SE, 1,659 SI. Final tuning: 150 trees, depth 1, shrinkage 0.1, minimum node size 10 | The artifact preserves the final fitted model, tuning results, supplied resampling predictions, and predictor set. |
| Stored resampling evidence | Ten supplied assessment splits (`Resample01` through `Resample10`), with one held-out prediction per retained training record at the selected tuning setting | This is useful internal spatial-resampling evidence if the realized fold geometry is documented. It is not an external or independent accuracy assessment. The stored `repeatedcv` label says five repeats, but the custom index contains only ten splits; do not describe this artifact as 10 × 5 repeated CV. |

The GeoPackage also records other acquisitions from 2010 through 2023 and both
QL1 and QL2. Therefore, the raster must not be described simply as “2019 LiDAR”
outside the Lincoln / KNF-area footprint. A map legend, data dictionary, or
companion coverage layer should show acquisition year and QL for every predicted
area.

The intended training and assessment design matters. The package supports nested
spatial CV: feature selection and GBM tuning occur inside the training portion
of each outer spatial fold, and each assessment sample receives a prediction
from a model that did not train on that fold. This avoids reporting a
resubstitution score as accuracy. The saved five-variable model demonstrates the
final model and its ten supplied resamples; it does not by itself document a
complete nested outer-CV run. For a reportable accuracy statement, retain and
present the `run_mscv_gbm()` result, realized polygon/fold audit, and outer
held-out predictions. Any such estimate still applies only to sampled locations
carrying stand-derived labels and landscapes represented by the training data.

The current diagnostic work also tests whether the added structure metrics carry
useful signal. On the present held-out evaluation set, refitting without the
understory variable lowered balanced accuracy and macro F1 by about 0.06 and
reduced MSF and SE recall. Removing the three midstory/vertical variables lowered
balanced accuracy by about 0.18, macro F1 by about 0.13, and SI recall by about
0.50. Those are useful screening results, not a claim of ecological causation;
they should be repeated across the planned spatial folds before use in formal
comparison material.

## How the model can improve reconnaissance

The efficiency claim should be specific and modest. The model does not remove a
need to visit a proposed treatment stand. It can help a team decide **where to
look first and what to check**:

1. Map agreement between the rule-based stage and LiDAR stage. Agreement can be
   lower priority for structural-stage review, subject to normal project needs.
2. Map disagreement and model uncertainty. Those locations are candidates for
   earlier field review, image review, or data QA.
3. Flag locations where a rule-based stem-exclusion call has LiDAR structure more
   consistent with multi-story conditions, and the converse. The flag is a
   prompt to inspect vertical layering, understory cover, retained trees,
   disturbance history, and map currency.
4. Use recent disturbance overlays to identify where old LiDAR is no longer a
   credible representation of current structure and where no prediction should
   be relied on without updated data.

This is a triage / QA workflow, not an automated approval or exclusion rule.
For a treatment proposal, field findings remain the information used to establish
the applicable structural condition and compliance determination.

## Proposed coexistence and governance

Before operational use, prepare a short jointly reviewed use-and-limitations
memo. It should state:

- the official / decision-support status of each layer;
- that neither map replaces field verification for proposed actions;
- the LiDAR acquisition date, coverage, resolution, predictor definitions, and
  data version for each run;
- model training-label source, class definitions, exclusions, and geographic
  domain;
- the spatial validation design and reported metrics, including uncertainty by
  class and landscape;
- how disagreements, low-confidence predictions, fire, harvest, and stale LiDAR
  are handled;
- who may run, distribute, interpret, and update the model;
- a versioned archive of the input data, model, outputs, and change log.

This protects against the appearance of competing, undocumented models. It makes
the LiDAR output a transparent supplemental evidence layer whose role can be
reviewed by biologists, silviculturists, fuels staff, GIS staff, and planners.

## Questions to resolve before the meeting

### LiDAR and operational currency

1. What are the acquisition date(s), vendor / project name(s), point density,
   processing version(s), spatial coverage, and raster resolution for every
   LiDAR source used in the current model and the 2/2/26 and 9/17/26 maps?
2. Are the predictors built from one acquisition or a mosaic? If a mosaic, where
   are date boundaries and how are acquisition differences handled?
3. Is there a KNF or Region 1 schedule, budget, or trigger for new LiDAR? If
   there is no recurring acquisition schedule, what updates are feasible between
   acquisitions (for example, disturbance masks, no-prediction masks, or
   targeted reacquisition)?
4. Which disturbance datasets will be authoritative, how quickly are they
   refreshed, and what disturbance magnitude/date makes a LiDAR prediction stale?

### Training and field-validation provenance

5. Please provide the source table / geodatabase for every field-validation
   record used, with stable sample and polygon IDs, project area, survey date,
   observer or team identifier where permitted, field protocol / form version,
   original class, final harmonized class, and any QA status.
6. Which KNF project areas and landscapes occur in training, tuning, held-out
   assessment, and any map demonstration? Provide counts of stands and samples
   by class, project, survey year, and landscape.
7. What screening was applied before model fitting: duplicate removal, boundary
   exclusion, ambiguous-label exclusion, field-protocol consistency checks,
   observer review, timing relative to LiDAR, and any class balancing?
8. How do field labels correspond to the three modeled classes? In particular,
   confirm the rationale for combining the legacy Multi-Story Non-foraging and
   Stem Exclusion labels into `se` for the current model.
9. Which experienced KNF lynx biologists have reviewed whether the represented
   areas and labels cover the range of Salish and Purcell conditions? What
   geographic or ecological settings remain out of domain?

### Comparison with the existing layer

10. Identify the exact existing-layer version used in every comparison map,
    including VMap vintage, disturbance update date, and rule/toolbox version.
11. Provide the comparison table or GIS join used to calculate the existing
    model's accuracy: matched record IDs, class crosswalk, inclusion/exclusion
    rules, timing relative to field survey and VMap/LiDAR, and whether records
    were independent of LiDAR training.
12. Confirm whether any record used to train the LiDAR model was also used to
    score either model. A fair head-to-head comparison needs a common held-out,
    spatially independent field-validation set that was not used for LiDAR
    selection, tuning, or fitting.
13. Define the intended comparison estimand: agreement with field-assigned stand
    structure, agreement with the existing rule layer, or a project-screening
 utility measure. These answer different questions and should not be mixed.

### Gut-check comparison

Treat the gut-check comparison as a **map-review diagnostic**, not as the
reported accuracy study, unless all of the following can be supplied: its file
location; reviewer, review date, and qualifications; a pre-defined review
protocol; the exact map versions; stable locations / polygon IDs; and proof that
the reviewed records were excluded from model fitting, feature selection, and
tuning. Records marked “Ira” or “need review” should be excluded from any
professional field-validation subset. The practical value of this exercise is to
find implausible outputs, stale disturbance areas, and labels or source data that
need review; it does not establish comparative accuracy.

## Meeting agenda

1. Confirm the question and the model's non-decision role (10 min).
2. Walk through the existing KNF process and the LiDAR workflow side by side
   (15 min).
3. Review LiDAR, field-label, and map-version provenance (20 min).
4. Review spatially held-out performance and class-level errors only after the
   data lineage is confirmed (20 min).
5. Review maps of agreement, disagreement, uncertainty, recent disturbance, and
   field-recon outcomes for one or two candidate areas (20 min).
6. Agree on a pilot, the use-and-limitations memo, data-update triggers, and
   biologist review checkpoints (15 min).

## Statements to avoid

- “The LiDAR model determines compliance.”
- “The model gives pixel truth.”
- “The LiDAR model is more accurate than the existing model” until both are
  scored against the same independent, spatially held-out field dataset with a
  documented class crosswalk.
- “The model updates itself after fire or harvest.”
- “A high-confidence prediction removes the need for field verification.”
