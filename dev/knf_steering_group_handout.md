# LiDAR structural-stage mapping: supplemental decision support

## What this is

This work tests whether **directly measured vegetation structure from LiDAR**
can add useful information to the existing KNF rule-based structural-stage map.
It is a supplemental screening, map-QA, and reconnaissance-prioritization tool.

It does **not** replace the KNF lynx habitat layer, NRLMD / Forest Plan
requirements, or project-level field verification.

## Why test a LiDAR model?

The existing workflow assigns structural stage from mapped vegetation attributes,
habitat rules, and disturbance information. The LiDAR model instead uses measured
30 m vegetation structure: maximum height, low-to-mid layering, vegetation above
6.1 m, and midstory/understory connectivity. It produces probabilities for
Multi-Story Foraging (MSF), Stem Exclusion (SE), and Stand Initiation (SI), plus
uncertainty.

This makes the same product useful at two connected scales. At the **fine scale**,
30 m class probabilities, entropy, and probability margin can show within-stand
variation and flag locations where a proposed treatment area needs earlier field
review. At the **broad scale**, classes, probabilities, uncertainty, and
rule-versus-LiDAR disagreement can be summarized by project area, LAU, or other
planning unit to identify where map currency and structural-stage assumptions are
most consequential.

Field verification remains the decision standard for a proposed action. It also
improves the system: reviewed observations can be retained with their date,
observer/reviewer, source, and LiDAR version; used to evaluate disagreement and
uncertainty; and, after QA, added to a versioned future training and retraining
run. A field finding does not silently alter the existing model or map. Recent
fire, harvest, or other disturbance should similarly trigger review or a
no-prediction flag when the LiDAR acquisition no longer represents current
structure.

## What the current geographically held-out comparison shows

At 528 geographically independent holdout points with a recorded three-class
field call and predictions from both products, LiDAR had higher balanced accuracy.
Neither these points nor their broader project
area were used to train the LiDAR model, making this an external geographic
validation comparison. The `review` flag is retained as a transparent evidence
stratum because some records were reviewed from notes/photos rather than the
same type of on-site record.

Class-specific one-vs-rest balanced accuracy is `(sensitivity + specificity) / 2`.
It gives equal weight to two operational errors: failing to identify a field-labeled
class (a false negative) and incorrectly calling another location that class (a
false positive). A model can appear strong on only one of these measures; balanced
accuracy requires both.

For each class separately (for example, SI versus not SI), every point falls in
one cell of this two-by-two confusion matrix:

| Recorded field class | Model calls the class | Model calls another class |
| --- | --- | --- |
| **Class is present** | True positive (TP) | False negative (FN) |
| **Class is absent** | False positive (FP) | True negative (TN) |

`sensitivity = TP / (TP + FN)` measures how often the model finds the class
when the field call is that class. `specificity = TN / (TN + FP)` measures how
often it avoids calling that class elsewhere. `balanced accuracy = (sensitivity
+ specificity) / 2`. Thus, balanced accuracy rewards finding a stage and
avoiding overcalling it; a high score needs both.

| Validation stratum | Model | N | MSF balanced accuracy | SE balanced accuracy | SI balanced accuracy | Mean balanced accuracy |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| All eligible holdout points | KNF rule model | 528 | 58.7% | 53.2% | 54.4% | 55.4% |
| All eligible holdout points | LiDAR GBM | 528 | 63.6% | 64.3% | 70.9% | 66.2% |
| `review = FALSE` | KNF rule model | 284 | 62.5% | 57.5% | 54.8% | 58.3% |
| `review = FALSE` | LiDAR GBM | 284 | 62.8% | 63.3% | 72.1% | 66.1% |
| `review = TRUE` | KNF rule model | 238 | 57.4% | 48.7% | 54.3% | 53.5% |
| `review = TRUE` | LiDAR GBM | 238 | 61.9% | 64.5% | 69.7% | 65.4% |

LiDAR has higher balanced accuracy for MSF, SE, and SI in both review strata.
Its largest advantage is SI: 70.9% versus 54.4% across all eligible holdout
points. The review strata are not exclusions; they make the source of the field
call transparent.

![Field-stage calls versus predictions](../outputs/gutcheck_disagreement/figures/all_eligible_comparison_confusion.png)

## How this can help field reconnaissance

1. Compare the rule stage and LiDAR stage.
2. Prioritize locations where the maps disagree, LiDAR uncertainty is high, or
   recent disturbance makes the LiDAR vintage unreliable.
3. Review imagery, disturbance history, stand context, and field conditions.
4. Use field findings for project decisions and update the record of known map
   limitations.

The table below shows where the two products agree with the recorded field call.

| Comparison outcome | Points |
| --- | ---: |
| Both products correct | 142 |
| LiDAR correct; rule model wrong | 136 |
| Rule model correct; LiDAR wrong | 108 |
| Both wrong; same call | 101 |
| Both wrong; different calls | 41 |

![Agreement and disagreement outcomes](../outputs/gutcheck_disagreement/figures/all_eligible_comparison_outcomes.png)

## Where the two model products disagree

At the same 528 eligible holdout points, this comparison asks: when the rule
layer and LiDAR GBM disagree, which product agrees with the recorded field call,
how certain is the LiDAR prediction, and what measured structure characterizes
the disagreement? “Correct” means agreement with the recorded three-class field
call. LiDAR entropy ranges from zero to one; higher values mean its three class
probabilities were more similar. Probability margin is the highest probability
minus the second-highest; lower values mean a less decisive LiDAR call.

For a LiDAR probability vector `p = (p_MSF, p_SE, p_SI)`:

- **Confidence** is the probability of the predicted class: `max(p_MSF, p_SE, p_SI)`.
  A confidence of 0.80 means the model assigned 80% probability to its leading
  class.
- **Normalized entropy** summarizes how evenly probability is spread across all
  three classes: `- [sum(p_i * log(p_i))] / log(3)`. It is near 0 when one class
  dominates and equals 1 when all three classes have probability 1/3.
- **Probability margin** is the separation between the first- and second-ranked
  classes: `p_(1st) - p_(2nd)`. A larger margin means the leading class is more
  clearly separated from its nearest alternative.

For example, probabilities MSF = 0.70, SE = 0.20, and SI = 0.10 give confidence
0.70 and margin 0.50. The prediction is relatively decisive because one class is
well above the other two. These values describe the model's internal certainty,
not whether the map is correct at an individual location.

| Comparison outcome | N | Median LiDAR confidence | Median entropy | Median probability margin | Interpretation |
| --- | ---: | ---: | ---: | ---: | --- |
| Both correct | 142 | 0.60 | 0.80 | 0.31 | Both methods match the recorded field call. |
| LiDAR correct; rule wrong | 136 | 0.60 | 0.80 | 0.33 | Primary set for identifying structural information added by LiDAR. |
| Rule correct; LiDAR wrong | 108 | 0.54 | 0.83 | 0.21 | Primary set for LiDAR failure-mode review; LiDAR calls are comparatively less decisive. |
| Both wrong; same call | 101 | 0.57 | 0.83 | 0.26 | Possible shared limitation, label ambiguity, stale inputs, or a class not well represented by either approach. |
| Both wrong; different calls | 41 | 0.66 | 0.77 | 0.41 | Highest-priority QA cases because neither product agrees with the field call and the products also disagree. |

The rule-correct/LiDAR-wrong group has the lowest median LiDAR confidence and
margin, and the highest entropy. Entropy and margin can therefore help identify
uncertain LiDAR calls for review. The 41 points where both products are wrong and
make different calls are not necessarily low-confidence; they need imagery,
disturbance history, comments, and field review.

### Which field and model calls make up each group?

The outcome-group medians combine several distinct three-way combinations:
recorded field stage, rule-model call, and LiDAR call. The table below keeps
those classifications visible. It includes every combination with five or more
points (513 of 528 points); the remaining 15 points occur in smaller
combinations. For example, 25 of the 41 points where both products are wrong and
make different calls have a recorded SE field call, rule-model MSF call, and
LiDAR SI call. That is the main classification pattern behind that outcome
group, rather than a single generic form of disagreement.

| Comparison outcome | Recorded field stage | Rule-model call | LiDAR call | N | Median zmax (m) | Median low-mid stratum count | Median count >6.1 m | Median midstory degree | Median understory betweenness |
| --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Both correct | SE | SE | SE | 99 | 27.3 | 6.8 | 33.1 | 3.48 | 245.9 |
| Both correct | MSF | MSF | MSF | 41 | 36.8 | 15.8 | 34.3 | 3.29 | 477.5 |
| LiDAR correct; rule wrong | SE | MSF | SE | 86 | 27.2 | 9.1 | 35.9 | 3.62 | 255.4 |
| LiDAR correct; rule wrong | SI | SE | SI | 17 | 32.3 | 12.5 | 22.2 | 2.40 | 524.0 |
| LiDAR correct; rule wrong | MSF | SE | MSF | 17 | 30.8 | 17.5 | 37.1 | 3.65 | 483.8 |
| LiDAR correct; rule wrong | SI | MSF | SI | 10 | 33.1 | 37.1 | 51.8 | 3.21 | 555.9 |
| LiDAR correct; rule wrong | SE | SI | SE | 5 | 38.1 | 10.9 | 35.3 | 3.72 | 478.7 |
| Rule correct; LiDAR wrong | SE | SE | MSF | 65 | 32.4 | 9.4 | 30.8 | 3.44 | 381.9 |
| Rule correct; LiDAR wrong | SE | SE | SI | 20 | 25.4 | 79.8 | 91.5 | 3.45 | 650.2 |
| Rule correct; LiDAR wrong | MSF | MSF | SE | 11 | 24.4 | 14.1 | 44.8 | 4.16 | 517.8 |
| Rule correct; LiDAR wrong | MSF | MSF | SI | 8 | 28.1 | 57.6 | 72.6 | 3.41 | 644.6 |
| Both wrong; same call | SE | MSF | MSF | 77 | 36.0 | 11.4 | 31.1 | 3.36 | 418.9 |
| Both wrong; same call | MSF | SE | SE | 9 | 24.8 | 9.0 | 41.3 | 3.69 | 482.7 |
| Both wrong; same call | SI | SE | SE | 7 | 26.6 | 7.8 | 43.9 | 3.88 | 266.0 |
| Both wrong; same call | SI | MSF | MSF | 5 | 34.2 | 9.2 | 20.2 | 2.19 | 188.8 |
| Both wrong; different calls | SE | MSF | SI | 25 | 25.2 | 79.4 | 97.1 | 3.37 | 631.4 |
| Both wrong; different calls | SI | SE | MSF | 6 | 33.3 | 19.9 | 45.9 | 3.80 | 445.9 |
| Both wrong; different calls | MSF | SE | SI | 5 | 28.7 | 41.1 | 52.9 | 3.32 | 602.2 |

### Important directional transitions

| Recorded field stage | Rule-model call -> LiDAR call | N | Median LiDAR confidence | Median margin | Diagnostic meaning |
| --- | --- | ---: | ---: | ---: | --- |
| SE | MSF -> SE | 86 | 0.56 | 0.25 | LiDAR agrees with the field SE call and corrects a rule-model MSF call. |
| SI | SE -> SI | 17 | 0.84 | 0.73 | LiDAR agrees with the field SI call with strong separation from its second choice. |
| SE | MSF -> SI | 25 | 0.73 | 0.56 | LiDAR is confidently wrong relative to the recorded SE call; review timing, disturbance, labels, and model limits. |
| SE | SE -> MSF | 65 | 0.53 | 0.17 | LiDAR is wrong relative to recorded SE, but usually uncertain. |

### Structure in the disagreement groups

| Comparison outcome | Median zmax (m) | Median low-mid stratum count | Median count >6.1 m | Median midstory degree | Median understory betweenness |
| --- | ---: | ---: | ---: | ---: | ---: |
| Both correct | 28.7 | 8.6 | 33.2 | 3.40 | 311.6 |
| LiDAR correct; rule wrong | 29.4 | 10.9 | 36.1 | 3.43 | 352.6 |
| Rule correct; LiDAR wrong | 29.9 | 15.4 | 40.6 | 3.44 | 503.0 |
| Both wrong; same call | 33.8 | 10.8 | 33.5 | 3.35 | 421.1 |
| Both wrong; different calls | 27.7 | 64.1 | 76.0 | 3.37 | 602.2 |

The 41 points where both products are wrong and disagree have substantially
higher low-mid layering, vegetation above 6.1 m, and understory betweenness than
the both-correct group. Treat them as a distinct ecological or data-QA group for
review; these medians do not establish thresholds or causal mechanisms.

### Agreement and disagreement by field class

The figure below shows which recorded field class is associated with each
outcome. Each row sums to 100%, making it possible to see whether agreement or a
particular error type is concentrated in MSF, SE, or SI.

![Agreement and disagreement by field class](../outputs/gutcheck_disagreement/figures/all_eligible_outcome_by_field_class.png)

## Appropriate next steps

- Review high-value disagreement groups with KNF wildlife specialists.
- Document survey, reviewer, label, and LiDAR-acquisition provenance.
- Define disturbance and LiDAR-age triggers for a no-prediction or mandatory
  review flag.
- Evaluate the model with documented, spatially independent field data before
  making a formal comparative-performance claim.
- Publish a jointly reviewed use-and-limitations memo explaining how the LiDAR
  layer and existing KNF layer coexist.

For technical detail, data lineage, class-specific metrics, uncertainty analysis,
and directional disagreement results, see the full Steering Group briefing.
