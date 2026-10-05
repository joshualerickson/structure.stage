# Private notes: KNF Wildlife Steering Group discussion

Use these notes with the full briefing. They are not a stakeholder handout.

## Position to maintain

- The LiDAR layer is supplemental structural evidence, screening, QA, and
  reconnaissance prioritization.
- It does not replace the official KNF habitat layer, NRLMD / Forest Plan
  processes, or project-level field verification.
- Avoid saying “pixel truth.” Predictions are 30 m LiDAR cells trained and
  assessed against stand-derived / field-recorded labels.
- The gut-check is a geographically independent holdout comparison: neither its
  points nor its broader project area entered LiDAR training. Preserve reviewer
  provenance and the `review` strata when reporting it.

## Answers and evidence to bring

| Steering Group topic | What is established | Work still needed |
| --- | --- | --- |
| LiDAR age and coverage | Current coverage metadata lists `MT_Lincoln` and `MT_Lincoln2` as 2019 QL1. The 30 m output raster provides three probabilities, class, entropy, and margin. | Obtain the 2024 acquisition footprint/metadata and verify whether it was incorporated into the current raster. Carry year and QL as a companion layer for every mapped area. |
| Updating after fire/harvest | The existing KNF 2020 documentation calls for every-other-year mapping and annual updates after major fire when needed. LiDAR output can be paired with a disturbance/no-prediction flag. | Agree on authoritative disturbance layers, refresh cadence, and a threshold for “stale LiDAR; field review required.” |
| Intended users | Wildlife, silviculture, fuels, GIS, and planning staff can use disagreement and uncertainty to sequence field review. | Coauthor use-and-limitations memo and decide who owns model runs / map distribution. |
| Reconnaissance efficiency | Efficiency is prioritization: inspect map disagreement, high uncertainty, and stale-disturbance locations first; do not promise fewer required stand visits. | Pilot the workflow on an upcoming project and record whether it changes travel, review time, or discovered stage corrections. |
| Multiple-model concern | Existing and LiDAR products serve different evidence roles. The use-and-limitations memo must state that neither alone determines compliance. | Agree on a versioned map-comparison and field-review process before operational use. |
| Training labels | Legacy source inventory and preparation lineage are now recorded. Flathead source points retain `globalid`, observer, and survey date. | Reconstruct source-qualified sample/polygon IDs in the final legacy combined dataset; document Ksanka label source, timing, and QA. |
| Gut-check comparison | 1,207 survey points exactly match by `GlobalID` between source and joined layer. 528 have comparable three-class field/rule/LiDAR calls. The project owner confirmed that the points and broader area were excluded from LiDAR training. | Confirm field protocol and reviewer identity/role; retain `review = TRUE/FALSE` as an evidence-provenance stratum. |
| Salish versus Purcell transferability | Current data cover only conditions represented by the training sources and LiDAR acquisitions. | Ask local biologists to identify missing ecological settings, then reserve or collect geographically independent data for them. |

## Numbers to state carefully

- Gut-check, all eligible three-class points (`n = 528`): rule 47.3% overall
  agreement, 41.3% macro recall; LiDAR 52.7% overall agreement, 56.0% macro
  recall.
- LiDAR is not uniformly better: among `review = FALSE` points (`n = 284`), the
  rule model has higher overall agreement (55.3% vs. 51.4%) but lower macro
  recall (44.2% vs. 56.2%).
- The SI contrast is material: rule SI recall 11.3%; LiDAR SI recall 54.7% in
  all eligible points. LiDAR SI precision remains 32.2%, so SI output is a
  field-review flag, not a determination.
- Do not call the saved five-variable model's ten custom resamples external
  validation. Preserve the nested spatial-CV result and polygon/fold audit for
  any reportable model-performance statement.

## Questions to ask the group in the meeting

1. Which map disagreements would be most useful for biologists to see first:
   rule MSF -> LiDAR SE, rule SE -> LiDAR SI, or another transition?
2. What conditions should automatically mark a LiDAR prediction stale or outside
   the model’s usable domain?
3. Which recent project areas have field observations suitable for a genuinely
   independent, spatially separated evaluation?
4. What specific label and observer QA rules would make a future validation set
   acceptable to the Steering Group?
5. What would make this useful to a project team without being mistaken for a
   compliance or treatment-decision layer?

## Follow-up artifacts

- Complete data contract with stable sample, polygon, source, acquisition,
  observer/reviewer, survey date, label, and raster-version fields.
- 2024 LiDAR provenance and disturbance-currency policy.
- Independent spatial validation protocol and results.
- Jointly reviewed use-and-limitations memo.
- A pilot map-review log that records disagreement, uncertainty, field finding,
  and final decision.
