# KNF rule-versus-LiDAR disagreement audit

This diagnostic identifies the direction of rule-model and LiDAR-GBM disagreement at gut-check points. It must be interpreted against stand-derived field calls, review status, imagery, disturbance history, and map age; it is not pixel-level truth or causal evidence.

## Case types

- `Both correct`: both models agree with the three-class field call.
- `LiDAR correct; rule wrong`: the priority set for understanding LiDAR-added structural evidence.
- `Rule correct; LiDAR wrong`: the priority set for identifying LiDAR failure modes, label ambiguity, stale LiDAR, or missing predictors.
- `Both wrong; same call`: likely shared limitation, crosswalk issue, label ambiguity, or a condition neither model represents well.
- `Both wrong; different calls`: strongest disagreement / QA candidates.

## Uncertainty fields

- `lidar_confidence`: largest of P(MSF), P(SE), and P(SI).
- `lidar_entropy`: normalized Shannon entropy of all three probabilities; values nearer one mean the probability distribution is more even and uncertain.
- `lidar_margin`: highest probability minus second-highest probability; values nearer zero mean the top two classes are difficult to distinguish.

## Outputs

- `*_point_audit.csv`: field/rule/LiDAR classes, probabilities, uncertainty, review status, metrics, and directional case type.
- `*_case_summary.csv`: counts and uncertainty summarized by case type.
- `*_direction_summary.csv`: the same information by observed class and rule-to-LiDAR transition.
- `*_metric_summary.csv`: structural-metric distributions by case type. These are descriptive, not causal effects.
