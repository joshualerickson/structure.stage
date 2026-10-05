# Polygon-level structural-stage comparison

This folder evaluates the polygon-summarized LiDAR GBM output in
`koot_model_example.gpkg` against the 528 eligible three-class gut-check points.
The polygon-level LiDAR class is `winner`: the class with the largest LiDAR
structural-stage share in the intersected polygon. The polygon-level rule class
is `structural`, harmonized where possible to MSF, SE, or SI.

## Spatial matching

Of 528 eligible points, 524 intersect exactly one Kootenai polygon and are used
for the LiDAR polygon-winner comparison. Three points intersect no polygon and
one intersects two polygons; they are retained in the joined layer with a join
status but excluded from performance metrics. Six of the 524 matched points have
non-comparable rule categories, so the polygon-rule comparison uses 518 points.

## Overall results

| Product | N | Overall accuracy | Mean balanced accuracy | Macro recall | Macro F1 |
| --- | ---: | ---: | ---: | ---: | ---: |
| LiDAR GBM point prediction, matched subset | 524 | 52.7% | 66.3% | 56.1% | 47.3% |
| LiDAR GBM polygon winner | 524 | 49.4% | 63.5% | 52.7% | 44.4% |
| KNF rule-model polygon stage | 518 | 47.1% | 53.4% | 38.8% | 33.6% |

Polygon aggregation reduces the LiDAR result relative to its cell/point call,
as expected when a within-polygon majority class replaces local structural
variation. On the same unambiguous polygon-matched points, the polygon LiDAR
winner still has higher mean balanced accuracy than the comparable polygon rule
stage. This is a held-out geographic comparison against recorded field calls;
it does not remove the need for project-level field verification.

See the CSV files for class-specific metrics, confusion counts, and the spatial
join denominator.
