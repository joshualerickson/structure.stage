# Build an auditable inventory of the legacy lynx training-data preparation.
#
# This script documents the data lineage implemented in `prepping_lynx.R`; it
# does not rerun that script or overwrite any source data. The legacy script
# contains machine-specific paths and interactive exploration. Run this script
# after adding a new source or before a model run to produce a reviewable record
# of the source layers, schemas, class counts, and known provenance gaps.

.training_source_catalog <- function(source_dir) {
  base::data.frame(
    source_name = base::c(
      "Flathead field-labeled points",
      "Flathead stand polygons",
      "Ksanka prepared points",
      "Combined legacy training points",
      "Lynx survey units"
    ),
    path = base::file.path(source_dir, base::c(
      "fh_lynx_pts(1).gpkg",
      "fh_stands_lynx.gpkg",
      "final_lynx.gpkg",
      "final_lynx.gpkg",
      "lynx_survey_units.gpkg"
    )),
    layer = base::c(
      "fh_lynx_pts",
      "fh_stands_lynx",
      "final_ksanka_fnf_structure",
      "final_lynx_structure",
      "lynx_surveys_06092025"
    ),
    role = base::c(
      "Source point labels for Flathead preparation",
      "Flathead polygons sampled after spatial intersection with source labels",
      "Prepared Ksanka / FNF labeled points used in the combined layer",
      "Direct source of the legacy model-training CSV",
      "Potential provenance / review layer; not read by the legacy preparation script"
    ),
    stringsAsFactors = FALSE
  )
}

.selected_predictor_dictionary <- function() {
  base::data.frame(
    name = base::c(
      "zmax", "n_strata_low_mid", "n_gt_6_1",
      "midstory_mean_degree", "understory_mean_betweenness"
    ),
    units = base::c(
      "m", "count per 30 m cell", "count per 30 m cell",
      "connections per vegetation voxel", "dimensionless"
    ),
    description = base::c(
      "Maximum vegetation height.",
      "Detected vegetation / tree count in the 6.1–12.1 m low-to-mid stratum.",
      "Detected vegetation / tree count above 6.1 m.",
      "Mean number of connections among midstory vegetation voxels.",
      "Mean understory voxel betweenness centrality: relative importance of voxels in connecting vegetation paths."
    ),
    extraction = base::rep(
      "Derived from normalized airborne-LiDAR point clouds as a 30 m raster metric.",
      5L
    ),
    stringsAsFactors = FALSE
  )
}

.legacy_lineage <- function() {
  base::data.frame(
    step = base::seq_len(7L),
    operation = base::c(
      "Read prepared Ksanka / FNF points",
      "Read Flathead field-labeled points and stand polygons",
      "Harmonize Flathead source labels",
      "Identify Flathead polygons intersecting source labels",
      "Sample Flathead polygons",
      "Combine prepared Ksanka and Flathead samples",
      "Extract LiDAR and ancillary raster metrics at points"
    ),
    implementation = base::c(
      "Reads `final_ksanka_fnf_structure` from `final_lynx.gpkg`.",
      "Reads `fh_lynx_pts` and `fh_stands_lynx`.",
      "Maps MMSF to Multi-Story Foraging; MNF to Multi-Story Non-foraging; SE to Stem Exclusion; SI to Stand Initiation; drops NA and ESI.",
      "Uses spatial intersection between Flathead source labels and stand polygons.",
      "Samples approximately integer(area_acres / 2) points in each retained Flathead polygon. This is pseudo-replication within a stand and must remain grouped in spatial assessment.",
      "Writes `final_lynx_structure` with a harmonized stage field; legacy output contains only the stage field.",
      "Extracts 30 m standard, canopy, graph, and environmental raster metrics, then writes `lynx_df.csv`."
    ),
    audit_note = base::c(
      "Prepared-point provenance must be recovered from the upstream Ksanka source.",
      "The Flathead source includes `globalid`, observer, and survey-date fields.",
      "The final modeling class map later merges Multi-Story Non-foraging and Stem Exclusion into SE; retain both original labels before that modeling decision.",
      "Retain original point and polygon identifiers after intersection.",
      "The legacy combined output drops the originating stand ID, so polygon-level leakage cannot be audited from `lynx_df.csv` alone.",
      "This is the principal legacy provenance loss; do not repeat it for new sources.",
      "The legacy script uses machine-specific paths. Record exact raster product versions, acquisition year, QL, and processing parameters for every new run."
    ),
    stringsAsFactors = FALSE
  )
}

.read_layer_inventory <- function(path, layer, source_name, role) {
  if (!base::file.exists(path)) {
    return(base::list(
      source = base::data.frame(
        source_name = source_name, path = path, layer = layer, role = role,
        status = "missing", feature_count = NA_integer_, geometry_type = NA_character_,
        crs = NA_character_, stringsAsFactors = FALSE
      ),
      fields = base::data.frame(), stages = base::data.frame()
    ))
  }
  records <- sf::st_read(path, layer = layer, quiet = TRUE)
  attributes <- sf::st_drop_geometry(records)
  source <- base::data.frame(
    source_name = source_name, path = path, layer = layer, role = role,
    status = "read", feature_count = base::nrow(records),
    geometry_type = base::as.character(sf::st_geometry_type(records, by_geometry = FALSE)),
    crs = if (base::is.null(sf::st_crs(records)$input)) NA_character_ else sf::st_crs(records)$input,
    stringsAsFactors = FALSE
  )
  fields <- base::data.frame(
    source_name = source_name, layer = layer, field = base::names(attributes),
    storage_class = base::vapply(attributes, function(x) base::class(x)[1L], base::character(1)),
    stringsAsFactors = FALSE
  )
  stage_name <- base::intersect(
    base::names(attributes), base::c("structure_stage", "STRCSTG", "structural")
  )
  stages <- if (base::length(stage_name)) {
    counts <- base::as.data.frame(base::table(attributes[[stage_name[1L]]], useNA = "ifany"),
      stringsAsFactors = FALSE
    )
    base::names(counts) <- base::c("source_stage", "n")
    counts$source_name <- source_name
    counts$layer <- layer
    counts$stage_field <- stage_name[1L]
    counts
  } else {
    base::data.frame()
  }
  base::list(source = source, fields = fields, stages = stages)
}

#' Build the legacy lynx training-data record
#'
#' @param source_dir Directory containing the files used by `prepping_lynx.R`.
#' @param output_dir Existing or new directory for CSV and Markdown audit files.
#' @param training_csv Optional path to the legacy extracted training CSV.
#' @return An invisible list of inventory tables.
#'
#' @details This is development documentation, not a package function. It does
#' not alter input files or fit a model.
build_legacy_training_data_record <- function(
    source_dir = "outputs/models",
    output_dir = "outputs/training_data_record",
    training_csv = "/home/josh.erickson/Downloads/lynx_df.csv"
) {
  if (!base::dir.exists(source_dir)) base::stop("source_dir does not exist.", call. = FALSE)
  if (!base::dir.exists(output_dir)) base::dir.create(output_dir, recursive = TRUE)
  catalog <- .training_source_catalog(source_dir)
  inventory <- base::lapply(base::seq_len(base::nrow(catalog)), function(i) {
    .read_layer_inventory(catalog$path[i], catalog$layer[i], catalog$source_name[i], catalog$role[i])
  })
  source_inventory <- base::do.call(base::rbind, base::lapply(inventory, `[[`, "source"))
  field_inventory <- base::do.call(base::rbind, base::Filter(
    function(x) base::nrow(x) > 0L, base::lapply(inventory, `[[`, "fields")
  ))
  stage_tables <- base::Filter(
    function(x) base::nrow(x) > 0L, base::lapply(inventory, `[[`, "stages")
  )
  stage_inventory <- if (base::length(stage_tables)) {
    base::do.call(base::rbind, stage_tables)
  } else {
    base::data.frame()
  }
  utils::write.csv(catalog, base::file.path(output_dir, "source_catalog.csv"), row.names = FALSE)
  utils::write.csv(source_inventory, base::file.path(output_dir, "source_inventory.csv"), row.names = FALSE)
  utils::write.csv(field_inventory, base::file.path(output_dir, "source_field_inventory.csv"), row.names = FALSE)
  utils::write.csv(stage_inventory, base::file.path(output_dir, "source_stage_counts.csv"), row.names = FALSE)
  utils::write.csv(.legacy_lineage(), base::file.path(output_dir, "legacy_lineage.csv"), row.names = FALSE)
  utils::write.csv(.selected_predictor_dictionary(), base::file.path(output_dir, "selected_predictor_dictionary.csv"), row.names = FALSE)

  training_summary <- if (base::file.exists(training_csv)) {
    data <- utils::read.csv(training_csv, check.names = FALSE)
    stage_counts <- base::as.data.frame(base::table(data$structure_stage, useNA = "ifany"), stringsAsFactors = FALSE)
    base::names(stage_counts) <- base::c("source_stage", "n")
    utils::write.csv(stage_counts, base::file.path(output_dir, "legacy_csv_stage_counts.csv"), row.names = FALSE)
    base::data.frame(
      training_csv = training_csv, rows = base::nrow(data), columns = base::ncol(data),
      has_sample_id = "sample_id" %in% base::names(data),
      has_polygon_id = "polygon_id" %in% base::names(data),
      has_source_id = "source_id" %in% base::names(data),
      has_acquisition_id = "acquisition_id" %in% base::names(data),
      stringsAsFactors = FALSE
    )
  } else {
    base::data.frame(training_csv = training_csv, rows = NA_integer_, columns = NA_integer_,
      has_sample_id = NA, has_polygon_id = NA, has_source_id = NA, has_acquisition_id = NA)
  }
  utils::write.csv(training_summary, base::file.path(output_dir, "legacy_csv_inventory.csv"), row.names = FALSE)

  lines <- base::c(
    "# Legacy lynx training-data record",
    "",
    "Generated by `dev/training_data_record.R`. This is an inventory of the existing preparation lineage; it does not certify that legacy labels are independent field truth.",
    "",
    "## Current legacy contract",
    "",
    "The legacy modeling CSV supplies coordinates, a source structural-stage label, and extracted raster metrics. It does **not** preserve stable sample, polygon, source, acquisition, observer, or survey-date identifiers. Consequently, it cannot by itself support a polygon-leakage audit, observer-effect review, or survey-to-LiDAR timing audit.",
    "",
    "Future onboarding must retain `sample_id`, source-qualified `polygon_id`, `source_id`, `acquisition_id`, survey date, observer / QA status where permitted, x/y coordinates, declared CRS, original label, harmonized label, and every predictor's source version and units.",
    "",
    "## Generated files",
    "",
    "- `source_catalog.csv`: source layers and intended role.",
    "- `source_inventory.csv`, `source_field_inventory.csv`, and `source_stage_counts.csv`: directly observed layer contents.",
    "- `legacy_lineage.csv`: transformation record transcribed from `prepping_lynx.R`.",
    "- `selected_predictor_dictionary.csv`: units and descriptions for the final five predictors.",
    "- `legacy_csv_inventory.csv` and `legacy_csv_stage_counts.csv`: audit of the extracted training CSV when available."
  )
  base::writeLines(lines, base::file.path(output_dir, "README.md"))
  base::invisible(base::list(
    catalog = catalog, source_inventory = source_inventory, field_inventory = field_inventory,
    stage_inventory = stage_inventory, lineage = .legacy_lineage(),
    predictors = .selected_predictor_dictionary(), training_summary = training_summary
  ))
}

# Example:
# build_legacy_training_data_record()
