# Generate ArcGIS Pro import-ready FGDC CSDGM metadata for key GeoPackages.
#
# Run from the repository root:
#   Rscript dev/generate_arcpro_metadata.R
#
# Each XML is a sidecar file. In ArcGIS Pro, use Import Metadata on the target
# GeoPackage item and select the corresponding XML file with FGDC selected as
# the metadata format.

root <- normalizePath(getwd(), mustWork = TRUE)
models_dir <- file.path(root, "outputs", "models")
metadata_date <- format(Sys.Date(), "%Y%m%d")

if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE)) {
  stop("Packages `sf` and `xml2` are required.", call. = FALSE)
}

xml_add_text <- function(parent, name, value) {
  xml2::xml_add_child(parent, name, as.character(value %||% ""))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) y else x
}

field_description <- function(field) {
  exact <- c(
    structure_stage = "Structural-stage class assigned to this point or predicted for this location.",
    GlobalID = "Source-record globally unique identifier used to join survey and model records.",
    Surveyor = "Surveyor identifier retained from the source wildlife survey record.",
    TimeStamp = "Date or timestamp retained from the source wildlife survey record.",
    Lynx_Model = "Existing rule-model structural-stage call retained from the source record.",
    Lynx_Field = "Recorded field structural-stage call retained from the source record.",
    review = "Source review flag retained from the gut-check dataset.",
    msf = "LiDAR GBM probability for Multi-Story Foraging (MSF).",
    se = "LiDAR GBM probability for Stem Exclusion (SE).",
    si = "LiDAR GBM probability for Stand Initiation (SI).",
    zmax = "Maximum LiDAR-derived vegetation height, in meters.",
    n_strata_low_mid = "LiDAR-derived low-to-mid vertical stratum metric used by the final GBM.",
    n_gt_6_1 = "LiDAR-derived measure of vegetation above 6.1 meters, used by the final GBM.",
    midstory_mean_degree = "LiDAR-derived midstory connectivity metric used by the final GBM.",
    understory_mean_betweenness = "LiDAR-derived understory connectivity metric used by the final GBM.",
    field_stage = "Recorded field stage harmonized to msf, se, or si for the three-class comparison.",
    rule_stage = "Existing rule-model stage harmonized to msf, se, or si for the three-class comparison.",
    lidar_stage = "LiDAR GBM predicted stage: msf, se, or si.",
    eligible_3class = "TRUE when recorded field, rule-model, and LiDAR stages are all available in the three-class comparison.",
    review_status = "Review provenance stratum derived from the source review flag.",
    comparison_outcome = "Agreement category relative to the recorded field call: both correct, one model correct, or both wrong.",
    models_agree = "TRUE when the harmonized rule-model and LiDAR stages are identical among eligible records.",
    rule_matches_field = "TRUE when the harmonized rule-model stage matches the recorded field stage among eligible records.",
    lidar_matches_field = "TRUE when the LiDAR stage matches the recorded field stage among eligible records.",
    rule_to_lidar = "Directional transition from the harmonized rule-model stage to the LiDAR stage.",
    field_rule_lidar = "Recorded field stage plus rule-to-LiDAR directional transition.",
    lidar_confidence = "Largest LiDAR class probability.",
    lidar_entropy = "Normalized Shannon entropy of LiDAR class probabilities; 0 is concentrated and 1 is evenly distributed.",
    lidar_margin = "Largest LiDAR class probability minus the second-largest probability.",
    acres = "Polygon area in acres retained from the source dataset.",
    forest_id = "Forest or stand identifier retained from the source dataset.",
    winner = "Structural stage with the largest within-polygon LiDAR vote or share.",
    winner_share = "Within-polygon share associated with the winning LiDAR structural stage.",
    runnerup_share = "Within-polygon share associated with the second-ranked LiDAR structural stage.",
    margin = "Difference between winner and runner-up LiDAR structural-stage shares.",
    vote_entropy = "Entropy of within-polygon LiDAR structural-stage shares.",
    vote_entropy_norm = "Normalized entropy of within-polygon LiDAR structural-stage shares.",
    landsat8_summer_fall_diff = "Value extracted at the point from landsat8_summer_fall_diff_R1_30m.tif, a 30-meter WGS84 Landsat summer-to-fall difference raster.",
    landsat_extract_status = "Extraction status: extracted, or no_data_or_outside_raster.",
    landsat8_summer_fall_diff_mean_5x5 = "Mean of the 5 by 5 raster-cell window centered on the point, extracted from landsat8_summer_fall_diff_R1_30m.tif. At nominal 30-meter resolution, the window is 150 by 150 meters.",
    landsat_mean5x5_valid_cells = "Number of non-missing raster cells contributing to the 5 by 5 Landsat neighborhood mean; a complete window contains 25 cells.",
    landsat_mean5x5_status = "Five-by-five neighborhood extraction status: extracted_all_25_cells, extracted_partial_window, or no_data_or_outside_raster.",
    koot_polygon_join_status = "Spatial-join status between the eligible point and koot_model_example polygon layer: matched_one_polygon, no_intersecting_polygon, or multiple_intersecting_polygons.",
    koot_polygon_match_count = "Number of koot_model_example polygons intersecting the point.",
    koot_fid_lynxha = "Identifier of the spatially matched koot_model_example polygon.",
    koot_forest_id = "Forest identifier retained from the spatially matched koot_model_example polygon.",
    koot_rule_structural = "Original rule-model structural category from the matched polygon.",
    koot_rule_stage = "Matched-polygon rule-model structural category harmonized to msf, se, or si when comparable.",
    koot_gbm_winner = "LiDAR structural stage with the largest summarized class share in the matched polygon.",
    koot_gbm_msf_share = "Matched-polygon share of LiDAR structural-stage output classified as msf.",
    koot_gbm_se_share = "Matched-polygon share of LiDAR structural-stage output classified as se.",
    koot_gbm_si_share = "Matched-polygon share of LiDAR structural-stage output classified as si.",
    koot_gbm_margin = "Matched-polygon difference between the largest and second-largest LiDAR structural-stage shares.",
    koot_gbm_entropy_norm = "Normalized entropy of the matched-polygon LiDAR structural-stage shares."
  )
  if (field %in% names(exact)) return(exact[[field]])
  if (grepl("^n_strata", field)) return("LiDAR-derived vertical-stratum count or density metric retained from the source dataset.")
  if (grepl("^n_gt_", field)) return("LiDAR-derived vegetation-above-height-threshold metric retained from the source dataset.")
  if (grepl("^(understory|midstory)_", field)) return("LiDAR-derived understory or midstory structural/connectivity metric retained from the source dataset.")
  if (grepl("^(z|LAD|LAI|topo_|rumple|fractional_canopy|trees_|n_trees|smoothness|bt_diff|graph_metrics)", field)) return("LiDAR-derived vegetation-structure metric retained from the source dataset.")
  "Source attribute retained from the supplied dataset; its detailed domain definition has not yet been independently documented."
}

layer_description <- function(dataset, layer) {
  descriptions <- list(
    final_lynx = c(
      final_ksanka_structure = "Point records from the Ksanka structural-stage training source, carrying the source structural-stage label.",
      final_ksanka_fnf_structure = "Point records from the Ksanka/FNF structural-stage training source, carrying the source structural-stage label.",
      final_lynx_structure = "Combined final structural-stage training point records used to develop the LiDAR model."
    ),
    gt_sf_eda = c(
      gt_sf_eda = "Gut-check wildlife-survey points with source field calls, existing rule-model calls, LiDAR probabilities, and LiDAR structural metrics.",
      gt_sf_eda_handout_audit = "All gut-check points with derived agreement, disagreement, class-transition, and LiDAR uncertainty attributes used in the stakeholder handout.",
      gt_sf_eda_eligible_3class = "Subset of gut-check points eligible for the three-class MSF/SE/SI comparison; includes the handout audit attributes.",
      gt_sf_eda_eligible_3class_landsat = "Eligible three-class gut-check points with an extracted value from the 30-meter Landsat summer-to-fall difference raster.",
      gt_sf_eda_eligible_3class_landsat_mean5x5 = "Eligible three-class gut-check points with the mean of a 5 by 5 Landsat summer-to-fall difference raster neighborhood centered on each point.",
      gt_sf_eda_eligible_3class_koot_polygon = "Eligible three-class gut-check points spatially joined to Kootenai rule-model polygons and polygon-summarized LiDAR structural-stage classes for polygon-level performance comparison."
    ),
    koot_model_example = c(
      koot_model_example = "KNF rule-model polygons with retained mapped vegetation and habitat attributes plus summarized LiDAR structural-stage comparison attributes."
    )
  )
  descriptions[[dataset]][[layer]] %||% "Spatial layer contained in this GeoPackage."
}

dataset_info <- list(
  final_lynx = list(
    file = "final_lynx.gpkg",
    title = "Final LiDAR structural-stage training points",
    abstract = "Point records assembled for LiDAR structural-stage model development. Each record carries a source structural-stage label. The layer supports model development and provenance review; labels are stand-derived and do not represent independently observed pixel-level truth.",
    purpose = "Provide the final point-based training inputs and source labels for the LiDAR structural-stage modeling workflow.",
    lineage = "Compiled from final structural-stage preparation outputs. The combined layer contains 30,170 points, including source-specific subsets. Training-data provenance gaps, including stable polygon identifiers and acquisition metadata, remain documented limitations.",
    use = "Use for model-development provenance and documented retraining workflows. Do not use as independent pixel-scale validation data."
  ),
  gt_sf_eda = list(
    file = "gt_sf_eda.gpkg",
    title = "KNF gut-check surveys and LiDAR structural-stage comparison",
    abstract = "Wildlife-survey point records with field and existing rule-model calls, LiDAR structural metrics, and LiDAR class probabilities. Derived audit layers identify agreement/disagreement categories and uncertainty for the eligible three-class MSF/SE/SI comparison.",
    purpose = "Support external geographic validation, map QA, reconnaissance prioritization, and review of rule-model versus LiDAR structural-stage disagreement.",
    lineage = "The base gt_sf_eda layer contains 1,207 supplied survey records. The derived audit layers join 528 records eligible for direct three-class comparison by GlobalID. Records with non-comparable rule categories, such as Other, Early Stand Initiation, or No Habitat, are retained in the all-record audit layer but have no three-class comparison outcome.",
    use = "Use eligible_3class to select records appropriate for three-class metric reporting. Review provenance, survey protocol, field notes, imagery, disturbance history, and map vintage before project decisions."
  ),
  koot_model_example = list(
    file = "koot_model_example.gpkg",
    title = "KNF rule-model polygons with LiDAR structural-stage summaries",
    abstract = "Polygon dataset retaining mapped KNF habitat and vegetation attributes, existing structural-stage outputs, and within-polygon summaries of LiDAR structural-stage classes and uncertainty.",
    purpose = "Provide a spatial comparison between the existing KNF rule-based product and LiDAR structural-stage outputs for map review and project screening.",
    lineage = "Prepared from supplied KNF model polygons and joined LiDAR structural-stage summary attributes. Source mapped-vegetation attribute definitions and update history remain governed by the originating KNF data documentation.",
    use = "Use for broad-scale map comparison, reconnaissance prioritization, and QA. It does not replace project-level field verification or Forest Plan/NRLMD requirements."
  )
)

add_contact <- function(parent) {
  contact <- xml2::xml_add_child(parent, "ptcontac")
  info <- xml2::xml_add_child(contact, "cntinfo")
  xml_add_text(info, "cntper", "Joshua Erickson")
  xml_add_text(info, "cntemail", "joshualerickson@gmail.com")
}

write_fgdc_metadata <- function(key, info) {
  gpkg_path <- file.path(models_dir, info$file)
  layers <- sf::st_layers(gpkg_path)$name
  first_layer <- sf::st_read(gpkg_path, layer = layers[[1]], quiet = TRUE)
  geographic <- sf::st_transform(first_layer, 4326)
  extent <- sf::st_bbox(geographic)
  crs <- sf::st_crs(first_layer)

  doc <- xml2::read_xml("<metadata/>")
  idinfo <- xml2::xml_add_child(doc, "idinfo")
  citation <- xml2::xml_add_child(idinfo, "citation")
  citeinfo <- xml2::xml_add_child(citation, "citeinfo")
  xml_add_text(citeinfo, "title", info$title)
  xml_add_text(citeinfo, "pubdate", metadata_date)
  xml_add_text(citeinfo, "origin", "Joshua Erickson")
  descript <- xml2::xml_add_child(idinfo, "descript")
  xml_add_text(descript, "abstract", info$abstract)
  xml_add_text(descript, "purpose", info$purpose)
  timeperd <- xml2::xml_add_child(idinfo, "timeperd")
  timeinfo <- xml2::xml_add_child(timeperd, "timeinfo")
  sngdate <- xml2::xml_add_child(timeinfo, "sngdate")
  xml_add_text(sngdate, "caldate", metadata_date)
  status <- xml2::xml_add_child(idinfo, "status")
  xml_add_text(status, "progress", "Complete")
  xml_add_text(status, "update", "As needed")
  spdom <- xml2::xml_add_child(idinfo, "spdom")
  bounding <- xml2::xml_add_child(spdom, "bounding")
  xml_add_text(bounding, "westbc", sprintf("%.6f", extent[["xmin"]]))
  xml_add_text(bounding, "eastbc", sprintf("%.6f", extent[["xmax"]]))
  xml_add_text(bounding, "northbc", sprintf("%.6f", extent[["ymax"]]))
  xml_add_text(bounding, "southbc", sprintf("%.6f", extent[["ymin"]]))
  keywords <- xml2::xml_add_child(idinfo, "keywords")
  theme <- xml2::xml_add_child(keywords, "theme")
  xml_add_text(theme, "themekt", "None")
  for (term in c("LiDAR", "vegetation structure", "structural stage", "lynx habitat", "MSF", "Stem Exclusion", "Stand Initiation")) xml_add_text(theme, "themekey", term)
  xml_add_text(idinfo, "accconst", "None")
  xml_add_text(idinfo, "useconst", info$use)
  add_contact(idinfo)

  dataqual <- xml2::xml_add_child(doc, "dataqual")
  attracc <- xml2::xml_add_child(dataqual, "attracc")
  xml_add_text(attracc, "attraccr", "See lineage and attribute definitions.")
  lineage <- xml2::xml_add_child(dataqual, "lineage")
  procstep <- xml2::xml_add_child(lineage, "procstep")
  xml_add_text(procstep, "procdesc", info$lineage)
  xml_add_text(procstep, "procdate", metadata_date)

  spref <- xml2::xml_add_child(doc, "spref")
  horizsys <- xml2::xml_add_child(spref, "horizsys")
  planar <- xml2::xml_add_child(horizsys, "planar")
  mapproj <- xml2::xml_add_child(planar, "mapproj")
  xml_add_text(mapproj, "mapprojn", "Universal Transverse Mercator")
  grid <- xml2::xml_add_child(mapproj, "gridsys")
  xml_add_text(grid, "gridsysn", "Universal Transverse Mercator")
  zone <- if (!is.na(crs$epsg) && crs$epsg == 26911) "11" else "Not documented"
  xml_add_text(grid, "utmzone", zone)
  planci <- xml2::xml_add_child(planar, "planci")
  xml_add_text(planci, "plance", "row and column")
  xml_add_text(planci, "coordrep", "Coordinate pairs")
  xml_add_text(planci, "plandu", "meters")
  geodetic <- xml2::xml_add_child(horizsys, "geodetic")
  xml_add_text(geodetic, "horizdn", "North American Datum of 1983")
  xml_add_text(geodetic, "ellips", "GRS 1980")

  eainfo <- xml2::xml_add_child(doc, "eainfo")
  for (layer in layers) {
    layer_data <- sf::st_read(gpkg_path, layer = layer, quiet = TRUE)
    detailed <- xml2::xml_add_child(eainfo, "detailed")
    enttyp <- xml2::xml_add_child(detailed, "enttyp")
    xml_add_text(enttyp, "enttypl", layer)
    xml_add_text(enttyp, "enttypd", layer_description(key, layer))
    for (field in names(sf::st_drop_geometry(layer_data))) {
      attribute <- xml2::xml_add_child(detailed, "attr")
      xml_add_text(attribute, "attrlabl", field)
      xml_add_text(attribute, "attrdef", field_description(field))
      xml_add_text(attribute, "attrdefs", "Project data record and derived audit documentation")
    }
  }

  metainfo <- xml2::xml_add_child(doc, "metainfo")
  xml_add_text(metainfo, "metd", metadata_date)
  xml_add_text(metainfo, "metstdn", "FGDC Content Standard for Digital Geospatial Metadata")
  xml_add_text(metainfo, "metstdv", "FGDC-STD-001-1998")
  add_contact(metainfo)

  output_path <- sub("\\.gpkg$", ".xml", gpkg_path)
  xml2::write_xml(doc, output_path, options = "format")
  message("Wrote ", output_path)
}

for (key in names(dataset_info)) write_fgdc_metadata(key, dataset_info[[key]])
