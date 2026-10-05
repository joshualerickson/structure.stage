# Render the KNF stakeholder handout from its Markdown source.
#
# Edit `dev/knf_steering_group_handout.md`, then run:
#   Rscript dev/render_knf_handout.R
#
# The resulting HTML embeds local figures so it can be emailed as one file.

script_argument <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_argument[grepl("^--file=", script_argument)][1])
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source_path <- file.path(root, "dev", "knf_steering_group_handout.md")
output_dir <- file.path(root, "outputs", "handouts")
output_path <- file.path(output_dir, "KNF_LiDAR_structural_stage_handout.html")

embed_local_images <- function(html, source_dir) {
  image_tags <- regmatches(html, gregexpr('<img[^>]+src="[^"]+"[^>]*>', html, perl = TRUE))[[1]]
  for (tag in image_tags) {
    source_match <- regmatches(tag, regexpr('src="[^"]+"', tag, perl = TRUE))
    relative_path <- substr(source_match, 6L, nchar(source_match) - 1L)
    image_path <- normalizePath(file.path(source_dir, relative_path), mustWork = TRUE)
    extension <- tolower(tools::file_ext(image_path))
    mime <- switch(extension, png = "image/png", svg = "image/svg+xml", jpeg = "image/jpeg", jpg = "image/jpeg", stop("Unsupported image type: ", extension))
    uri <- paste0("data:", mime, ";base64,", base64enc::base64encode(image_path))
    html <- sub(tag, sub(source_match, paste0('src="', uri, '"'), tag, fixed = TRUE), html, fixed = TRUE)
  }
  html
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
markdown <- paste(readLines(source_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
body <- commonmark::markdown_html(markdown, extensions = "table")
body <- embed_local_images(body, dirname(source_path))

css <- paste(
  "@page { size: letter; margin: 0.55in; }",
  "body { color: #17212b; font: 10.5pt Arial, Helvetica, sans-serif; line-height: 1.35; max-width: 7.4in; margin: auto; }",
  "h1 { color: #123a5a; font-size: 22pt; line-height: 1.08; margin: 0 0 6pt; }",
  "h2 { color: #123a5a; border-bottom: 1px solid #a8bac7; font-size: 14pt; margin: 16pt 0 6pt; padding-bottom: 2pt; }",
  "h3 { color: #123a5a; font-size: 11.5pt; margin: 13pt 0 5pt; }",
  "p { margin: 5pt 0; }",
  "table { border-collapse: collapse; margin: 7pt 0 9pt; width: 100%; font-size: 9pt; }",
  "th { background: #123a5a; color: white; text-align: left; }",
  "th, td { border: 1px solid #b7c4cc; padding: 5pt; vertical-align: top; }",
  "tr:nth-child(even) { background: #f4f7f8; }",
  "img { display: block; width: 100%; height: auto; margin: 8pt 0 4pt; page-break-inside: avoid; }",
  "ul, ol { margin: 5pt 0 5pt 18pt; padding: 0; }",
  "li { margin: 2pt 0; }",
  sep = "\n"
)

html <- paste0(
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
  "<title>LiDAR structural-stage mapping: supplemental decision support</title><style>",
  css,
  "</style></head><body>",
  body,
  "</body></html>"
)
writeLines(html, output_path, useBytes = TRUE)
message(normalizePath(output_path))
