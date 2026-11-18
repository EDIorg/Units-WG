# Shared setup for report pages. Pages read pipeline results with
# targets::tar_read(..., store = store) so tarchetypes::tar_quarto() can
# detect dependencies.

# here::here() would stop at report/ (it treats _quarto.yml as a root), so
# locate the analysis package root explicitly.
pkg_root <- rprojroot::find_root(rprojroot::is_r_package)
store <- file.path(pkg_root, "_targets")
docs_url <- "https://github.com/EDIorg/Units-WG/blob/agentic/eml_corpus_analysis/docs"
derived_url <- "https://github.com/EDIorg/Units-WG/blob/agentic/eml_corpus_analysis/data/derived"

#' Interactive table with a CSV download button
#'
#' @param data Data frame.
#' @param id Unique element id on the page; also the download file name.
#' @param ... Passed to reactable::reactable().
interactive_table <- function(data, id, ...) {
  table <- reactable::reactable(
    data,
    elementId = id,
    searchable = TRUE,
    filterable = TRUE,
    resizable = TRUE,
    compact = TRUE,
    striped = TRUE,
    defaultPageSize = 15,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(15, 50, 200),
    defaultColDef = reactable::colDef(minWidth = 110),
    ...
  )
  htmltools::browsable(htmltools::tagList(
    htmltools::tags$button(
      "Download CSV (current filters)",
      class = "btn btn-sm btn-outline-secondary mb-2",
      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s.csv')", id, id)
    ),
    table
  ))
}

wide <- function(width = 320) reactable::colDef(minWidth = width)

pct <- function(x, digits = 1) paste0(format(round(100 * x, digits), nsmall = digits), "%")

num <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

doc_link <- function(file, text = file) sprintf("[%s](%s/%s)", text, docs_url, file)

#' Crosstalk filters in a row above a full-width table
#'
#' @param filters One crosstalk filter or a list of them.
#' @param table An htmlwidget (usually reactable) built on the same SharedData.
filtered_table <- function(filters, table) {
  if (inherits(filters, c("shiny.tag", "shiny.tag.list"))) {
    filters <- list(filters)
  }
  widths <- rep(min(4, 12 %/% length(filters)), length(filters))
  htmltools::tagList(do.call(crosstalk::bscols, c(list(widths = widths), filters)), table)
}
