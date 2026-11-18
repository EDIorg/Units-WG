#' Build the corpus manifest
#'
#' Records every file's identifier, EML version, size, SHA-256, and parse
#' status so a later snapshot can be compared file-by-file.
#'
#' @param datasets Dataset table from the parser.
#' @param snapshot Corpus snapshot date (from config).
#' @return One row per corpus file.
corpus_manifest <- function(datasets, snapshot) {
  datasets |>
    dplyr::transmute(
      file, package_id, eml_version, bytes, sha256,
      parse_status = dplyr::if_else(is.na(parse_error), "ok", "error"),
      snapshot = snapshot
    ) |>
    dplyr::arrange(file)
}

#' Build a run manifest for provenance
#'
#' @param config_path Path to `config.yml`.
#' @param seeds_path Path to the concept seeds.
#' @param prompt_path Path to the LLM prompt.
#' @param qudt_units Output of [fetch_qudt_units()] (for retrieval time).
#' @param manifest Output of [corpus_manifest()].
#' @param cfg Active configuration list.
#' @return A named list suitable for JSON serialization.
run_manifest <- function(config_path, seeds_path, prompt_path, qudt_units, manifest, cfg) {
  git <- \(...) {
    out <- suppressWarnings(tryCatch(
      system2("git", c("-C", here::here(), ...), stdout = TRUE, stderr = FALSE),
      error = \(e) NA_character_
    ))
    if (length(out) == 0) "" else out
  }
  packages <- c(
    "targets", "tarchetypes", "crew", "xml2", "dplyr", "tidytext", "httr2",
    "ellmer", "quarto", "reactable", "plotly", "crosstalk"
  )
  list(
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    git_commit = git("rev-parse", "HEAD")[1],
    git_dirty = any(nzchar(git("status", "--porcelain"))),
    config_profile = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    config_sha256 = digest::digest(file = config_path, algo = "sha256"),
    concept_seeds_sha256 = digest::digest(file = seeds_path, algo = "sha256"),
    llm = list(
      enabled = isTRUE(cfg$llm$enabled), provider = cfg$llm$provider, model = cfg$llm$model,
      prompt_sha256 = digest::digest(file = prompt_path, algo = "sha256")
    ),
    corpus = list(
      snapshot = cfg$corpus$snapshot,
      source = cfg$corpus$source,
      files = nrow(manifest),
      parse_errors = sum(manifest$parse_status == "error"),
      sample_n = cfg$corpus$sample_n,
      manifest_sha256 = digest::digest(manifest$sha256, algo = "sha256")
    ),
    qudt = list(
      endpoint = cfg$qudt$sparql_endpoint,
      retrieved = qudt_units$retrieved[1],
      units = dplyr::n_distinct(qudt_units$qudt_unit)
    ),
    r_version = R.version.string,
    quarto_version = tryCatch(as.character(quarto::quarto_version()), error = \(e) NA_character_),
    packages = purrr::set_names(packages) |>
      purrr::map(\(p) tryCatch(as.character(utils::packageVersion(p)), error = \(e) NA_character_))
  )
}

#' Write a derived table as CSV and return its path
#'
#' @param x Data frame.
#' @param path Output path.
#' @return `path`, for use with `format = "file"` targets.
write_derived_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  path
}

#' Write a named list of tables as CSVs in one directory
#'
#' @param tables Named list of data frames; names become file names.
#' @param dir Output directory.
#' @return Character vector of written paths.
write_derived_tables <- function(tables, dir) {
  purrr::imap_chr(tables, \(x, name) write_derived_csv(x, file.path(dir, paste0(name, ".csv"))))
}

#' Write a list as pretty JSON and return its path
#'
#' @param x List.
#' @param path Output path.
#' @return `path`.
write_derived_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  path
}

#' Write the large attribute table to Parquet via DuckDB
#'
#' Keeps the full table queryable outside R without committing it.
#'
#' @param x Attribute table.
#' @param path Output Parquet path (gitignored work area).
#' @return `path`.
write_parquet_duckdb <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "x", x)
  target <- normalizePath(path, mustWork = FALSE)
  DBI::dbExecute(con, sprintf("COPY x TO '%s' (FORMAT PARQUET)", target))
  path
}
