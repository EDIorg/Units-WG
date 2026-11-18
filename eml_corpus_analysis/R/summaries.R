#' Read reviewable concept seed patterns
#'
#' Seeds are case-insensitive ICU regular expressions over attribute names,
#' labels, and definitions. They are a transparent first pass, not a
#' classification of record; see `docs/methods.md`.
#'
#' @param path Path to `concept_seeds.csv`.
#' @return Tibble of seeds with `concept`, `label`, `include_pattern`,
#'   `exclude_pattern`, `qudt_qk_hint`, and `notes`.
read_concept_seeds <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
}

#' Match attributes to seed concepts
#'
#' An attribute may match several concepts; matches are not exclusive.
#'
#' @param units_tbl Attributes from [annotate_units()].
#' @param seeds Output of [read_concept_seeds()].
#' @return Tibble with `attribute_id` and `concept`.
match_concepts <- function(units_tbl, seeds) {
  text <- stringr::str_to_lower(paste(
    dplyr::coalesce(units_tbl$attribute_name, ""),
    dplyr::coalesce(units_tbl$attribute_label, ""),
    dplyr::coalesce(units_tbl$attribute_definition, "")
  ))
  purrr::pmap(seeds, function(concept, include_pattern, exclude_pattern, ...) {
    hit <- stringr::str_detect(text, include_pattern)
    if (!is.na(exclude_pattern)) {
      hit <- hit & !stringr::str_detect(text, exclude_pattern)
    }
    tibble::tibble(attribute_id = units_tbl$attribute_id[hit], concept = concept)
  }) |>
    purrr::list_rbind()
}

#' Summarise the corpus for the overview page
#'
#' @param datasets Dataset table from the parser.
#' @param units_tbl Attributes from [annotate_units()].
#' @param annotations Annotation table from the parser.
#' @return A list of headline counts and small tables (`eml_versions`,
#'   `scopes`, `mapping`, `dataset_units`).
summarise_corpus <- function(datasets, units_tbl, annotations) {
  with_units <- dplyr::filter(units_tbl, unit_kind != "none")
  unit_datasets <- with_units |>
    dplyr::summarise(datasets = dplyr::n_distinct(package_id), .by = unit_group)
  list(
    files = nrow(datasets),
    parse_errors = sum(!is.na(datasets$parse_error)),
    scopes_n = dplyr::n_distinct(datasets$scope, na.rm = TRUE),
    attributes = nrow(units_tbl),
    standard_unit_uses = sum(units_tbl$unit_kind == "standard"),
    custom_unit_uses = sum(units_tbl$unit_kind == "custom"),
    unit_names = dplyr::n_distinct(with_units$unit_key),
    unit_groups = nrow(unit_datasets),
    single_dataset_groups = sum(unit_datasets$datasets == 1),
    groups_100_plus = sum(unit_datasets$datasets >= 100),
    share_uses_mapped = mean(!is.na(with_units$qudt_unit)),
    annotated_attributes = dplyr::n_distinct(annotations$attribute_id, na.rm = TRUE),
    annotated_datasets = dplyr::n_distinct(annotations$package_id),
    eml_versions = dplyr::count(datasets, eml_version, sort = TRUE, name = "files"),
    scopes = datasets |>
      dplyr::summarise(
        datasets = dplyr::n(), attributes = sum(n_attributes, na.rm = TRUE), .by = scope
      ) |>
      dplyr::arrange(dplyr::desc(datasets)),
    mapping = with_units |>
      dplyr::summarise(
        uses = dplyr::n(), unit_names = dplyr::n_distinct(unit_key), .by = mapping_source
      ) |>
      dplyr::mutate(share_uses = uses / sum(uses)) |>
      dplyr::arrange(dplyr::desc(uses)),
    dataset_units = dplyr::count(unit_datasets, datasets, name = "unit_groups")
  )
}

#' Summarise unit usage across the corpus
#'
#' Units are grouped by `unit_group`: the verified QUDT unit when one is
#' mapped, otherwise the spelling-insensitive match key.
#'
#' @param units_tbl Attributes from [annotate_units()].
#' @return One row per unit group, ordered by uses.
summarise_units <- function(units_tbl) {
  units_tbl |>
    dplyr::filter(unit_kind != "none") |>
    dplyr::summarise(
      uses = dplyr::n(),
      datasets = dplyr::n_distinct(package_id),
      scopes = dplyr::n_distinct(scope),
      variants = dplyr::n_distinct(unit_key),
      top_names = top_values(unit, 3),
      kind = paste(sort(unique(unit_kind)), collapse = "+"),
      qudt_unit = first_present(qudt_unit),
      mapping_source = paste(sort(unique(mapping_source)), collapse = "+"),
      dimension_vector = first_present(dimension_vector),
      distinct_definitions = dplyr::n_distinct(stringr::str_to_lower(attribute_definition)),
      .by = unit_group
    ) |>
    dplyr::arrange(dplyr::desc(uses))
}

#' Summarise dimension-vector families
#'
#' A family is the set of units sharing a QUDT DimensionVector; QUDT QKs
#' attached to the family's units are weighted by corpus uses so the label
#' reflects what the units are used for in QUDT, not only what exists.
#'
#' @param units_tbl Attributes from [annotate_units()].
#' @param qudt_units Output of [fetch_qudt_units()].
#' @return One row per dimension vector.
summarise_families <- function(units_tbl, qudt_units) {
  mapped <- dplyr::filter(units_tbl, !is.na(dimension_vector))
  unit_uses <- dplyr::count(mapped, dimension_vector, qudt_unit, name = "uses")
  qk_weights <- unit_uses |>
    dplyr::inner_join(
      dplyr::distinct(dplyr::filter(qudt_units, !is.na(qk)), qudt_unit, qk),
      by = "qudt_unit", relationship = "many-to-many"
    ) |>
    dplyr::summarise(uses = sum(uses), .by = c(dimension_vector, qk)) |>
    dplyr::arrange(dplyr::desc(uses)) |>
    dplyr::summarise(
      top_qudt_qks = paste0(utils::head(qk, 4), collapse = ", "),
      .by = dimension_vector
    )
  mapped |>
    dplyr::summarise(
      uses = dplyr::n(),
      datasets = dplyr::n_distinct(package_id),
      scopes = dplyr::n_distinct(scope),
      units = dplyr::n_distinct(qudt_unit),
      top_units = top_values(qudt_unit, 5),
      .by = dimension_vector
    ) |>
    dplyr::left_join(qk_weights, by = "dimension_vector") |>
    dplyr::arrange(dplyr::desc(uses))
}

#' Summarise how many ecological concepts each unit serves
#'
#' The polysemy index is the normalized Shannon entropy of seed-concept
#' dataset counts for a unit (0 = one concept; 1 = evenly spread).
#'
#' @param units_tbl Attributes from [annotate_units()].
#' @param concept_hits Output of [match_concepts()].
#' @param min_datasets Minimum datasets for a unit group to be summarised.
#' @return One row per unit group.
summarise_polysemy <- function(units_tbl, concept_hits, min_datasets = 20) {
  with_units <- dplyr::filter(units_tbl, unit_kind != "none")
  eligible <- with_units |>
    dplyr::summarise(datasets = dplyr::n_distinct(package_id), .by = unit_group) |>
    dplyr::filter(datasets >= min_datasets)
  by_concept <- concept_hits |>
    dplyr::inner_join(dplyr::select(with_units, attribute_id, unit_group, package_id),
      by = "attribute_id"
    ) |>
    dplyr::semi_join(eligible, by = "unit_group") |>
    dplyr::summarise(concept_datasets = dplyr::n_distinct(package_id), .by = c(unit_group, concept))
  concept_stats <- by_concept |>
    dplyr::arrange(unit_group, dplyr::desc(concept_datasets)) |>
    dplyr::summarise(
      concepts = sum(concept_datasets >= 2),
      polysemy_index = normalized_entropy(concept_datasets),
      top_concepts = paste0(
        utils::head(concept, 5), " (", utils::head(concept_datasets, 5), ")",
        collapse = ", "
      ),
      .by = unit_group
    )
  with_units |>
    dplyr::semi_join(eligible, by = "unit_group") |>
    dplyr::mutate(matched = attribute_id %in% concept_hits$attribute_id) |>
    dplyr::summarise(
      datasets = dplyr::n_distinct(package_id),
      attributes = dplyr::n(),
      share_matched = mean(matched),
      dimension_vector = first_present(dimension_vector),
      top_terms = top_terms(attribute_definition),
      .by = unit_group
    ) |>
    dplyr::left_join(concept_stats, by = "unit_group") |>
    dplyr::mutate(concepts = dplyr::coalesce(concepts, 0L)) |>
    dplyr::arrange(dplyr::desc(concepts), dplyr::desc(datasets))
}

#' Summarise units used for each seed concept
#'
#' @param units_tbl Attributes from [annotate_units()].
#' @param concept_hits Output of [match_concepts()].
#' @return A list with `by_unit` (concept x unit group), `by_dimension`
#'   (concept x dimension vector), and `overview` (one row per concept).
summarise_concept_units <- function(units_tbl, concept_hits) {
  hits <- concept_hits |>
    dplyr::inner_join(
      dplyr::filter(units_tbl, unit_kind != "none") |>
        dplyr::select(
          attribute_id, package_id, scope, unit, unit_group, qudt_unit, dimension_vector
        ),
      by = "attribute_id"
    )
  by_unit <- hits |>
    dplyr::summarise(
      attributes = dplyr::n(),
      datasets = dplyr::n_distinct(package_id),
      scopes = dplyr::n_distinct(scope),
      top_names = top_values(unit, 3),
      qudt_unit = first_present(qudt_unit),
      dimension_vector = first_present(dimension_vector),
      .by = c(concept, unit_group)
    ) |>
    dplyr::arrange(concept, dplyr::desc(datasets))
  by_dimension <- hits |>
    dplyr::mutate(dimension_vector = dplyr::coalesce(dimension_vector, "unmapped")) |>
    dplyr::summarise(
      attributes = dplyr::n(),
      datasets = dplyr::n_distinct(package_id),
      scopes = dplyr::n_distinct(scope),
      units = dplyr::n_distinct(unit_group),
      top_units = top_values(unit_group, 5),
      .by = c(concept, dimension_vector)
    ) |>
    dplyr::arrange(concept, dplyr::desc(datasets))
  overview <- hits |>
    dplyr::summarise(
      attributes = dplyr::n(),
      datasets = dplyr::n_distinct(package_id),
      scopes = dplyr::n_distinct(scope),
      unit_groups = dplyr::n_distinct(unit_group),
      dimension_vectors = dplyr::n_distinct(dimension_vector, na.rm = TRUE),
      share_qudt_mapped = mean(!is.na(qudt_unit)),
      top_units = top_values(unit_group, 6),
      .by = concept
    ) |>
    dplyr::arrange(dplyr::desc(datasets))
  list(by_unit = by_unit, by_dimension = by_dimension, overview = overview)
}

#' Retrieve corpus evidence for candidates from a QUDT generation run
#'
#' Candidates are bridged to EML units in two ways: the RCode_JP
#' pseudo-qname and the spelling-insensitive match key.
#'
#' @param units_tbl Attributes from [annotate_units()].
#' @param decisions_path Path to a run's `decisions.csv`.
#' @param examples_per_candidate Distinct example definitions per withheld
#'   candidate.
#' @return A list with `summary` (one row per candidate) and `examples`.
summarise_mob_evidence <- function(units_tbl, decisions_path, examples_per_candidate = 5) {
  decisions <- readr::read_csv(decisions_path, show_col_types = FALSE) |>
    dplyr::select(record_id, unit_expression, status, quantity_kind_uris) |>
    dplyr::mutate(candidate_key = qname_match_key(unit_expression))
  with_units <- dplyr::filter(units_tbl, unit_kind != "none")
  by_jp <- with_units |>
    dplyr::inner_join(decisions, by = c("jp_qname" = "unit_expression"), keep = TRUE) |>
    dplyr::mutate(bridge = "rcode_jp")
  by_key <- with_units |>
    dplyr::inner_join(decisions, by = c("match_key" = "candidate_key"), keep = TRUE) |>
    dplyr::mutate(bridge = "match_key")
  hits <- dplyr::bind_rows(by_jp, by_key) |>
    dplyr::mutate(
      bridge = paste(sort(unique(bridge)), collapse = "+"),
      .by = c(record_id, attribute_id)
    ) |>
    dplyr::distinct(record_id, attribute_id, .keep_all = TRUE)
  summary <- decisions |>
    dplyr::left_join(
      hits |>
        dplyr::summarise(
          bridge = paste(sort(unique(unlist(stringr::str_split(bridge, "\\+")))), collapse = "+"),
          eml_unit_names = top_values(unit, 3),
          uses = dplyr::n(),
          datasets = dplyr::n_distinct(package_id),
          scopes = dplyr::n_distinct(scope),
          top_terms = top_terms(attribute_definition, 6),
          .by = record_id
        ),
      by = "record_id"
    ) |>
    dplyr::mutate(
      dplyr::across(c(uses, datasets, scopes), \(x) dplyr::coalesce(x, 0L)),
      quantity_kind = local_name(quantity_kind_uris)
    ) |>
    dplyr::select(-quantity_kind_uris) |>
    dplyr::arrange(status, dplyr::desc(datasets))
  examples <- hits |>
    dplyr::filter(status %in% c("needs-clarification", "blocked-conversion")) |>
    dplyr::distinct(record_id, attribute_definition, .keep_all = TRUE) |>
    dplyr::slice_head(n = examples_per_candidate, by = record_id) |>
    dplyr::transmute(
      candidate = unit_expression, status, package_id, unit,
      attribute = stringr::str_trunc(attribute_name, 60),
      definition = stringr::str_trunc(attribute_definition, 240)
    ) |>
    dplyr::arrange(status, candidate)
  list(summary = summary, examples = examples)
}

#' Classify an annotation URI by vocabulary
#'
#' @param uri Character vector of annotation value or property URIs.
#' @return Character vector of vocabulary names.
annotation_vocabulary <- function(uri) {
  dplyr::case_when(
    stringr::str_detect(uri, "qudt\\.org") ~ "QUDT",
    stringr::str_detect(uri, "ECSO") ~ "ECSO",
    stringr::str_detect(uri, "ENVO") ~ "ENVO",
    stringr::str_detect(uri, "PATO") ~ "PATO",
    stringr::str_detect(uri, "obolibrary") ~ "Other OBO",
    stringr::str_detect(uri, "rs\\.tdwg\\.org/dwc") ~ "Darwin Core",
    stringr::str_detect(uri, "oboe") ~ "OBOE",
    is.na(uri) ~ "missing",
    .default = "Other"
  )
}

#' Summarise existing semantic annotations
#'
#' @param annotations Annotation table from the parser.
#' @param units_tbl Attributes from [annotate_units()].
#' @return A list with `vocabularies`, `properties`, `values` (non-QUDT
#'   values), and `measured_types` (values of "contains measurements of
#'   type" annotations with the units they are used with).
summarise_annotations <- function(annotations, units_tbl) {
  labelled <- annotations |>
    dplyr::mutate(
      vocabulary = annotation_vocabulary(value_uri),
      value = dplyr::coalesce(value_label, local_name(value_uri)),
      property = dplyr::coalesce(property_label, local_name(property_uri))
    )
  measured_types <- labelled |>
    dplyr::filter(stringr::str_detect(
      stringr::str_to_lower(paste(property, property_uri)),
      "containsmeasurementsoftype|contains measurements of type"
    )) |>
    dplyr::inner_join(
      dplyr::select(units_tbl, attribute_id, unit_group, dimension_vector),
      by = "attribute_id"
    ) |>
    dplyr::summarise(
      attributes = dplyr::n(),
      datasets = dplyr::n_distinct(package_id),
      units = top_values(unit_group, 4),
      .by = c(vocabulary, value, value_uri)
    ) |>
    dplyr::arrange(dplyr::desc(datasets))
  list(
    vocabularies = labelled |>
      dplyr::summarise(
        annotations = dplyr::n(), datasets = dplyr::n_distinct(package_id),
        .by = c(subject_level, vocabulary)
      ) |>
      dplyr::arrange(subject_level, dplyr::desc(annotations)),
    properties = labelled |>
      dplyr::summarise(
        annotations = dplyr::n(), datasets = dplyr::n_distinct(package_id),
        .by = c(property, property_uri)
      ) |>
      dplyr::arrange(dplyr::desc(datasets)),
    values = labelled |>
      dplyr::filter(vocabulary != "QUDT") |>
      dplyr::summarise(
        annotations = dplyr::n(), datasets = dplyr::n_distinct(package_id),
        .by = c(vocabulary, value, value_uri)
      ) |>
      dplyr::arrange(dplyr::desc(datasets)),
    measured_types = measured_types
  )
}

#' Count informative definition terms
#'
#' A transparent bag-of-words summary used before any TF-IDF or LLM step.
#'
#' @param definitions Character vector of attribute definitions.
#' @param n Number of terms to return.
#' @return A string of `term (count)` pairs separated by commas.
top_terms <- function(definitions, n = 8) {
  tokens <- stringr::str_split(stringr::str_to_lower(definitions), "[^a-z0-9]+")
  tokens <- unlist(tokens, use.names = FALSE)
  keep <- nchar(tokens) > 2 &
    !tokens %in% definition_stop_words() &
    !stringr::str_detect(tokens, "^[0-9]+$")
  tokens <- tokens[keep]
  if (length(tokens) == 0) {
    return("")
  }
  counts <- utils::head(sort(table(tokens), decreasing = TRUE), n)
  paste0(names(counts), " (", as.integer(counts), ")", collapse = ", ")
}

#' Stop words for attribute definitions
#'
#' English stop words plus metadata boilerplate that carries no concept.
#'
#' @return Character vector.
definition_stop_words <- function() {
  c(
    tidytext::stop_words$word,
    "per", "unit", "units", "value", "values", "measured", "measurement", "total",
    "mean", "average", "sample", "samples", "data", "column", "variable", "recorded"
  )
}

top_values <- function(x, n = 3) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  counts <- utils::head(sort(table(x), decreasing = TRUE), n)
  paste0(names(counts), " (", as.integer(counts), ")", collapse = ", ")
}

first_present <- function(x) {
  dplyr::first(x[!is.na(x)])
}

normalized_entropy <- function(counts) {
  if (length(counts) < 2) {
    return(0)
  }
  p <- counts / sum(counts)
  -sum(p * log(p)) / log(length(p))
}
