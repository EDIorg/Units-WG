#' Apply QK admission criteria to concept x dimension-vector evidence
#'
#' Encodes the measurable parts of the draft criteria in
#' `docs/decision-log.md`. Criteria that need human judgment (an
#' authoritative operational definition, a QUDT community check, audit
#' precision) are reported as open checks, never assumed to pass.
#'
#' @param concept_dimensions `by_dimension` from [summarise_concept_units()].
#' @param seeds Output of [read_concept_seeds()].
#' @param qudt_qks Output of [fetch_qudt_quantity_kinds()].
#' @param parents Output of [suggest_parent_qks()].
#' @param min_datasets,min_scopes Recurrence thresholds.
#' @return One row per concept and dimension vector with `status` and
#'   supporting evidence, ordered for review.
assess_qk_candidates <- function(concept_dimensions, seeds, qudt_qks, parents,
                                 min_datasets = 20, min_scopes = 3) {
  qks <- qudt_qks |>
    dplyr::filter(!is.na(dimension_vector)) |>
    dplyr::distinct(qk, qk_label, dimension_vector) |>
    dplyr::mutate(search_text = stringr::str_to_lower(paste(
      qk_label, stringr::str_replace_all(qk, "([a-z])([A-Z])", "\\1 \\2")
    )))
  concept_dimensions |>
    dplyr::filter(dimension_vector != "unmapped") |>
    dplyr::left_join(dplyr::select(seeds, concept, label, qudt_qk_hint), by = "concept") |>
    dplyr::mutate(
      qudt_qks_with_vector = purrr::map_int(
        dimension_vector, \(dv) sum(qks$dimension_vector == dv)
      ),
      existing_qk_matches = purrr::map2_chr(dimension_vector, qudt_qk_hint, \(dv, hint) {
        if (is.na(hint) || !nzchar(hint)) {
          return(NA_character_)
        }
        dv_qks <- qks[qks$dimension_vector == dv, ]
        found <- dv_qks$qk[stringr::str_detect(dv_qks$search_text, hint)]
        if (length(found) == 0) {
          return(NA_character_)
        }
        paste(utils::head(sort(unique(found)), 6), collapse = ", ")
      }),
      meets_recurrence = datasets >= min_datasets & scopes >= min_scopes,
      # QUDT marks dimensionless vectors with D1 (e.g. A0E0L0I0M0H0T0D1).
      dimensionless = stringr::str_detect(dimension_vector, "^A0E0L0I0M0H0T0D[01]$"),
      status = dplyr::case_when(
        !meets_recurrence ~ "below-threshold",
        !is.na(existing_qk_matches) ~ "reuse-or-specialize-existing-qk",
        dimensionless ~ "dimensionless-needs-ratio-semantics",
        .default = "candidate-new-qk"
      ),
      open_checks = paste0(
        "quantity type (not substance or entity); authoritative definition; ",
        "audit precision; QUDT community check",
        dplyr::if_else(dimensionless, "; ratio semantics", "")
      )
    ) |>
    dplyr::mutate(
      vectors_meeting_recurrence = sum(meets_recurrence),
      # Share of the concept's dataset-vector occurrences (mapped units only).
      # Low shares often signal co-located ancillary variables (sensor depth,
      # probe temperature) rather than the concept itself; see the audit.
      vector_share = round(datasets / sum(datasets), 3),
      .by = concept
    ) |>
    dplyr::left_join(parents, by = c("concept", "dimension_vector")) |>
    dplyr::mutate(status_rank = match(status, c(
      "candidate-new-qk", "reuse-or-specialize-existing-qk",
      "dimensionless-needs-ratio-semantics", "below-threshold"
    ))) |>
    dplyr::arrange(status_rank, dplyr::desc(datasets)) |>
    dplyr::select(-status_rank)
}

#' Suggest generic parent QKs for concept x dimension-vector pairs
#'
#' The parent is the QUDT QK most often attached (via
#' `unitForQuantityKind` or legacy `hasQuantityKind`) to the units the corpus
#' uses for the concept, weighted by datasets, restricted to QKs sharing the
#' pair's dimension vector so `qudt:specializationOf` remains commensurate.
#'
#' @param concept_units `by_unit` from [summarise_concept_units()].
#' @param qudt_units Output of [fetch_qudt_units()].
#' @param qudt_qks Output of [fetch_qudt_quantity_kinds()].
#' @return Tibble with `concept`, `dimension_vector`, and `suggested_parent_qk`.
suggest_parent_qks <- function(concept_units, qudt_units, qudt_qks) {
  qk_vectors <- dplyr::distinct(qudt_qks, qk, qk_vector = dimension_vector)
  concept_units |>
    dplyr::filter(!is.na(qudt_unit), !is.na(dimension_vector)) |>
    dplyr::inner_join(
      qudt_units |>
        dplyr::filter(qk_relation %in% c("unitForQuantityKind", "hasQuantityKind")) |>
        dplyr::distinct(qudt_unit, qk),
      by = "qudt_unit", relationship = "many-to-many"
    ) |>
    dplyr::inner_join(qk_vectors, by = "qk", relationship = "many-to-many") |>
    dplyr::filter(qk_vector == dimension_vector) |>
    dplyr::summarise(weight = sum(datasets), .by = c(concept, dimension_vector, qk)) |>
    dplyr::arrange(dplyr::desc(weight)) |>
    dplyr::summarise(
      suggested_parent_qk = dplyr::first(qk),
      other_parent_options = paste(utils::head(qk[-1], 3), collapse = ", "),
      .by = c(concept, dimension_vector)
    )
}

#' Propose unit attachments for candidate or reused QKs
#'
#' A unit is proposed only when it is commensurate (same dimension vector by
#' construction) and the corpus uses it for the concept in at least
#' `attach_min_datasets` datasets. Dimensional equality alone is never
#' sufficient.
#'
#' @param candidates Output of [assess_qk_candidates()].
#' @param concept_units `by_unit` from [summarise_concept_units()].
#' @param qudt_units Output of [fetch_qudt_units()].
#' @param attach_min_datasets Minimum datasets using the unit for the concept.
#' @return One row per concept, dimension vector, and unit.
propose_unit_attachments <- function(candidates, concept_units, qudt_units,
                                     attach_min_datasets = 3) {
  current <- qudt_units |>
    dplyr::filter(!is.na(qk)) |>
    dplyr::summarise(
      current_qudt_qks = paste(sort(unique(qk)), collapse = ", "),
      .by = qudt_unit
    )
  candidates |>
    dplyr::filter(status %in% c("candidate-new-qk", "reuse-or-specialize-existing-qk")) |>
    dplyr::select(concept, dimension_vector, status, existing_qk_matches) |>
    dplyr::inner_join(
      concept_units |>
        dplyr::filter(!is.na(qudt_unit), datasets >= attach_min_datasets) |>
        dplyr::select(concept, dimension_vector, qudt_unit, datasets, scopes),
      by = c("concept", "dimension_vector")
    ) |>
    dplyr::left_join(current, by = "qudt_unit") |>
    dplyr::mutate(
      target_qk = dplyr::if_else(
        status == "reuse-or-specialize-existing-qk", existing_qk_matches,
        paste0("new: ", concept)
      ),
      already_attached = purrr::map2_lgl(
        current_qudt_qks, existing_qk_matches, \(cur, matches) {
          if (is.na(matches) || is.na(cur)) {
            return(FALSE)
          }
          any(stringr::str_split_1(matches, ", ") %in% stringr::str_split_1(cur, ", "))
        }
      ),
      proposed_relation = dplyr::if_else(already_attached, "none (present)", "unitForQuantityKind")
    ) |>
    dplyr::arrange(concept, dimension_vector, dplyr::desc(datasets))
}

#' Read the curated QK shortlist
#'
#' The shortlist (`data/reference/qk_shortlist.csv`) is the working group's
#' reviewable synthesis of the candidate table: one proposal per row and one
#' DimensionVector per proposal. `concepts` lists seed concepts separated by
#' semicolons.
#'
#' @param path Path to the shortlist CSV.
#' @return Tibble of shortlist rows.
read_qk_shortlist <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
}

#' Attach live corpus evidence to the curated QK shortlist
#'
#' Fails when a row names an unknown seed concept or a QUDT QK that does not
#' exist, so the shortlist cannot drift from the seeds or from QUDT.
#'
#' @param shortlist Output of [read_qk_shortlist()].
#' @param units_tbl Attributes from [annotate_units()].
#' @param concept_hits Output of [match_concepts()].
#' @param seeds Output of [read_concept_seeds()].
#' @param qudt_qks Output of [fetch_qudt_quantity_kinds()].
#' @return The shortlist with `attributes`, `datasets`, `scopes` (union over
#'   the row's concepts within its DimensionVector), and `top_units`.
summarise_qk_shortlist <- function(shortlist, units_tbl, concept_hits, seeds, qudt_qks) {
  long <- shortlist |>
    dplyr::select(proposal_id, dimension_vector, concepts) |>
    tidyr::separate_longer_delim(concepts, ";") |>
    dplyr::mutate(concept = stringr::str_trim(concepts), .keep = "unused")
  unknown_concepts <- setdiff(long$concept, seeds$concept)
  if (length(unknown_concepts) > 0) {
    stop("Unknown seed concepts in QK shortlist: ", paste(unknown_concepts, collapse = ", "))
  }
  unknown_qks <- setdiff(shortlist$qudt_qk, qudt_qks$qk)
  if (length(unknown_qks) > 0) {
    stop("QUDT QKs in shortlist not found in QUDT: ", paste(unknown_qks, collapse = ", "))
  }
  evidence <- long |>
    dplyr::inner_join(concept_hits, by = "concept", relationship = "many-to-many") |>
    dplyr::inner_join(
      dplyr::select(units_tbl, attribute_id, package_id, scope, unit_group, dimension_vector),
      by = c("attribute_id", "dimension_vector")
    ) |>
    dplyr::distinct(proposal_id, attribute_id, package_id, scope, unit_group) |>
    dplyr::summarise(
      attributes = dplyr::n(),
      datasets = dplyr::n_distinct(package_id),
      scopes = dplyr::n_distinct(scope),
      top_units = top_values(unit_group, 5),
      .by = proposal_id
    )
  shortlist |>
    dplyr::left_join(evidence, by = "proposal_id") |>
    dplyr::mutate(dplyr::across(c(attributes, datasets, scopes), \(x) dplyr::coalesce(x, 0L)))
}
