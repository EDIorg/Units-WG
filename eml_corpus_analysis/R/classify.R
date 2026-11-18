#' Cluster attribute definitions within dimension-vector families
#'
#' Deterministic first step of concept classification. Distinct (case-folded)
#' definitions in each family are represented with TF-IDF over the family's
#' most common terms, L2-normalized, and grouped with seeded k-means. Large
#' families are sampled (seeded) to bound memory; unsampled definitions stay
#' unassigned and coverage is reported.
#'
#' @param units_tbl Attributes from [annotate_units()].
#' @param min_definitions Minimum distinct definitions for a family to be
#'   clustered.
#' @param max_definitions Maximum definitions sampled per family.
#' @param vocabulary Number of terms kept per family.
#' @param max_k Maximum clusters per family.
#' @param seed Random seed for sampling and k-means.
#' @return A list with `clusters` (one row per cluster) and `assignments`
#'   (`dimension_vector`, `definition_key`, `cluster_id`).
cluster_definitions <- function(units_tbl, min_definitions = 50, max_definitions = 3000,
                                vocabulary = 300, max_k = 12, seed = 20261112) {
  with_keys <- units_tbl |>
    dplyr::filter(!is.na(dimension_vector), !is.na(attribute_definition)) |>
    dplyr::mutate(definition_key = definition_key(attribute_definition))
  defs <- with_keys |>
    dplyr::summarise(
      attributes = dplyr::n(),
      definition = dplyr::first(attribute_definition),
      .by = c(dimension_vector, definition_key)
    )
  families <- defs |>
    dplyr::count(dimension_vector, name = "definitions") |>
    dplyr::filter(definitions >= min_definitions)
  results <- purrr::map(families$dimension_vector, \(dv) {
    cluster_family(
      dplyr::filter(defs, dimension_vector == dv), max_definitions, vocabulary, max_k, seed
    )
  })
  assignments <- purrr::list_rbind(purrr::map(results, "assignments"))
  cluster_stats <- with_keys |>
    dplyr::inner_join(assignments, by = c("dimension_vector", "definition_key")) |>
    dplyr::summarise(
      attributes = dplyr::n(),
      datasets = dplyr::n_distinct(package_id),
      scopes = dplyr::n_distinct(scope),
      units = top_values(unit_group, 4),
      .by = cluster_id
    )
  clusters <- purrr::list_rbind(purrr::map(results, "clusters")) |>
    dplyr::left_join(cluster_stats, by = "cluster_id") |>
    dplyr::left_join(
      dplyr::rename(families, family_definitions = definitions),
      by = "dimension_vector"
    ) |>
    dplyr::arrange(dimension_vector, dplyr::desc(attributes))
  list(clusters = clusters, assignments = assignments)
}

#' Case-folded, whitespace-normalized definition key
#'
#' @param definition Character vector of attribute definitions.
#' @return Character vector.
definition_key <- function(definition) {
  stringr::str_to_lower(stringr::str_squish(definition))
}

cluster_family <- function(defs, max_definitions, vocabulary, max_k, seed) {
  set.seed(seed)
  if (nrow(defs) > max_definitions) {
    defs <- dplyr::slice_sample(defs, n = max_definitions)
  }
  defs <- dplyr::mutate(defs, doc = dplyr::row_number())
  tokens <- defs |>
    dplyr::select(doc, definition_key) |>
    tidytext::unnest_tokens(word, definition_key) |>
    dplyr::filter(
      nchar(word) > 2,
      !word %in% definition_stop_words(),
      !stringr::str_detect(word, "^[0-9.,]+$")
    )
  vocab <- tokens |>
    dplyr::distinct(doc, word) |>
    dplyr::count(word, sort = TRUE) |>
    dplyr::slice_head(n = vocabulary)
  tfidf <- tokens |>
    dplyr::semi_join(vocab, by = "word") |>
    dplyr::count(doc, word) |>
    tidytext::bind_tf_idf(word, doc, n)
  mat <- matrix(0, nrow(defs), nrow(vocab), dimnames = list(NULL, vocab$word))
  mat[cbind(tfidf$doc, match(tfidf$word, vocab$word))] <- tfidf$tf_idf
  norms <- sqrt(rowSums(mat^2))
  keep <- norms > 0
  mat <- mat[keep, , drop = FALSE] / norms[keep]
  distinct_rows <- nrow(unique(mat))
  k <- min(max_k, max(2, round(sqrt(nrow(mat) / 10))), distinct_rows - 1)
  if (k < 2) {
    return(list(clusters = NULL, assignments = NULL))
  }
  set.seed(seed)
  # Duplicate rows can make random starts non-distinct; fall back to fixed
  # distinct starting centres in that case.
  km <- tryCatch(
    stats::kmeans(mat, centers = k, nstart = 5, iter.max = 50),
    error = \(e) {
      starts <- unique(mat)[seq_len(k), , drop = FALSE]
      stats::kmeans(mat, centers = starts, iter.max = 50)
    }
  )
  dv <- defs$dimension_vector[1]
  ids <- sprintf("%s/c%02d", dv, km$cluster)
  kept <- defs[keep, ]
  clusters <- purrr::map(seq_len(k), \(cl) {
    members <- which(km$cluster == cl)
    centre <- km$centers[cl, ]
    similarity <- as.vector(mat[members, , drop = FALSE] %*% centre)
    exemplars <- kept$definition[members][order(similarity, decreasing = TRUE)]
    tibble::tibble(
      cluster_id = sprintf("%s/c%02d", dv, cl),
      dimension_vector = dv,
      definitions = length(members),
      top_terms = paste(names(utils::head(sort(centre, decreasing = TRUE), 8)), collapse = ", "),
      exemplars = paste(
        stringr::str_trunc(utils::head(unique(exemplars), 4), 200),
        collapse = " || "
      )
    )
  }) |>
    purrr::list_rbind()
  list(
    clusters = clusters,
    assignments = tibble::tibble(
      dimension_vector = dv, definition_key = kept$definition_key, cluster_id = ids
    )
  )
}

#' Label clusters with an LLM (opt-in)
#'
#' Sends each cluster's top terms, units, and exemplar definitions (public
#' EML metadata) to the configured provider and requests structured output.
#' Disabled unless `llm$enabled` is `TRUE`; API keys come from the provider's
#' environment variable (for example `OPENAI_API_KEY`), never from config.
#'
#' @param clusters `clusters` element of [cluster_definitions()].
#' @param llm List with `enabled`, `provider`, `model`, and `max_clusters`.
#' @param prompt_path Path to the versioned system prompt.
#' @param seed Seed passed to providers that support it.
#' @return One row per cluster with `llm_status` and, when labelled, the
#'   structured fields, model, and prompt hash.
label_clusters_llm <- function(clusters, llm, prompt_path, seed = 20261112) {
  if (!isTRUE(llm$enabled)) {
    return(tibble::tibble(cluster_id = clusters$cluster_id, llm_status = "not-run"))
  }
  prompt <- readr::read_file(prompt_path)
  chat_fun <- getExportedValue("ellmer", paste0("chat_", llm$provider))
  label_type <- ellmer::type_object(
    measured_concept = ellmer::type_string("Short name of the measured ecological quantity."),
    entity_or_medium = ellmer::type_string("Organism, material, or medium measured."),
    process = ellmer::type_string("Ecological process, or 'none' for a state variable."),
    denominator_semantics = ellmer::type_string(
      "What each denominator means (area, mass, time, count)."
    ),
    coherent = ellmer::type_boolean("TRUE when exemplars describe one quantity."),
    ecological_qk_candidate = ellmer::type_boolean(
      "TRUE when a QUDT QuantityKind could be justified."
    ),
    aligned_terms = ellmer::type_string("Matching ECSO, ENVO, or QUDT terms if known; else empty."),
    confidence = ellmer::type_enum(c("high", "medium", "low"), "Confidence in the label."),
    rationale = ellmer::type_string("One or two sentences citing exemplar evidence.")
  )
  selected <- clusters |>
    dplyr::arrange(dplyr::desc(attributes)) |>
    dplyr::slice_head(n = llm$max_clusters)
  purrr::map(seq_len(nrow(selected)), \(i) {
    row <- selected[i, ]
    chat <- chat_fun(
      system_prompt = prompt, model = llm$model,
      params = ellmer::params(temperature = 0, seed = seed), echo = "none"
    )
    result <- tryCatch(
      tibble::as_tibble(chat$chat_structured(format_cluster_prompt(row), type = label_type)) |>
        dplyr::mutate(llm_status = "labelled"),
      error = \(e) tibble::tibble(llm_status = paste("error:", conditionMessage(e)))
    )
    dplyr::mutate(result, cluster_id = row$cluster_id, .before = 1)
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(llm_model = llm$model, prompt_sha256 = digest::digest(prompt, algo = "sha256"))
}

format_cluster_prompt <- function(row) {
  paste0(
    "Dimension vector: ", row$dimension_vector, "\n",
    "Units used (uses): ", row$units, "\n",
    "Top terms: ", row$top_terms, "\n",
    "Attributes: ", row$attributes, "; datasets: ", row$datasets, "\n",
    "Exemplar definitions:\n- ", stringr::str_replace_all(row$exemplars, " \\|\\| ", "\n- ")
  )
}

#' Draw a stratified audit sample of attributes per cluster
#'
#' @param units_tbl Attributes from [annotate_units()].
#' @param assignments `assignments` element of [cluster_definitions()].
#' @param labels Output of [label_clusters_llm()].
#' @param per_cluster Attributes sampled per cluster.
#' @param seed Random seed.
#' @return Tibble for human review with blank reviewer columns.
draw_audit_sample <- function(units_tbl, assignments, labels, per_cluster = 5, seed = 20261112) {
  label_cols <- labels |>
    dplyr::select(cluster_id, dplyr::any_of(c("llm_status", "measured_concept")))
  has_labels <- "measured_concept" %in% names(label_cols)
  set.seed(seed)
  units_tbl |>
    dplyr::filter(!is.na(dimension_vector), !is.na(attribute_definition)) |>
    dplyr::mutate(definition_key = definition_key(attribute_definition)) |>
    dplyr::inner_join(assignments, by = c("dimension_vector", "definition_key")) |>
    dplyr::slice_sample(n = per_cluster, by = cluster_id) |>
    dplyr::left_join(label_cols, by = "cluster_id") |>
    dplyr::transmute(
      attribute_id, package_id, cluster_id, unit, unit_group,
      attribute_name = stringr::str_trunc(attribute_name, 80),
      definition = stringr::str_trunc(attribute_definition, 300),
      llm_measured_concept = if (has_labels) measured_concept else NA,
      reviewer_measured_concept = NA_character_,
      reviewer_agrees = NA_character_,
      reviewer_notes = NA_character_
    ) |>
    dplyr::arrange(cluster_id, attribute_id)
}

#' Summarise completed human audits
#'
#' @param path CSV of reviewed audit rows (`reviewer_agrees` as yes/no).
#' @return Agreement by dimension vector; empty when no rows are reviewed.
summarise_audit <- function(path) {
  reviewed <- path |>
    readr::read_csv(show_col_types = FALSE, col_types = readr::cols(.default = "c")) |>
    dplyr::filter(!is.na(reviewer_agrees))
  reviewed |>
    dplyr::mutate(dimension_vector = stringr::str_remove(cluster_id, "/c[0-9]+$")) |>
    dplyr::summarise(
      reviewed = dplyr::n(),
      agreement = mean(stringr::str_to_lower(reviewer_agrees) %in% c("yes", "y", "true")),
      .by = dimension_vector
    )
}
