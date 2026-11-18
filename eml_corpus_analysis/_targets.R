# Pipeline for the EML corpus units and ecological-context analysis.
# Run from eml_corpus_analysis/:
#   EML_CORPUS_ROOT=~/Desktop/EMLCorpus20261112 Rscript -e 'targets::tar_make()'
# Use R_CONFIG_ACTIVE=sample for a quick seeded subset. See README.md.

library(targets)
library(tarchetypes)

targets::tar_source("R")

cfg <- config::get(file = "config.yml")
corpus_root <- path.expand(Sys.getenv("EML_CORPUS_ROOT"))

targets::tar_option_set(
  packages = character(),
  seed = cfg$seed,
  controller = crew::crew_controller_local(workers = cfg$workers, seconds_idle = 30),
  error = "stop"
)

list(
  # ---- Phase 1: deterministic extraction -----------------------------------
  tarchetypes::tar_files_input(
    corpus_xml,
    list_corpus_files(corpus_root, cfg$corpus$sample_n, cfg$seed),
    batches = cfg$corpus$batches
  ),
  tar_target(parsed, parse_eml_batch(corpus_xml), pattern = map(corpus_xml), iteration = "list"),
  tar_target(datasets, combine_parsed(parsed, "datasets"), deployment = "main"),
  tar_target(attributes, combine_parsed(parsed, "attributes"), deployment = "main"),
  tar_target(custom_units, combine_parsed(parsed, "custom_units"), deployment = "main"),
  tar_target(annotations, combine_parsed(parsed, "annotations"), deployment = "main"),

  # QUDT is fetched once and frozen; refresh with tar_invalidate(starts_with("qudt_")).
  tar_target(qudt_units, fetch_qudt_units(cfg$qudt$sparql_endpoint),
    cue = tar_cue(mode = "never"), deployment = "main"
  ),
  tar_target(qudt_qks, fetch_qudt_quantity_kinds(cfg$qudt$sparql_endpoint),
    cue = tar_cue(mode = "never"), deployment = "main"
  ),
  tar_target(jp_mapping_file, cfg$reference$jp_mapping, format = "file"),
  tar_target(jp_mapping, read_jp_mapping(jp_mapping_file)),
  tar_target(
    unit_map,
    build_unit_map(
      c(attributes$standard_unit, attributes$custom_unit), jp_mapping, qudt_units
    )
  ),
  tar_target(units_tbl, annotate_units(attributes, unit_map), deployment = "main"),
  tar_target(attributes_parquet,
    write_parquet_duckdb(units_tbl, "data/work/attributes.parquet"),
    format = "file", deployment = "main"
  ),

  # ---- Phase 2: human-friendly summaries -----------------------------------
  tar_target(seeds_file, cfg$reference$concept_seeds, format = "file"),
  tar_target(seeds, read_concept_seeds(seeds_file)),
  tar_target(concept_hits, match_concepts(units_tbl, seeds)),
  tar_target(corpus_overview, summarise_corpus(datasets, units_tbl, annotations)),
  tar_target(unit_summary, summarise_units(units_tbl)),
  tar_target(family_summary, summarise_families(units_tbl, qudt_units)),
  tar_target(
    polysemy,
    summarise_polysemy(units_tbl, concept_hits, cfg$analysis$polysemy_min_datasets)
  ),
  tar_target(concept_units, summarise_concept_units(units_tbl, concept_hits)),
  tar_target(mob_decisions_file, cfg$reference$mob_decisions, format = "file"),
  tar_target(mob_evidence, summarise_mob_evidence(units_tbl, mob_decisions_file)),
  tar_target(annotation_summary, summarise_annotations(annotations, units_tbl)),

  # ---- Phase 3: concept classification ------------------------------------
  tar_target(
    clusters,
    cluster_definitions(
      units_tbl,
      min_definitions = cfg$analysis$cluster_min_definitions,
      max_definitions = cfg$analysis$cluster_max_definitions,
      vocabulary = cfg$analysis$cluster_vocabulary,
      max_k = cfg$analysis$cluster_max_k,
      seed = cfg$seed
    )
  ),
  tar_target(prompt_file, cfg$llm$prompt, format = "file"),
  tar_target(llm_labels,
    label_clusters_llm(clusters$clusters, cfg$llm, prompt_file, cfg$seed),
    deployment = "main"
  ),
  tar_target(
    audit_sample,
    draw_audit_sample(
      units_tbl, clusters$assignments, llm_labels, cfg$analysis$audit_per_cluster, cfg$seed
    )
  ),
  tar_target(audit_reviewed_file, "data/reference/audit_reviewed.csv", format = "file"),
  tar_target(audit_summary, summarise_audit(audit_reviewed_file)),

  # ---- Phase 4: QK admission criteria and candidates ----------------------
  tar_target(qk_parents, suggest_parent_qks(concept_units$by_unit, qudt_units, qudt_qks)),
  tar_target(
    qk_candidates,
    assess_qk_candidates(
      concept_units$by_dimension, seeds, qudt_qks, qk_parents,
      min_datasets = cfg$qk_criteria$min_datasets,
      min_scopes = cfg$qk_criteria$min_scopes
    )
  ),
  tar_target(qk_shortlist_file, "data/reference/qk_shortlist.csv", format = "file"),
  tar_target(
    qk_shortlist,
    summarise_qk_shortlist(
      read_qk_shortlist(qk_shortlist_file), units_tbl, concept_hits, seeds, qudt_qks
    )
  ),
  tar_target(
    qk_attachments,
    propose_unit_attachments(
      qk_candidates, concept_units$by_unit, qudt_units, cfg$qk_criteria$attach_min_datasets
    )
  ),

  # ---- Outputs and provenance ----------------------------------------------
  tar_target(corpus_manifest_tbl, corpus_manifest(datasets, cfg$corpus$snapshot)),
  tar_target(
    derived_files,
    write_derived_tables(
      list(
        unit_summary = unit_summary,
        family_summary = family_summary,
        polysemy = polysemy,
        concept_overview = concept_units$overview,
        concept_units = concept_units$by_unit,
        concept_dimensions = concept_units$by_dimension,
        mob_evidence = mob_evidence$summary,
        mob_examples = mob_evidence$examples,
        annotation_vocabularies = annotation_summary$vocabularies,
        annotation_properties = annotation_summary$properties,
        annotation_values = annotation_summary$values,
        annotation_measured_types = annotation_summary$measured_types,
        clusters = clusters$clusters,
        cluster_labels = llm_labels,
        audit_sample = audit_sample,
        audit_summary = audit_summary,
        qk_candidates = qk_candidates,
        qk_attachments = qk_attachments,
        qk_shortlist = qk_shortlist,
        corpus_manifest = corpus_manifest_tbl
      ),
      "data/derived"
    ),
    format = "file"
  ),
  tar_target(config_file, "config.yml", format = "file"),
  tar_target(
    run_manifest_file,
    write_derived_json(
      run_manifest(config_file, seeds_file, prompt_file, qudt_units, corpus_manifest_tbl, cfg),
      "data/derived/run_manifest.json"
    ),
    format = "file", deployment = "main"
  ),
  tarchetypes::tar_quarto(
    report,
    path = "report",
    extra_files = c(
      "report/_common.R", "report/_quarto.yml", "report/styles.css",
      "config.yml"
    ),
    working_directory = "report",
    quiet = TRUE,
    deployment = "main"
  )
)
