# Data dictionary

Tables in `data/derived/` are written by the `derived_files` target and are
small enough to commit. Counts are distinct datasets (EML packages),
attributes, or scopes (the package-ID prefix, such as `knb-lter-fce` or
`edi`) unless noted.

Common columns:

| Column | Meaning |
|---|---|
| `unit_group` | Verified QUDT unit qname when mapped; otherwise the spelling-insensitive EML match key. |
| `dimension_vector` | QUDT DimensionVector of the mapped unit (for example `A0E0L-2I0M1H0T-1D0`); `unmapped` or empty when none. Dimensionless is `...D1`. |
| `uses` / `attributes` | Number of attribute rows. |
| `datasets`, `scopes` | Distinct packages and scopes. |
| `top_*` | The most frequent values with counts in parentheses, separated by commas. |

## Corpus and provenance

**`corpus_manifest.csv`**: one row per EML file.
`file`, `package_id`, `eml_version`, `bytes`, `sha256` (file hash),
`parse_status` (`ok` or `error`), `snapshot` (corpus date).

**`run_manifest.json`**: the build record. It includes the git commit and
dirty flag, SHA-256 hashes of `config.yml`, the seeds, and the prompt, the
corpus summary, the QUDT endpoint and retrieval time, LLM settings, and R,
Quarto, and package versions.

## Units

**`unit_summary.csv`**: one row per `unit_group`.
`uses`, `datasets`, `scopes`; `variants` (distinct raw spellings);
`top_names` (raw EML names); `kind` (`standard`, `custom`, or both);
`qudt_unit`; `mapping_source` (`rcode_jp_verified`, `match_key`,
`unmapped`); `dimension_vector`; `distinct_definitions` (distinct
case-folded attribute definitions).

**`family_summary.csv`**: one row per DimensionVector family.
`uses`, `datasets`, `scopes`, `units` (distinct unit groups), `top_units`,
and `top_qudt_qks` (QUDT QKs attached to the family's units, weighted by
corpus uses).

**`polysemy.csv`**: units used in at least `analysis$polysemy_min_datasets`
datasets. `share_matched` is the share of attributes matching any concept
seed. `top_terms` holds informative definition terms. `concepts` is the
number of seed concepts seen in two or more datasets. `polysemy_index` is the
normalized entropy of concept dataset counts (0 means one concept, 1 means an
even spread). `top_concepts` lists concepts with their dataset counts.

## Concepts

**`concept_overview.csv`**: one row per seed concept.
`unit_groups`, `dimension_vectors`, `share_qudt_mapped` (share of matched
attributes with a QUDT-mapped unit), `top_units`.

**`concept_units.csv`**: concept × `unit_group`.
`top_names` (attribute names), `qudt_unit`, `dimension_vector`.

**`concept_dimensions.csv`**: concept × `dimension_vector`, including
`unmapped`. `units` is the number of distinct unit groups, and `top_units`
lists the most used.

## MOB candidate evidence

**`mob_evidence.csv`**: one row per candidate in the 2026-09-07 MOB run.
- `record_id`, `unit_expression`, `status`, and `quantity_kind` come from
  the run's `decisions.csv`.
- `candidate_key` is the match key derived from the candidate.
- `bridge` shows how EML units were linked: `rcode_jp`, `match_key`, both,
  or `none`.
- `eml_unit_names`, `uses`, `datasets`, `scopes`, and `top_terms` describe
  corpus usage.

**`mob_examples.csv`**: up to 5 example attributes per withheld candidate
(`needs-clarification` or `blocked-conversion`).
`candidate`, `status`, `package_id`, `unit`, `attribute`, `definition`.

## Annotations

**`annotation_vocabularies.csv`**: `subject_level` (dataset, entity, or
attribute) × `vocabulary` (QUDT, ECSO, ENVO, PATO, other OBO, OBOE, Darwin Core,
or other),
with annotation and dataset counts.

**`annotation_properties.csv`**: `property`, `property_uri`,
`annotations`, `datasets`.

**`annotation_values.csv`**: the 500 most used values outside QUDT. `vocabulary`,
`value`, `value_uri`, `annotations`, `datasets`.

**`annotation_measured_types.csv`**: values of "contains measurements of
type" annotations, with the `units` used for them.

## Classification and audit

**`clusters.csv`**: one row per TF-IDF/k-means cluster.
- `cluster_id` is `<dimension_vector>/cNN`.
- `definitions` counts distinct definitions in the cluster.
- `top_terms` lists the highest-weighted centroid terms.
- `exemplars` holds up to 4 definitions closest to the centroid, separated by
  ` || `.
- `attributes`, `datasets`, `scopes`, and `units` describe usage.
- `family_definitions` counts distinct definitions in the family.

**`cluster_labels.csv`**: `cluster_id` and `llm_status` (`not-run`,
`labelled`, or `error: ...`). When labeling is enabled, it adds the
structured fields in [llm-protocol.md](llm-protocol.md), `llm_model`, and
`prompt_sha256`.

**`audit_sample.csv`**: a seeded sample of `analysis$audit_per_cluster`
attributes per cluster for human review.
- Context: `attribute_id`, `package_id`, `cluster_id`, `unit`, `unit_group`,
  `attribute_name`, `definition`.
- `llm_measured_concept` is filled when labeling ran.
- Reviewers fill `reviewer_measured_concept`, `reviewer_agrees`
  (`yes`/`no`), and `reviewer_notes`.

**`audit_summary.csv`**: agreement by `dimension_vector`, computed from
`data/reference/audit_reviewed.csv`. `reviewed` counts reviewed rows, and
`agreement` is the share of `yes` answers. The table is empty until audits
are recorded.

## QK criteria

**`qk_candidates.csv`**: concept × mapped `dimension_vector`.
- `label` and `qudt_qk_hint` come from the seeds.
- `qudt_qks_with_vector` counts QUDT QKs sharing the vector.
- `existing_qk_matches` lists QKs with that vector whose label or name
  matches the hint.
- `meets_recurrence` is `datasets >= min_datasets` and
  `scopes >= min_scopes`.
- `dimensionless` is a logical flag.
- `status` is one of:
  - `candidate-new-qk`: recurs, has no QUDT match, and has dimensions;
  - `reuse-or-specialize-existing-qk`: a QUDT QK already matches;
  - `dimensionless-needs-ratio-semantics`;
  - `below-threshold`.
- `open_checks` lists the manual criteria still pending.
- `vectors_meeting_recurrence` counts the concept's vectors that pass
  recurrence.
- `vector_share` is the pair's share of the concept's dataset counts across
  mapped vectors. Low values suggest ancillary columns.
- `suggested_parent_qk` and `other_parent_options` are QUDT QKs with the same
  vector, ranked by how often they are attached to the concept's units.

**`qk_attachments.csv`**: candidate pair × QUDT unit used for it in at least
`attach_min_datasets` datasets.
- `current_qudt_qks` lists the unit's existing QK links.
- `target_qk` is the matched existing QK or, for new candidates,
  `new: <concept>`.
- `already_attached` is a logical flag.
- `proposed_relation` is `unitForQuantityKind` or `none (present)`.

**`qk_shortlist.csv`**: the curated shortlist from
`data/reference/qk_shortlist.csv` (edited by hand), plus evidence columns
computed by the pipeline.
- Curated columns:
  - `proposal_id` and `proposal` (working name);
  - `tier`: `1-reuse`, `2-new-generic`, `3-new-named`, `4-defer`, or
    `5-annotate`;
  - `action`;
  - `qudt_qk`: the existing QK to reuse, or the parent QK;
  - `relation`;
  - `concepts`: seed concepts separated by `;`;
  - `dimension_vector`, `semantics`, and `rationale`.
- Computed columns:
  - `attributes`, `datasets`, and `scopes`: distinct counts across the listed
    concepts within `dimension_vector`;
  - `top_units`: the most-used unit groups, with dataset counts.
- The build stops if a concept is not in the seeds or `qudt_qk` is not a
  current QUDT QK.
