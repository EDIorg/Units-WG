# Methods

Each section maps to targets in `_targets.R` and functions in `R/`. All
random steps use `seed` from `config.yml`.

## 1. Corpus and provenance

- **Input:** EML XML files under `EML_CORPUS_ROOT`, read-only. The
  2026-11-12 snapshot has 10,634 files.
- **Batches:** `tar_files_input()` splits files into `corpus$batches`
  batches, which are parsed in parallel by `crew` workers. A positive
  `corpus$sample_n` draws a seeded sample instead (`sample` profile).
- **Manifests:** `corpus_manifest()` records each file's package ID, EML
  version, size, SHA-256, and parse status. `run_manifest()` records the git
  commit and dirty flag, hashes of the configuration, seeds, and prompt, LLM
  settings, QUDT retrieval time, and R, Quarto, and package versions.

## 2. Parsing (`R/parse_eml.R`)

`xml2` parses every file without depending on namespaces, so EML 2.0–2.2
share one code path. Custom-unit definitions use `local-name()` XPath because
`stmml:unitList` uses a namespace prefix. `parse_eml_batch()` returns:

- `datasets`: package ID, scope, EML version, title, keywords and
  thesauri, short abstract and methods snippets, geographic and temporal
  coverage, and whether taxonomic coverage is present.
- `attributes`: one row per attribute, with entity, name, label, definition,
  measurement scale, `standardUnit` or `customUnit`, precision, number type,
  and annotation count.
- `custom_units`: `unitList` definitions (ID, `parentSI`, `multiplierToSI`,
  unit type, description).
- `annotations`: annotations at dataset, entity, and attribute levels, with
  property and value URIs and labels.

A file that fails to parse gets a `datasets` row with `parse_error` set and
empty tables; it does not stop the run.

## 3. Unit normalization (`R/units.R`)

1. **Match key.** `unit_match_key()` groups spelling variants (plurals,
   `squareMeter` and `meterSquared`, `litre` and `liter`) and treats
   denominators as an unordered set. `qname_match_key()` puts QUDT unit
   qnames into the same key space.
2. **QUDT data.** `fetch_qudt_units()` and `fetch_qudt_quantity_kinds()`
   query the live QUDT SPARQL endpoint for units, QKs, DimensionVectors, QK
   relations (`unitForQuantityKind`, `categorizedByQuantityKind`, legacy
   `hasQuantityKind`), and QK parents. The endpoint does not report a
   version, so provenance records the retrieval time.
3. **Bridge.** `build_unit_map()` uses the RCode_JP mapping first, but only
   when its QUDT URI still exists in current QUDT (`rcode_jp_verified`).
   Otherwise, an EML unit maps when its match key equals exactly one QUDT
   unit's key (`match_key`). Everything else is `unmapped`.
4. **Unit group.** `unit_group` is the verified QUDT unit when one exists,
   otherwise the match key. Raw unit names are always kept.

## 4. Descriptive summaries (`R/summaries.R`)

- **Units and families.** Usage by unit group and by DimensionVector family.
  Family labels weight QUDT QKs by corpus use.
- **Concept seeds.** `data/reference/concept_seeds.csv` lists 35 ecological
  concepts. Each has case-insensitive include and exclude regular expressions
  over attribute names, labels, and definitions. Matches are not exclusive.
  Exclude patterns remove known co-located ancillary columns, such as probe
  temperature beside soil moisture or sensor depth beside PAR. Seeds are a
  transparent first pass, not a classification of record.
- **Concept × unit.** `summarise_concept_units()` counts attributes,
  datasets, and scopes per concept × unit group and per concept ×
  DimensionVector.
- **Polysemy.** For units used in at least `analysis$polysemy_min_datasets`
  datasets, the polysemy index is the normalized Shannon entropy of
  seed-concept dataset counts: 0 means one concept, 1 means an even spread.
- **MOB evidence.** `summarise_mob_evidence()` links each candidate in the
  2026-09-07 MOB run to EML units through the RCode_JP pseudo-qname and the
  match key. It then reports usage and example attributes.
- **Annotations.** Coverage by vocabulary, properties, and values. Values of
  "contains measurements of type" annotations are listed with their units as
  a partial gold standard.

## 5. Classification (`R/classify.R`)

1. Distinct definitions (case-folded and whitespace-normalized) are grouped
   by DimensionVector. Families with fewer than
   `analysis$cluster_min_definitions` definitions are skipped. Larger
   families are sampled down to `analysis$cluster_max_definitions` with a
   seed.
2. Definitions become L2-normalized TF-IDF vectors over the family's
   `analysis$cluster_vocabulary` most common terms, excluding stop words and
   numbers.
3. Seeded k-means, with k = min(`cluster_max_k`, max(2, round(√(n / 10)))),
   groups the definitions. Each cluster reports its top centroid terms and
   the exemplars closest to the centroid.
4. Optional LLM labeling and a stratified human audit follow
   ([llm-protocol.md](llm-protocol.md)).

## 6. QK criteria (`R/criteria.R`)

- **Assessment.** `assess_qk_candidates()` evaluates each concept ×
  DimensionVector pair against the automated criteria in the
  [decision log](decision-log.md):
  - recurrence (`qk_criteria$min_datasets`, `qk_criteria$min_scopes`);
  - a search of QUDT QKs with the same vector, using the seed's
    `qudt_qk_hint`;
  - a dimensionless check (QUDT marks dimensionless vectors with `D1`).

  It reports `vector_share` as a review aid and lists the manual checks that
  remain in `open_checks`.
- **Parent QK.** `suggest_parent_qks()` proposes the QUDT QK with the same
  vector that is most often attached to the concept's units, weighted by
  datasets.
- **Attachments.** `propose_unit_attachments()` lists QUDT units used for a
  candidate pair in at least `qk_criteria$attach_min_datasets` datasets. It
  marks links that already exist and otherwise proposes an additive
  `qudt:unitForQuantityKind`.
- **Curated shortlist.** `data/reference/qk_shortlist.csv` consolidates the
  automated candidates into proposals sorted into tiers, applying criterion 8
  and checking parent QKs against QUDT by hand. `summarise_qk_shortlist()`
  validates the concept and QK names and adds corpus evidence. The report's
  Findings page presents the result.

## 7. Report

`tar_quarto()` renders `report/` after the derived tables are written, so
every figure and table comes from the current pipeline state. Pages read
targets from the store through `report/_common.R`.

## Known limitations

- Seed precision has not been audited. Concept counts are upper bounds for
  broad patterns.
- EML unit names are usage, not authority. Some attributes use the wrong
  unit or free text.
- About 8.6% of unit uses are unmapped and drop out of vector-based
  criteria.
- Recurrence thresholds are provisional.
