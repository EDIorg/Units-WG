# LLM labeling and audit protocol

LLM labeling is **off by default** (`llm$enabled: false`). The deterministic
pipeline, including clusters and the audit sample, runs without it.

## Why it is opt-in

- **Data sharing:** exemplar definitions, top terms, and units go to a
  third-party API. The text is public EML metadata, but sending it is still
  an external action.
- **Cost:** one request per cluster, up to `llm$max_clusters` (200). The full
  corpus has 263 clusters.
- **Reproducibility:** provider outputs can change even with fixed settings.

Enable it only with working-group agreement, and record who approved it and
when in the [decision log](decision-log.md).

## Configuration

| Setting | Value |
|---|---|
| Provider | `llm$provider` (`openai`); called through `ellmer::chat_<provider>()` |
| Model | `llm$model`, pinned to a dated snapshot (`gpt-4.1-2025-04-14`) |
| Prompt | `llm$prompt` (`prompts/concept-labeling-v1.md`); its SHA-256 is stored with every label and in the run manifest |
| Parameters | `temperature = 0`, `seed = config$seed` |
| Credentials | The provider's environment variable (for example `OPENAI_API_KEY`). Never put keys in `config.yml` or commit them. |

To change the prompt, create a new versioned file (`-v2.md`) instead of
editing v1, and update `llm$prompt`. `targets` then relabels the clusters.

## Inputs and outputs

Each request contains one cluster: its DimensionVector, units with use
counts, top TF-IDF terms, attribute and dataset counts, and up to four
exemplar definitions.

The model returns structured fields:

| Field | Meaning |
|---|---|
| `measured_concept` | Short name of the measured quantity |
| `entity_or_medium` | Organism, material, or medium |
| `process` | Ecological process, or `none` for state variables |
| `denominator_semantics` | What each denominator means |
| `coherent` | Whether the exemplars describe one quantity |
| `ecological_qk_candidate` | Whether a QUDT QK could be justified |
| `aligned_terms` | Known QUDT, ECSO, ENVO, or OBOE terms; must not be invented |
| `confidence` | `high`, `medium`, or `low` |
| `rationale` | One or two sentences citing the exemplars |

Labels are written to `cluster_labels.csv` with `llm_model` and
`prompt_sha256`. A failed request is recorded as `error: <message>` and does
not stop the run. Labels are cached as a target and rerun only when the
clusters, prompt, or LLM settings change.

## Human audit

Labels and seed matches are hypotheses until audited.

1. **Sample.** `audit_sample.csv` draws `analysis$audit_per_cluster` (5)
   attributes per cluster with the pipeline seed, which gives a sample
   stratified by DimensionVector family and cluster. About 1,300 rows are
   available. Reviewers may begin with clusters behind candidate QK pairs.
2. **Review.** For each row, a reviewer reads the attribute name, definition,
   and unit. They fill in `reviewer_measured_concept`, set `reviewer_agrees`
   to `yes` or `no` (whether the LLM label or the cluster's apparent concept
   is right), and add `reviewer_notes`. Reviewers should not look up the
   LLM rationale until they have recorded their own answer.
3. **Record.** Append reviewed rows to `data/reference/audit_reviewed.csv`
   and rerun. `audit_summary.csv` reports agreement by DimensionVector.
4. **Act.**
   - Families with agreement below 0.8 (provisional) are re-scoped, through
     seed edits or smaller clusters, or are left to manual classification.
     Their labels are not used as evidence for QK criteria.
   - Concept pairs with low `vector_share` (below about 0.1) get extra rows,
     because they are most likely to be ancillary columns.
   - Record decisions in the [decision log](decision-log.md).

Two reviewers per family are preferred. Disagreements are resolved in
discussion and noted in `reviewer_notes`.
