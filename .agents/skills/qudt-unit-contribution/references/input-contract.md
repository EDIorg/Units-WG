# Candidate Input Contract

CSV and TSV are preferred. Column names are case-sensitive and use
`snake_case`. Legacy text files containing one comma-terminated or plain unit
expression per line are fully supported.

The contributor normally supplies only `unit_expression`. The agent is
responsible for researching and proposing all other values. Optional columns
provide evidence or preferences when the contributor already has them; blank
optional fields do not make a row incomplete.

## Required User Column

| Column | Definition |
| --- | --- |
| `unit_expression` | Verbatim submitted unit expression, such as `GM-PER-HA-DAY`. This is required to identify and analyze the candidate. |

## Optional User Evidence

| Column | Definition |
| --- | --- |
| `measurement_context` | Contributor-supplied statement of what the value measures, such as “daily dry biomass production per hectare.” The agent researches and proposes this when absent. |
| `qualifier_definition` | Known meaning of a qualifier such as `NUM`, `COUNT`, dry mass, carbon, or a jurisdictional convention. The agent investigates it when absent. |
| `conversion_definition` | Known definition or derivation relating the candidate to its reference unit. The agent researches this when absent. |
| `conversion_source` | Citation or URL supporting a conversion. Contributor evidence is useful, but the agent must independently verify it and locate a source when absent. |
| `source_variable` | Original variable or attribute name, when available. It can help distinguish meanings that share the same unit expression. |
| `dataset_id` | Dataset, package, or study identifier that establishes provenance and may clarify domain usage. |
| `variable_name` | Human-readable variable name associated with the measurement. |
| `variable_definition` | Full source definition of the measured variable. |
| `proposed_qname` | Contributor's suggested QUDT local name. It is treated as a proposal and validated against current naming rules and duplicates. |
| `proposed_quantity_kind` | Contributor's suggested QuantityKind URI or local name. It is treated as a hypothesis requiring semantic verification. |
| `preferred_label` | Contributor's preferred human-readable unit label. |
| `preferred_symbol` | Contributor's preferred Unicode symbol. |
| `known_conversion` | Contributor-supplied multiplier, offset, or derivation. It accelerates research but must still be verified. |
| `reference_url` | Link to a dataset, protocol, standard, method, or other contextual source. |
| `notes` | Ambiguities, contributor concerns, usage details, review instructions, and candidate-specific citations or source URLs not represented elsewhere. Include a concise statement of what each source may support. |

## Minimal CSV

```csv
unit_expression
GM-PER-HA-DAY
NUM-PER-M2
```

Use [`../assets/candidate-units-template.csv`](../assets/candidate-units-template.csv)
when the additional fields are available.

Use `reference_url` for one simple reference. Put additional candidate-specific
citations, paths, URLs, locators, and short claim summaries in `notes`, separated
clearly with semicolons or line breaks inside the CSV cell. Reusable sources that
should help future batches belong in
[`recommended-sources.md`](./recommended-sources.md), not candidate input.

## Missing Information

The normalizer marks every nonempty `unit_expression` as `ready-for-research`.
It generates an internal sequential `record_id` in `intake.csv` so later
questions and decisions can refer to a stable row. Users do not provide this
identifier. The normalizer does not require context, qualifier, conversion, or
ontology fields at intake.

The agent must research those values and record evidence, alternatives, and
confidence. It asks the user only when a material ambiguity remains after
research and would change the resulting RDF. Missing contributor input alone is
not a reason to stop processing.

Rows lacking sufficient semantics remain in `decisions.csv` and `review.md` but
must not appear as RDF resources.
