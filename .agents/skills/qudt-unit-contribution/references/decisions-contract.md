# Decisions Ledger Contract

`decisions.csv` contains one row for every normalized candidate, including
existing, blocked, and clarification cases. Use the stable columns in
[`../assets/decisions-template.csv`](../assets/decisions-template.csv).

| Column | Definition |
| --- | --- |
| `record_id` | Internal sequential identifier generated during normalization and copied from `intake.csv`; users do not provide it. |
| `unit_expression` | Submitted expression copied from `intake.csv`. |
| `status` | One of the statuses defined by the skill. |
| `proposed_unit_uri` | Existing or proposed QUDT Unit URI, blank when unresolved. |
| `quantity_kind_uris` | Verified or proposed QK URIs separated by `|`. |
| `dimension_vector_uri` | Verified or proposed DV URI. |
| `local_evidence` | Local source file and line, query result, or explicit no-match statement. |
| `published_evidence` | Live SPARQL result or explicit no-match statement with query timestamp. |
| `reference_unit_uri` | QUDT reference unit to which the conversion applies. |
| `conversion_value` | Verified multiplier and offset, when applicable. |
| `conversion_equation` | Reproducible equation and constituent factors used to derive the conversion. |
| `conversion_exact` | Whether the authoritative definition makes the conversion exact. |
| `conversion_source` | Authoritative citation or URL supporting the conversion. |
| `qk_rationale` | Why the QuantityKind assignment matches measurement semantics. |
| `qk_relation` | `unitForQuantityKind`, `categorizedByQuantityKind`, or documented legacy relation. |
| `dv_rationale` | Dimensional derivation and component exponents. |
| `description_sources` | Citations supporting Unit and QuantityKind description claims, separated by `|`. |
| `description_rationale` | What each description asserts, omits, and distinguishes. |
| `semantic_hypotheses` | Material interpretations considered and why they were accepted or rejected. |
| `confidence` | `high`, `medium`, or `low`, with uncertainty explained in `issues`. |
| `issues` | Missing information, conflicting evidence, concerns, or required human decisions. |
| `recommendation` | Recommended disposition or next action. |

Do not use an empty evidence field to mean “not found.” Record an explicit
no-match statement and the source checked. Proposed resources must be labeled
as proposals in the evidence and rationale fields.
