# QUDT Unit Contribution Workflow

This workflow processes a file of ecology unit candidates, checks current QUDT
sources, and creates reviewable Turtle plus notes, concerns, recommendations,
and an evidence ledger. It is available in VS Code through GitHub Copilot and
the Codex extension.

## Before You Start

Open Units-WG in VS Code. Make a current QUDT source checkout available by
setting:

```bash
export QUDT_SOURCE_ROOT=/path/to/qudt-source
```

Alternatively, open the checkout as another VS Code workspace folder or supply
its path when the workflow asks. The checkout may be the official repository or
a project fork; the workflow will not choose one silently.

Prepare a CSV, TSV, or legacy text file containing the unit candidates. CSV and
TSV provide the most reliable semantic context. Legacy files may contain one
plain or comma-terminated unit expression per line.

## Input Columns

Column names use `snake_case`. The user normally supplies only the unit
expression. The agent researches and proposes context, conversions, labels,
QuantityKinds, DimensionVectors, and other modeled values.

### Required

| Column | Definition |
| --- | --- |
| `unit_expression` | Verbatim submitted unit expression, such as `GM-PER-HA-DAY`. The workflow cannot analyze a candidate without it. |

### Optional Evidence and Preferences

| Column | Definition |
| --- | --- |
| `measurement_context` | Known statement of what the value measures. The agent researches and proposes it when absent. |
| `qualifier_definition` | Known meaning of ambiguous or contextual qualifiers. The agent investigates it when absent. |
| `conversion_definition` | Known conversion definition or derivation. The agent researches it when absent. |
| `conversion_source` | Citation or URL supporting the conversion. The agent verifies supplied evidence or locates an authoritative source. |
| `source_variable` | Original variable or attribute name, when available. |
| `dataset_id` | Dataset, package, or study identifier that supplies provenance and may clarify domain usage. |
| `variable_name` | Human-readable variable name associated with the measurement. |
| `variable_definition` | Full source definition of the measured variable. |
| `proposed_qname` | Suggested QUDT local name. It is a proposal and will be checked against current naming rules and duplicates. |
| `proposed_quantity_kind` | Suggested QuantityKind URI or local name. It is a hypothesis requiring semantic verification. |
| `preferred_label` | Preferred human-readable unit label. |
| `preferred_symbol` | Preferred Unicode unit symbol. |
| `known_conversion` | Contributor-supplied multiplier, offset, or derivation. It accelerates research but still requires verification. |
| `reference_url` | Link to a dataset, protocol, standard, method, or other contextual source. |
| `notes` | Ambiguities, concerns, usage details, or review instructions not represented elsewhere. |

Start from
[the candidate template](../.agents/skills/qudt-unit-contribution/assets/candidate-units-template.csv)
when these fields are available. A minimal CSV is:

```csv
unit_expression
GM-PER-HA-DAY
NUM-PER-M2
```

Expression-only CSV and legacy lists are normal inputs. Every expression enters
research. The agent asks consolidated clarification questions only when a
material ambiguity remains after research and would change the RDF.

## Submit a File

The file can remain anywhere inside the open VS Code workspace. You do not need
to move it into a special input directory; the workflow copies it into the
run-specific `input/` folder.

1. Open a new Copilot or Codex chat in the Units-WG workspace.
2. Attach the file using the chat composer’s file/context control, drag it into
  the chat when supported, or mention its workspace-relative path in the
  request.
3. Invoke the QUDT skill or Copilot agent as described below.
4. Send a request identifying the attached file. A run name and QUDT source
  path are optional when they can be derived or discovered.
5. Answer any consolidated clarification questions. Candidates that do not
  require clarification continue through the workflow.

Do not paste a large candidate file into the message when it can be attached or
referenced by path. Keep the source file unchanged so its hash and provenance
can be recorded.

### Generic Request

Use this request with either Copilot or Codex after attaching the file:

```text
Use the qudt-unit-contribution skill to process the attached file.

Run name: [optional run name]
QUDT source root: [optional path, or resolve it from the workspace/environment]

Apply the documented source precedence and input contract. Research and propose
all values not supplied by the user, including measurement context, qualifier
meaning, conversions, labels, QuantityKinds, and DimensionVectors. Ask one
consolidated set of clarification questions only for material ambiguities that
remain after research, and continue processing unblocked candidates.

Create a new run-specific output directory containing units.ttl,
quantitykinds.ttl, dimensionvectors.ttl, decisions.csv, and review.md. Reuse
existing QUDT resources, flag proposed QuantityKinds and DimensionVectors for
human review, run all available validation, and summarize output paths,
candidate statuses, concerns, recommendations, and limitations.
```

A standalone copy is available as
[generic-request.md](../.agents/skills/qudt-unit-contribution/assets/generic-request.md).

## GitHub Copilot

Use any of these entry points:

1. Select **QUDT Ontology Engineer** from the agent picker and attach or name
  the candidate file, then send the generic request above.
2. Type `/process-qudt-unit-list`, attach or name the candidate file, and send.
3. Type `/qudt-unit-contribution` to invoke the shared skill directly.

The prompt and agent both delegate to the same skill.

## Codex

Open the Codex sidebar, run `/skills`, and select
`qudt-unit-contribution`, attach or name the candidate file, and send the generic
request. You can also mention the skill and path directly:

```text
$qudt-unit-contribution process AI_create_ttl/inputs/candidates.csv
```

If a newly added skill does not appear, restart Codex in the Units-WG project.

## Output Package

Each run creates a new directory:

```text
AI_create_ttl/runs/YYYY-MM-DD-run-name/
├── input/
├── intake.csv
├── units.ttl
├── quantitykinds.ttl
├── dimensionvectors.ttl
├── decisions.csv
└── review.md
```

The Turtle files contain only new draft resources. Existing QUDT resources are
referenced but not redefined. New QuantityKinds and DimensionVectors are marked
for human review.

Unit duplicate checks preserve the submitted factor expression. An
algebraically equal simpler unit does not automatically replace a candidate:
QUDT may retain both expressions when their factorization, symbol, UCUM code, or
semantic family differs. Only an exact modeled-expression match or a true
alternate label/spelling is treated as `existing-unit`.

Compound and powered Unit drafts include `qudt:expression` and explicit
`qudt:hasFactorUnit` nodes. These remain reviewable even when the QUDT build that
normally infers and validates factors is unavailable.

`decisions.csv` records the disposition and evidence for every candidate using
the stable
[decisions ledger contract](../.agents/skills/qudt-unit-contribution/references/decisions-contract.md).
`review.md` records provenance, concerns, recommendations, validation results,
limitations, and unresolved questions.

## Research Sources

The workflow follows the documented
[research and derivation protocol](../.agents/skills/qudt-unit-contribution/references/research-and-derivation.md).
Current QUDT sources determine ontology structure. BIPM, NIST, standards bodies,
jurisdictional definitions, and authoritative domain organizations support
conversions and definitions. Dataset metadata and domain references support
measurement meaning and descriptions.

The EDI/LTER `unit-registry` may be used only as a secondary lead for Unit
description research. Every retained description claim requires independent
confirmation, and registry text must not be copied verbatim. It must not inform
conversions, labels, aliases, deprecations, QuantityKinds, DimensionVectors,
measurement context, or any other generated value.

## Candidate Statuses

- `existing-unit`: an equivalent current QUDT unit was found and reused
- `ready-for-review`: a new unit draft has complete evidence
- `draft-review`: a new QuantityKind or DimensionVector needs domain review
- `needs-clarification`: source-specific semantics are unresolved
- `blocked-conversion`: an authoritative conversion is unresolved
- `blocked-source`: a mandatory QUDT or authoritative source was unavailable

Candidates with blocked or clarification statuses remain in the reports but do
not appear as RDF resources.

## Validation

The workflow normalizes input with:

```bash
uv run --script .agents/skills/qudt-unit-contribution/scripts/normalize_intake.py \
  INPUT_FILE --output RUN_DIR/intake.csv
```

It parse-checks and preflights generated Turtle with:

```bash
.agents/skills/qudt-unit-contribution/scripts/ttl_preflight_check.sh \
  RUN_DIR/units.ttl RUN_DIR/quantitykinds.ttl RUN_DIR/dimensionvectors.ttl
```

It also runs the current documented validation process in the configured QUDT
checkout. If that build cannot run, the limitation is reported and the output
must not be described as submission-ready.

The preflight script checks RDF syntax, placeholders, core properties, numeric
datatypes, and expected graph URIs. It does not establish semantic correctness,
verify reused URIs, or replace the QUDT repository build.

Templates use current common QUDT language-tag conventions. Language tags may
be added or omitted when appropriate for the literal and supported by current
target-vocabulary precedent. Preflight reports convention differences as
warnings for review rather than rejecting otherwise valid RDF.

All generated ontology content requires domain-expert review before submission.