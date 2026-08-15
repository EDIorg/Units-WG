---
name: qudt-unit-contribution
description: 'Process files of ecology unit candidates into evidence-backed QUDT Turtle and review artifacts. Use when checking QUDT units, selecting or proposing QuantityKinds and DimensionVectors, verifying conversions, or preparing batch unit contributions. Do not use for unrelated RDF work.'
argument-hint: 'Provide a CSV, TSV, or legacy text unit list; optionally provide a run name and QUDT source path.'
---

# QUDT Unit Contribution

Prepare reviewable QUDT unit contributions from a candidate file. Follow the
repository-wide `AGENTS.md` operating model. This skill is the sole active
authority for the QUDT batch workflow; Copilot agents and prompts are invocation
adapters only.

Never consult `archive/` unless the user explicitly requests a historical
comparison. Treat prior outputs and
`AI_create_ttl/claude_AugSep_mob/inputs/QUDT_Modeling_Rules.md` as historical
evidence, not current ontology rules.

## Required Inputs

Obtain:

1. A CSV, TSV, or legacy text file containing candidate units.
2. A short run name. Derive one from the input filename if none is supplied.
3. A QUDT source root, resolved in this order:
   - `QUDT_SOURCE_ROOT`
   - a QUDT checkout open as a workspace folder
   - a path supplied by the user

If no checkout is available, ask for its path or offer to clone the official
repository or the user's preferred fork into a user-approved location. Do not
silently choose a fork.

Read [`references/input-contract.md`](./references/input-contract.md) before
processing input. `unit_expression` is the only required user-supplied field.
Treat all other supplied fields as optional evidence. Research missing context,
qualifiers, conversions, labels, QuantityKinds, and DimensionVectors rather
than asking the user to model them.

## Source Precedence

Read [`references/source-precedence.md`](./references/source-precedence.md).
Before generating RDF, confirm that the local QUDT source, current wiki, and
live SPARQL endpoint are reachable. Stop and report the unavailable source if
any mandatory source cannot be accessed.

## Workflow

### 1. Create an isolated run

Create `AI_create_ttl/runs/YYYY-MM-DD-<run-name>/`. Never overwrite an existing
run. Copy the submitted input into `input/` and record its original path in
`review.md`.

Record:

- Input filename and SHA-256 hash
- Run timestamp
- QUDT source path, remote, branch, and commit
- Live endpoint and wiki access timestamp

### 2. Normalize and triage input

Run:

```bash
uv run --script .agents/skills/qudt-unit-contribution/scripts/normalize_intake.py \
  INPUT_FILE --output RUN_DIR/intake.csv
```

Every row with a `unit_expression` starts as `ready-for-research`. Research the
likely physical meaning, context, conversion, QuantityKind, and DimensionVector
before requesting user input. Ask one consolidated set of questions only for
material ambiguities that remain after research. Continue processing unblocked
rows while awaiting answers. Do not emit RDF for unresolved rows.

### 3. Check for existing resources

For every candidate, search by proposed qname, expression, label, symbol,
aliases, and spelling variants in both:

- The local aggregate unit vocabulary under the resolved QUDT source root
- `https://www.qudt.org/fuseki/qudt/sparql`

Also check whether a matching resource exists on the local branch but not in
the published endpoint, or vice versa. Record the result and evidence in
`decisions.csv`. If the same unit exists, reuse its URI and do not emit a new
unit definition.

### 4. Learn current constraints and precedents

Read the current local schema, SHACL shapes, aggregate vocabularies, QUDT build
instructions, and official wiki submission guidelines. Inspect relevant recent
history with `git log` and `git show`, plus at least three closely analogous
current entries. Do not infer current style from old generated files.

Use [`references/modeling-rules.md`](./references/modeling-rules.md) as the
compact checklist, but resolve any discrepancy in favor of current sources
according to source precedence.

### 5. Model composition and semantics

Derive the dimension vector from the physical expression. Select QuantityKinds
from measurement meaning, not dimensionality alone. Verify every reused QK and
DV in local and published QUDT.

- Reuse existing resources whenever their identity and semantics match.
- A new QuantityKind may use an existing DimensionVector.
- A unit may have more than one QuantityKind when current QUDT semantics and
  precedents support all assignments; do not impose an artificial one-QK rule.
- Draft a new QK or DV only when no suitable current resource exists.
- Mark every new QK or DV `draft-review` in `decisions.csv` and `review.md`.

### 6. Verify conversions

Trace each multiplier and offset to an authoritative definition such as BIPM,
NIST, ISO, or a jurisdictional standard. Transparent arithmetic from exact
definitions is allowed when the derivation and source are recorded.

Do not place `TODO_VERIFY`, guessed values, or uncertain conversions in Turtle.
Keep unresolved candidates in the report with status `blocked-conversion`.

### 7. Generate the output package

Create these files in the run directory:

- `units.ttl`: new unit definitions only
- `quantitykinds.ttl`: new QuantityKind drafts only
- `dimensionvectors.ttl`: new DimensionVector drafts only
- `decisions.csv`: one row per candidate following
  [`references/decisions-contract.md`](./references/decisions-contract.md)
- `review.md`: notes, concerns, recommendations, provenance, validation, and
  questions requiring human review

Use the templates under `assets/`, adapting them to current nearby QUDT style.
Reference existing QKs and DVs from `units.ttl`; do not duplicate their
definitions. Files with no new resources may contain prefixes and an explanatory
comment but no placeholder resources.

Candidate statuses are:

- `existing-unit`
- `ready-for-review`
- `draft-review`
- `needs-clarification`
- `blocked-conversion`
- `blocked-source`

### 8. Validate

Run:

```bash
.agents/skills/qudt-unit-contribution/scripts/ttl_preflight_check.sh \
  RUN_DIR/units.ttl RUN_DIR/quantitykinds.ttl RUN_DIR/dimensionvectors.ttl
```

Then validate the proposed resources with the resolved QUDT checkout's current
documented build process, using a reversible staging approach that does not
modify unrelated source. If the build tooling is unavailable, report that as a
validation limitation; do not claim submission readiness.

Recheck that all reused URIs exist and that QK and unit vectors agree. Resolve
all errors before finishing. Warnings and semantic uncertainties must appear in
`review.md`.

The preflight script checks RDF syntax, placeholders, core properties, numeric
datatypes, and graph URIs. It does not replace semantic review, URI verification,
or the QUDT repository build.

## Completion Report

Report:

- Run directory
- Candidate counts by status
- Existing resources reused
- New units, QKs, and DVs drafted
- Validation commands and results
- Remaining human decisions
- Limitations

Human review by a domain expert is required before submission.
