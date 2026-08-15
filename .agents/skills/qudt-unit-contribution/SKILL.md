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

Read and follow
[`references/research-and-derivation.md`](./references/research-and-derivation.md)
for conversion arithmetic, DimensionVector derivation, QuantityKind semantics,
description synthesis, and evidence quality. If the EDI/LTER `unit-registry` is
available as a workspace folder or through `UNIT_REGISTRY_ROOT`, record its
commit and use it only for Unit-description research within the strict limits
in that reference. It must not inform conversions, QKs, DVs, or other generated
values. Its absence does not block a run.

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
`decisions.csv`.

Treat the submitted factor expression as part of Unit identity. Algebraic or
numeric equivalence to a simpler unit does not make the candidate a duplicate:
QUDT may preserve distinct expressions such as `M-PER-M2` and `PER-M`, or
`J-PER-M2` and `N-PER-M`. If the exact qname is absent and the expression is
valid, continue toward a draft while recording equivalent units as evidence.
Stop as `existing-unit` only when the current QUDT resource represents the same
modeled expression or when the candidate is merely an alternate label or
spelling of that resource.

### 4. Learn current constraints and precedents

Read the current local schema, SHACL shapes, aggregate vocabularies, QUDT build
instructions, and official wiki submission guidelines. Inspect relevant recent
history with `git log` and `git show`, plus at least three closely analogous
current entries. Do not infer current style from old generated files.

Use [`references/modeling-rules.md`](./references/modeling-rules.md) as the
compact checklist, but resolve any discrepancy in favor of current sources
according to source precedence.

### 5. Model composition and semantics

Apply the factor-by-factor derivation and semantic-hypothesis procedures in the
research protocol. Derive the DimensionVector from verified constituent vectors.
Select QuantityKinds from measurement meaning, not dimensionality alone. Verify
every reused QK and DV in local and published QUDT.

- Reuse existing resources whenever their identity and semantics match.
- A new QuantityKind may use an existing DimensionVector.
- A unit may have more than one QuantityKind when current QUDT semantics and
  precedents support all assignments; do not impose an artificial one-QK rule.
- Prefer `qudt:unitForQuantityKind` for a commensurate unit assignment and
  `qudt:categorizedByQuantityKind` for organizational categorization. Use the
  legacy `qudt:hasQuantityKind` only when current target precedent requires it.
- Draft a new QK or DV only when no suitable current resource exists.
- Mark every new QK or DV `draft-review` in `decisions.csv` and `review.md`.

### 6. Verify conversions

Use the research protocol's exact conversion procedure. Trace each multiplier
and offset to an authoritative definition such as BIPM, NIST, ISO, or a
jurisdictional standard. Record the reference unit, equation, constituent
factors, arithmetic, exactness, decimal and scientific values, and sources.

Do not place `TODO_VERIFY`, guessed values, or uncertain conversions in Turtle.
Keep unresolved candidates in the report with status `blocked-conversion`.

### 7. Research descriptions

Use the research protocol to synthesize original, source-backed Unit and
QuantityKind descriptions. Do not infer ecological uses from the unit spelling
or copy historical registry prose. Record description sources, rationale,
semantic hypotheses, and confidence in `decisions.csv`.

If unit composition is established but measurement semantics remain ambiguous,
do not draft a QuantityKind description. Present the researched alternatives
for clarification.

### 8. Generate the output package

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

For every compound or powered unit, emit `qudt:expression` and explicit
`qudt:hasFactorUnit` nodes matching the qname. QUDT can infer factor units during
its build, but review artifacts must remain auditable when that build is not
available.

Candidate statuses are:

- `existing-unit`
- `ready-for-review`
- `draft-review`
- `needs-clarification`
- `blocked-conversion`
- `blocked-source`

### 9. Validate

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
