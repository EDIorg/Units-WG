# Continuous Improvement Protocol

Improve the workflow from reviewed evidence, not by treating generated output
as truth. Every change to an agent, skill, template, or validator must remain
subordinate to current authoritative sources.

## Capture Feedback During Every Run

Add a **Workflow Feedback** section to each run's `review.md`, even when it says
`None`. Record observations in these categories:

- `input`: unsupported formats, ambiguous tokens, or missing contributor fields
- `source`: inaccessible, stale, conflicting, or newly useful sources
- `identity`: duplicate detection or expression-identity mistakes
- `conversion`: derivation gaps, affine cases, precision, or exactness issues
- `dimension`: parsing or exponent-composition failures
- `semantics`: QK selection, commensurability, or context ambiguities
- `description`: unsupported claims, weak wording, or missing domain evidence
- `validation`: false positives, false negatives, build failures, or missing checks
- `usability`: invocation, reports, questions, or output-layout friction
- `upstream`: QUDT reviewer acceptance, rejection, requested changes, or merged patterns

For each observation, include:

- Candidate and run directory
- Observable problem or successful pattern
- Evidence and authoritative source
- Impact and frequency
- Proposed change
- Human disposition: `accepted`, `rejected`, `deferred`, or `needs-evidence`

Use
[`../assets/workflow-feedback-template.md`](../assets/workflow-feedback-template.md)
as a compact format.

## Promotion Criteria

Promote a lesson into the active workflow only when:

1. It is confirmed by current QUDT, an authoritative external source, an
   executable failure, or explicit upstream QUDT review.
2. It is generalizable beyond one candidate, or it prevents a high-impact error.
3. It does not conflict with the repository's `AGENTS.md` or source precedence.
4. The change identifies whether it belongs in the skill, a reference, a
   template, a script, the Copilot adapter, or user documentation.
5. A focused validation or regression example can demonstrate the improvement.

Repeated occurrence strengthens priority but is not a substitute for evidence.
A single severe confirmed defect, such as collapsing expression-specific Units,
may justify immediate promotion.

## Change Procedure

1. Reproduce the issue using a minimal candidate or fixture.
2. Verify expected behavior from current authoritative sources.
3. Update the sole owning layer:
   - `SKILL.md` for workflow and gates
   - `references/` for domain rules and evidence protocols
   - `assets/` for user or RDF templates
   - `scripts/` for deterministic checks
   - `.github/agents/` and `.github/prompts/` only for invocation behavior
   - `recommended-sources.md` for a vetted source useful across future batches
4. Add or update a positive and negative regression fixture when executable
   behavior changes.
5. Rerun focused tests, RDF preflight, link checks, diagnostics, and whitespace
   checks.
6. Rerun the affected candidate batch into a new immutable run directory; do
   not overwrite the original run.
7. Record the material change and limitations in `AI_WORKLOG.md`.

## Periodic Review

After each substantial batch or before a release:

1. Compare candidate statuses and recurring `issues` across recent runs.
2. Review all accepted and deferred Workflow Feedback items.
3. Compare generated drafts with upstream QUDT review outcomes.
4. Retire stale rules and references when current QUDT supersedes them.
5. Review the recommended-source catalog for stale versions, changed authority,
   access failures, and newly vetted sources.
6. Check discovery: exactly one active skill owns the workflow; Copilot agent
   and prompt remain thin adapters; archive entrypoints remain inactive.
7. Check portability in both Copilot and Codex.
8. Prioritize automation where repeated manual review has a deterministic rule.

## Useful Improvement Metrics

Track trends rather than optimizing a single run:

- Fraction of candidates resolved without clarification
- Existing versus new Unit/QK/DV decisions later confirmed by reviewers
- Conversion and DimensionVector corrections after first draft
- Preflight failures by category
- Number of unsupported description claims removed during review
- QUDT reviewer changes requested and acceptance rate
- Runtime and external-query volume for larger batches

Metrics diagnose the workflow; they do not determine ontology truth.

## Guardrails

- Never promote historical AI output directly into active rules.
- Never infer authority from frequency, confidence scores, or model agreement.
- Never weaken a validation gate solely to make a failing draft pass.
- Keep candidate-specific decisions in run artifacts, not global instructions.
- Keep `unit-registry` restricted to Unit-description research leads.
- Preserve rejected and superseded runs for comparison, clearly marked as such.
