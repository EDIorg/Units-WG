# AI Worklog

This file is a curated, non-exhaustive record of material AI-assisted work in
this project. It connects a request to its outcome, important decisions, and
verification without preserving a full conversation or tool transcript.

## What to record

Add an entry for non-trivial code, configuration, schema, or documentation
changes; architectural or security decisions; consequential external actions;
and failed approaches that would be useful to future contributors.

Do not add entries for routine questions, formatting-only changes, or other
work whose intent and outcome are already obvious from the diff.

## Content and safety

- Summarize the request; do not copy the prompt verbatim.
- Record observable outcomes and verification results, not unverified claims.
- Note material assumptions, trade-offs, limitations, and follow-up work.
- Link to issues, pull requests, or commits instead of duplicating their
  contents.
- Never include credentials, tokens, personal or sensitive data, proprietary
  prompt content, raw tool output, full transcripts, or private reasoning.

Treat this worklog as historical context, not as current system documentation,
a complete audit trail, or a reproducible record of an AI session. Update the
project's authoritative documentation when behavior or operating procedures
change.

Use a root `CHANGELOG.md`, when present, for user-facing release history.

## 2026-08-14 — Cross-platform QUDT contribution workflow

- **Request summary:** Replace earlier QUDT AI experiments with a reusable
  Copilot and Codex workflow that processes candidate-unit files into reviewable
  Turtle and decision artifacts.
- **Outcome:** Added one authoritative skill under `.agents/skills/`, thin
  Copilot agent and prompt adapters, structured and legacy input normalization,
  RDF-aware preflight validation, run-specific output guidance, source
  precedence, an auditable research-and-derivation protocol, modeling
  references, a governed recommended-source catalog, a controlled continuous
  improvement protocol, templates, tests, and contributor documentation.
  Archived entrypoints were renamed to prevent discovery.
- **Decisions:** Current local and published QUDT plus official guidance outrank
  historical project artifacts. `unit_expression` is the only required user
  field; the agent researches all other values and asks only about material
  ambiguities that remain. Language-tag convention differences are review
  warnings rather than hard failures. Unit identity preserves valid factor
  expressions instead of collapsing algebraically equal qnames; compound drafts
  include explicit expressions and factor units. New QuantityKinds and
  DimensionVectors are drafted and flagged for review. The EDI/LTER
  `unit-registry` is restricted to Unit-description research leads; it cannot
  inform conversions, QKs, DVs, context, or other generated values. Optimized
  for reliability and operational excellence at the cost of additional
  research and validation work.
- **Verification:** Parsed all customization frontmatter; validated local links;
  ran Ruff, Black, and four intake tests; normalized all 98 legacy expressions
  as research-ready; verified the live SPARQL endpoint; and confirmed RDF
  preflight accepts complete drafts and intentional language-tag variants while
  rejecting missing or inconsistent conversions and conflicting
  unit/QuantityKind vectors. Current precise QUDT QuantityKind relations and
  source-guideline requirements were checked against the local QUDT source and
  official wiki.
- **Limitations/follow-up:** Generated content still requires authoritative
  conversion research, current QUDT build validation, and domain-expert review
  during each run.
- **References:** `AI_create_ttl/QUDT_AI_WORKFLOW.md`

## 2026-08-13 — Cross-platform instruction cleanup

- **Request summary:** Review the AI instruction template for copied-project
  use, cross-platform Copilot behavior, and the repository-specific changelog.
- **Outcome:** Removed references that tell copied projects to maintain
  `.github/CHANGELOG.md`; made analysis and Well-Architected instruction files
  explicit with `applyTo` frontmatter for more consistent automatic matching.
- **Decisions:** Kept `AGENTS.md` as the canonical operating model and
  platform-specific files as adapters. The template repository's customization
  changelog belongs on the documentation branch, not in copied project files.
- **Verification:** Inspected the instruction files and compared the behavior
  against official VS Code and GitHub Copilot documentation.
- **Limitations/follow-up:** Confirm whether any copy scripts or template
  generation workflows need to exclude branch-only documentation files.
- **References:** None.

## Entry template

Keep each entry concise (normally no more than 100-200 words) and place the
newest entry first.

```markdown
## YYYY-MM-DD — Short description

- **Request summary:** A brief, sanitized statement of the goal.
- **Outcome:** What changed or was decided.
- **Decisions:** Important rationale and WAF trade-offs, if applicable.
- **Verification:** Commands or checks run and their results.
- **Limitations/follow-up:** Known gaps or next steps, or `None`.
- **References:** Related issue, pull request, or commit, or `None`.
```
