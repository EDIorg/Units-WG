# Engineering Operating Model — Intentional Coding

These instructions apply to all work in this repository. They establish an
**intentional, well-architected** workflow: think briefly before coding,
consult authoritative sources, and stay aware of design trade-offs — without
the overhead of formal spec files or gated planning phases.

## Instruction discovery

`AGENTS.md` is the canonical, cross-platform operating model for AI assistants
working in this repository. Platform-specific files should point here rather
than duplicate these rules.

- GitHub Copilot: `.github/copilot-instructions.md`, plus task-specific files
  in `.github/instructions/`.
- Claude Code: `CLAUDE.md`.
- Cursor: `.cursor/rules/`.

When a platform supports scoped or task-specific instruction files, consult
them when their description or path pattern matches the current task.

## The approach

Before implementing anything non-trivial:

1. **State the intent** — one sentence: what does this change do and why?
2. **Note relevant WAF pillars** — which of the five pillars apply, and is
   there an obvious trade-off to flag? (See below; consult
   `.github/ai-reference/waf/` for checklists.)
3. **Implement** — keep changes scoped; if scope expands materially, pause and
   re-state intent.
4. **Verify** — run tests/linters; confirm the change behaves as stated.

For large or ambiguous requests, ask one clarifying question rather than
guessing. For genuinely complex multi-team or long-lived work, consider the
companion spec-driven template instead.

## Record AI-assisted project work

After completing material AI-assisted project work, add a concise entry to
`AI_WORKLOG.md`. Summarize the request, outcome, important decisions,
verification, and any limitations or follow-up. Skip routine questions and
trivial changes whose intent is already obvious from the diff.

Do not copy raw prompts, transcripts, private reasoning, or tool output into
the worklog. Never record credentials, personal or sensitive data, or
proprietary prompt content. Treat the worklog as curated historical context,
not as a complete audit trail or a substitute for authoritative project
documentation.

Use a root `CHANGELOG.md`, when present, for user-facing release history.

## Well-Architected pillars (decision lens)

For non-trivial choices, note which pillar(s) you are optimizing for and what
you trade away. Full checklists are in `.github/ai-reference/waf/pillars/`.

- **Reliability** — reproducibility, error handling, idempotency, recovery.
- **Security** — least privilege, secret handling, dependency provenance,
  OWASP Top 10. Never commit credentials or PII.
- **Operational excellence** — automation, observability, CI, documentation.
- **Performance efficiency** — appropriate data structures, vectorization,
  avoiding needless recomputation.
- **Cost / sustainability** — compute and storage footprint, caching, batch
  vs. interactive workloads.

Inline format (keep it brief):
> _Optimizing for **reliability** (pinned deps, seeded RNG). Trading some
> setup time._

## Authoritative-source discipline

- Prefer official documentation over model memory. Use connected MCP doc
  servers and the project's own docs before asserting an API.
- When stating an API signature, function name, or config key, verify it from
  a source confirmed this session — not a guess.
- If a fact is uncertain, say so and point to where to verify.

## Project conventions

- **Languages and formats**: R and Python are the primary analysis languages;
  the repository also uses RDF/Turtle and shell automation. Use the
  task-specific standards in `.github/instructions/` when they apply to the
  file type or work being done, and validate RDF/Turtle with ontology-aware
  tooling rather than text checks alone.
- **Reproducibility first**: suggest `renv` for R and use Astral `uv` for
  Python dependency locking/execution, set seeds, and make scripts runnable
  end-to-end from a clean checkout.
- **Domain knowledge** (e.g. ecological metadata / EML / EDI) should live in
  portable `.agents/skills/` packages when practical. Copilot-specific agents,
  prompts, and instructions live under `.github/`.
- **Record material AI-assisted project work** in `AI_WORKLOG.md`; capture
  outcome and limitations, not raw prompts or transcripts.
