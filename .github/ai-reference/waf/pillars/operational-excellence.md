# Pillar: Operational Excellence (research-software context)

Automation, observability, and documentation so the work is maintainable.

## Checklist
- [ ] CI runs tests/linters on every change.
- [ ] Formatting/linting automated (`styler`/`lintr`, `ruff`/`black`).
- [ ] Rationale for non-obvious decisions captured in comments, commit
      messages, or the appropriate worklog/changelog.
- [ ] If the project keeps an `AI_WORKLOG.md`, material AI-assisted work is
      summarized there without raw prompts or transcripts.
- [ ] Logs/progress visible for long-running pipelines.
- [ ] README explains setup, run, and verify steps.

## Trade-off prompts
- Could a new contributor run and verify this without tribal knowledge?
- What manual step should be automated next?
