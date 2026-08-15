---
description: "Use when making non-trivial design or architecture decisions. Points to the local Well-Architected Framework reference material in .github/ai-reference/waf/ (pillar checklists, trade-off prompts) that the agent should consult and cite."
applyTo: "**"
---
# Well-Architected reference

For any non-trivial design choice, consult the local WAF reference in
[.github/ai-reference/waf/](../ai-reference/waf/) and **cite the pillar(s)** you are optimizing for
and what you trade away (see the base operating model in
`copilot-instructions.md`).

- Pillar checklists and trade-off prompts live in
  `.github/ai-reference/waf/pillars/`.
- These are version-controlled, reviewable references — prefer them (and live
  doc MCP servers) over model memory.
- Record local implementation rationale in the relevant code comment or commit
  message. For material AI-assisted work, add a concise `AI_WORKLOG.md` entry.
  Use a root `CHANGELOG.md`, when present, for user-facing release history. A
  separate spec file is not required for this lightweight workflow.
