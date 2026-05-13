# Copilot Workflow for QUDT TTL in This Repository

## Quick Start (First-Time Contributors)
1. Open a chat in this repository and describe your candidate units.
2. Ask to use the QUDT TTL Generation skill or QUDT TTL Curator agent.
  - Use the skill for interactive drafting and guidance in your current chat.
  - Use the agent for end-to-end curation runs (batching, file generation, validation summary).
3. Request outputs for units, quantity kinds, and dimension vectors.
4. Point Copilot to your target output folder.
5. Ask it to reuse existing QUDT resources before creating new ones.
6. Run preflight: ./.github/skills/qudt-ttl-generation/scripts/ttl_preflight_check.sh <ttl-file> [more ttl files]
  - Preflight fails on placeholders and TODOs, checks required unit fields, validates D1 usage, and warns on multi-QK units or mixed QUDT version URIs.
7. Fix all ERROR items reported by preflight.
8. Re-run preflight until it passes.
9. Review semantic choices for quantity kind and dimension vector.
10. Submit only files with no placeholders and no TODO markers.

This folder contains two Copilot customizations for QUDT unit development:

- Skill: .github/skills/qudt-ttl-generation
- Agent: .github/agents/qudt-ttl-curator.agent.md

## Which one to use

- Use the skill for repeatable steps and guidance while you stay in your current chat mode.
- Use the agent for focused, end-to-end curation work when you want tighter behavior and a structured output report.

## Auto-discovery vs explicit invocation

- Auto-discovery: Copilot may load the skill automatically when your request clearly matches QUDT TTL generation work.
- Explicit invocation: More reliable for important tasks. Prefer explicit invocation when preparing files for review or submission.

## Recommended interaction pattern

1. Start with unit candidates and context
- Provide candidate unit IDs and semantic notes (what is being measured, ecological context, known conversion intent).

2. Ask Copilot to apply the QUDT workflow
- Ask to use the QUDT TTL Generation skill or the QUDT TTL Curator agent.
- Request outputs as needed: units, quantity kinds, and dimension vectors.

3. Run preflight checks before review
- Command:
  ./.github/skills/qudt-ttl-generation/scripts/ttl_preflight_check.sh <ttl-file> [more ttl files]

4. Resolve all errors, then re-run preflight
- Keep iterating until preflight passes.

## What preflight enforces

- No placeholder quantity kind values such as quantitykind:Unknown or quantitykind:unknown
- No unresolved TODO markers
- Required unit fields in each unit block:
  - qudt:hasDimensionVector
  - qudt:hasQuantityKind
  - qudt:conversionMultiplier
  - qudt:plainTextDescription
  - rdfs:isDefinedBy
- Warns when mixed QUDT unit-version URIs appear in one file

## Suggested prompt style

Use concise requests that include:

- Candidate units
- Source files to consult
- Output file targets
- Requirement to run preflight and report findings

Example:
Create submission-ready TTL for these units using repository modeling rules, reuse existing QUDT resources when possible, write outputs to AI_create_ttl/new_units_ttl_files, run the preflight script, and summarize only remaining issues.

## Primary references in this repository

- Creating_units/Guides/How_to_create_units.md
- AI_create_ttl/claude_AugSep_mob/inputs/QUDT_Modeling_Rules.md
- AI_create_ttl/JP_Crossref_files/
