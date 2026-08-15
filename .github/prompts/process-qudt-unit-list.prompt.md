---
name: Process QUDT Unit List
description: "Process an attached or named CSV, TSV, or legacy text unit list through the Units-WG QUDT contribution workflow."
agent: "QUDT Ontology Engineer"
argument-hint: "Attach or name the unit-list file, then optionally specify a run name and QUDT source path."
---

Process the attached or named candidate-unit file using the
[QUDT unit contribution skill](../../.agents/skills/qudt-unit-contribution/SKILL.md).

If no input file is supplied, ask for one. Resolve the QUDT source root without
guessing a preferred fork. Use a new run-specific output directory and produce
the three Turtle files, `decisions.csv`, and `review.md` required by the skill.

Research and propose context, conversions, labels, QuantityKinds, and
DimensionVectors when the user supplies only unit expressions. Consolidate
questions only for material ambiguities that remain after research. Continue
unblocked work, but do not generate RDF for unresolved candidates. Finish with
output paths, status counts, validation results, concerns, and recommendations.
