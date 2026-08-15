---
description: "Use when the deliverable is a data analysis rather than software: exploratory analysis, statistics/modeling, figures, or reports/notebooks. Frames the work around a research question + success metric, and emphasizes reproducible pipelines."
applyTo: "**/*.{Rmd,rmd,qmd,ipynb}"
---
# Analysis (analyze, don't just build)

Same operating model as `copilot-instructions.md`, with data-analysis framing.
The deliverable is a **validated result** (figures, tables, model, report) — not
software. Treat reproducibility as the reliability pillar.

For R or Python utility software whose deliverable is not an analysis, use the
language-specific instructions without imposing the research-question framing
below.

## Framing analysis work

Before writing code, briefly state:
- **Question** — the research question, stated so it can be answered
  yes/no/by-how-much.
- **Success** — a defensible result + the metric/threshold that judges it
  (e.g. effect estimate with CI, validation score, passing assumption checks).
- **Scope** — what analyses are explicitly out of scope (avoid scope creep /
  p-hacking).

## Reliability = reproducibility

- Prefer pinned deps (suggest `renv` for R; use Astral `uv` lock/workflow for
  Python); set seeds; raw data immutable; outputs regenerable from scratch.
- Prefer a DAG pipeline (`targets` in R, Snakemake/`make` in Python) over
  ad-hoc scripts so only changed steps rerun — also helps Performance & Cost
  pillars.
- Notebooks for exploration; promote stable steps into scripted pipeline stages.

## Verify

- Re-run end-to-end from a clean checkout; outputs match.
- State assumptions and check them; report uncertainty, not just point estimates.
- If publishing data, follow the repository's current metadata and packaging
  workflow; do not assume a named metadata skill is installed.
