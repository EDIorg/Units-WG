---
description: "Use when the deliverable is a data analysis rather than software: exploratory analysis, statistics/modeling, figures, or reports/notebooks. Frames the work around a research question + success metric, and emphasizes reproducible pipelines."
applyTo: "**/*.{Rmd,rmd,qmd,ipynb}"
---
# Analysis (analyze, don't just build)

Same operating model as `AGENTS.md`, with data-analysis framing. Applies when
the deliverable is a **validated result** (figures, tables, model, report)
rather than software — including R and Python scripts that produce one. Treat
reproducibility as the reliability pillar.

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
