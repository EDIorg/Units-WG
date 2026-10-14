---
description: "Python coding standards: Astral uv workflow, ruff for linting and formatting, pytest, and reproducible environments. Applies when authoring or editing Python files."
applyTo: "**/*.py"
---
# Python Standards

- Use Astral `uv` for environments, dependencies, and running commands
  (`uv sync`, `uv lock`, `uv run ...`); commit `uv.lock`.
- Use `ruff` for both linting and formatting (`uv run ruff check`,
  `uv run ruff format`); do not add `black`.
- Write tests with `pytest`; seed RNGs (`random`, `numpy`) for determinism.
- Type hints on public functions; a type checker is optional.
- Never hardcode secrets or absolute paths; use environment variables.
