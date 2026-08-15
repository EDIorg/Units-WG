---
description: "Python coding standards: Astral uv workflow, typing, ruff/black formatting, pytest, and reproducible environments. Applies when authoring or editing Python files."
applyTo: "**/*.py"
---
# Python Standards

- Use Astral `uv` as the default workflow for environments, dependencies, and
  command execution (`uv sync`, `uv lock`, `uv run ...`).
- Format with `black`, lint with `ruff`; run tooling through `uv run`.
- Use type hints on public functions; check with `mypy` or `pyright`.
- Pin dependencies with `uv.lock`; do not rely on un-pinned `requirements.txt`.
- Write tests with `pytest`; seed RNGs (`random`, `numpy`) for determinism.
- Validate input only at system boundaries; raise specific exceptions.
- Never hardcode secrets or absolute paths; use env vars / `pydantic-settings`.
- Keep functions small and pure where practical; isolate I/O from logic.
