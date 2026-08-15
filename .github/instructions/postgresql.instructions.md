---
description: "Use when work involves PostgreSQL schema design, SQL queries, performance analysis, migrations, or data loading. Applies automatically to SQL files and should be consulted for DB-adjacent R/Python changes."
applyTo: "**/*.sql"
---
# PostgreSQL Standards

## Tooling and execution model

- Use the VS Code PostgreSQL MCP tools as the default interface for interactive
  exploration, diagnostics, and safe query execution.
- Prefer read-first workflow:
  1. Inspect schema/context first.
  2. Run read-only queries.
  3. Apply modifications only after confirming object state and impact.
- Keep credentials out of files; use connection profiles and environment-backed
  secrets.

## Safety and reliability

- Fetch fresh database context before schema or data modifications to avoid
  drift-based mistakes.
- For non-trivial changes, make DDL idempotent where practical (`IF EXISTS`,
  `IF NOT EXISTS`) and include rollback notes.
- Treat production modifications as explicit, reviewed steps; avoid ad-hoc
  destructive operations.

## Performance and operations

- When performance is a concern, capture and inspect query plans before and
  after query/index changes.
- Prefer set-based SQL over row-by-row procedural patterns.
- Use bulk-load paths for large imports rather than one-row-at-a-time inserts.
- Record why an index or query rewrite was chosen (reliability/performance/cost
  trade-off).

## Integration with R/Python

- Keep SQL in version-controlled files for reusable logic.
- Parameterize queries; never string-concatenate untrusted input.
- Validate row counts and key constraints at pipeline boundaries.
