# Rules Summary for QUDT Ecology Unit Contributions

This is a condensed checklist derived from this repository's modeling documentation.

## Core Principles
- Reuse existing Unit, QuantityKind, and DimensionVector resources when possible.
- Only create new QuantityKinds and DimensionVectors when no suitable existing resource matches.
- Ensure each Unit has exactly one `qudt:hasDimensionVector` and one `qudt:hasQuantityKind`.

## Dimension Vector Rules
- Vector label must match the exact vector string.
- Use exponents for `A`, `E`, `L`, `I`, `M`, `H`, `T`, and `D`.
- Use `D1` only for fully dimensionless vectors.
- Do not use `A` for simple counts; use dimensionless semantics for counts.

## QuantityKind Rules
- Name should describe quantity semantics, not unit formatting.
- Multiple quantity kinds can share the same vector.
- Choose existing quantity kinds when semantics match.

## Unit Rules
- Always include `qudt:hasDimensionVector` and `qudt:hasQuantityKind`.
- Include conversion to SI via `qudt:conversionMultiplier`.
- Keep labels, symbol, and descriptions clear and physically consistent.
- Avoid placeholder values in submission-ready files.

## Submission Quality Gate
- No `quantitykind:Unknown` or `quantitykind:unknown`.
- No unresolved `TO DO` comments.
- No mixed QUDT version URIs in one generated file.
