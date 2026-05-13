---
name: qudt-ttl-generation
description: "Generate and validate QUDT unit TTL for ecology use cases. Use when creating new unit, quantitykind, and dimensionvector triples, checking for duplicates, and preparing QUDT-ready submission files."
argument-hint: "Provide candidate units, source context, and target output folder."
user-invocable: true
---

# QUDT TTL Generation

Use this skill to produce high-quality TTL files for new ecology units and related QUDT entities.

## Use When
- You have candidate ecology units that may be missing from QUDT.
- You need to generate `units.ttl`, `quantitykinds.ttl`, and `dimensionvectors.ttl` with consistent links.
- You want a reproducible preflight check before submitting to QUDT.

## Repository Inputs
- Candidate and historical work:
  - `AI_create_ttl/new_units_ttl_files/`
  - `AI_create_ttl/claude_AugSep_mob/outputs/`
- Modeling guidance:
  - `Creating_units/Guides/How_to_create_units.md`
  - `AI_create_ttl/claude_AugSep_mob/inputs/QUDT_Modeling_Rules.md`
- Cross-reference aids:
  - `AI_create_ttl/JP_Crossref_files/RelatedQKs.csv`
  - `AI_create_ttl/JP_Crossref_files/Relatedunits.csv`
  - `AI_create_ttl/JP_Crossref_files/RelatedQKs_summary.csv`
  - `AI_create_ttl/JP_Crossref_files/RelatedUnits_summary.csv`

## Procedure
1. Build candidate list.
   - Normalize candidate IDs into QUDT-style names (for example `KiloGM-PER-HA-DAY`).
   - Include context notes (dataset, domain meaning, expected quantity semantics).
2. Reuse before create.
   - Check whether each candidate unit already exists.
   - Check whether the dimension vector already exists.
   - Check whether one or more suitable quantity kinds already exist for that vector.
3. Determine dimensional semantics.
   - Compute the dimension vector from the physical quantity, not from text style.
   - For count-like units, do not use amount-of-substance exponent unless the quantity is moles.
   - Use `D1` only for fully dimensionless vectors.
4. Generate three files.
   - `units.ttl`: every unit with one `qudt:hasDimensionVector` and one `qudt:hasQuantityKind`.
   - `quantitykinds.ttl`: only new quantity kinds that are truly needed.
   - `dimensionvectors.ttl`: only new vectors not already defined.
5. Run preflight checks.
   - Run `./.github/skills/qudt-ttl-generation/scripts/ttl_preflight_check.sh <ttl-file>...`.
   - Resolve all errors before review.
6. Produce review artifacts.
   - Add a compact table showing, for each candidate unit, whether Unit/QK/DV already existed and what was newly created.

## Required Unit Properties
- `rdfs:label`
- `dcterms:description`
- `qudt:plainTextDescription`
- `qudt:hasDimensionVector`
- `qudt:hasQuantityKind`
- `qudt:conversionMultiplier`
- `rdfs:isDefinedBy`

## Output Standard
- Keep placeholders out of final submission files.
- Disallow `quantitykind:Unknown`, `quantitykind:unknown`, and unresolved `TO DO` comments in submission-ready TTL.
- Keep QUDT version URIs consistent within each generated file.

## References
- [rules](./references/rules-summary.md)
- [preflight checker](./scripts/ttl_preflight_check.sh)
