# QUDT Unit Contribution Review: pilot-01-rerun-01

## Provenance

- Run timestamp: `2026-08-15T12:31:57-07:00`
- Input copied from: `../2026-08-15-pilot-01/input/test_units.csv`
- Copied input: `input/test_units.csv`
- Input SHA-256: `ab53e4813915c671d30d7512edb0e1d2cad384d12e244d79a4daddef4c57c3ae`
- Units-WG: `agentic` at `ea2487037a5fe4a361db5e83eb809beb15f4113e`
- QUDT source: `https://github.com/qudt/qudt-public-repo.git`, `main` at `6b8df6f429c45bc6fb0b25659d34f51954a148f9`
- Published QUDT endpoint and wiki checked: `2026-08-15`
- Optional unit-registry: not used

## Correction from the Original Pilot

The original pilot incorrectly treated algebraically reduced Units as duplicates. Current QUDT preserves expression-specific Units such as `M-PER-M2` alongside `PER-M`, and `J-PER-M2` alongside `N-PER-M`. The corrected workflow therefore drafts every valid exact qname that is absent, while recording algebraic equivalents as evidence.

## Summary

| Candidate | Status | Disposition |
| --- | --- | --- |
| `KiloM-PER-KiloM2` | `ready-for-review` | New Unit; reuse `InverseLength` and existing DV |
| `M2-PER-M3` | `ready-for-review` | New Unit; reuse `InverseLength` and existing DV |
| `KiloJ-PER-M2` | `ready-for-review` | New Unit; reuse four existing QKs and existing DV |

No new QuantityKinds or DimensionVectors are proposed.

## Candidate Decisions

### `KiloM-PER-KiloM2`

The exact qname is absent. The expression is preserved as kilometre per square kilometre even though it reduces to reciprocal kilometre:

```text
(1000 m) / (1000000 m²) = 0.001 m⁻¹
```

Factors are `KiloM¹` and `KiloM⁻²`. The existing generic QK is `InverseLength`; specialized same-vector meanings require source-variable context.

### `M2-PER-M3`

The exact qname is absent. The expression is preserved as square metre per cubic metre:

```text
m² / m³ = m⁻¹
```

Factors are `M²` and `M⁻³`. `InverseLength` is reused. The historical `AreaPerVolume` proposal is not used because that QK does not exist in current QUDT and no supplied measurement context supports creating it.

### `KiloJ-PER-M2`

The exact qname is absent. Current QUDT contains the base, giga, mega, and milli joule-per-square-metre expressions, strongly supporting the missing kilo member.

```text
(1000 J) / m² = 1000 J/m²
```

Factors are `KiloJ¹` and `M⁻²`. The draft uses the four QKs assigned to the base, giga, and milli precedents. The mega precedent carries only `EnergyPerArea` and `StrainEnergyReleaseRate`, so human review should confirm whether `EnergyFluence` and `RadiantFluence` also belong on the kilo unit.

## Comparison with the Manual KiloJ Draft

Improvements retained from the manual draft:

- Explicit `qudt:expression`
- Explicit `qudt:hasFactorUnit` nodes
- CGS-family and SI applicable systems, matching nearby scaled precedents
- Symbol and UCUM code

Corrections to the manual draft:

- Uses current `qudt:unitForQuantityKind` instead of legacy `qudt:hasQuantityKind`
- Uses the version placeholder in `rdfs:isDefinedBy`
- Adds `qudt:plainTextDescription`
- Avoids classifying the unit generically as “Energy density”
- Avoids repetitive aliases in a `qudt:LatexString` with no substantive LaTeX
- Omits unsupported `qudt:definedUnitOfSystem`; current scaled precedents are inconsistent
- Includes only verified English labels rather than unreviewed copied translations
- Omits `a qudt:FactorUnit` on blank nodes because QUDT's own inference output does not require that explicit type

## Sources

- BIPM SI prefixes: https://www.bipm.org/en/measurement-units/si-prefixes
- BIPM SI base units: https://www.bipm.org/en/measurement-units/si-base-units
- QUDT Unit Vocabulary Submission Guidelines: https://github.com/qudt/qudt-public-repo/wiki/Unit-Vocabulary-Submission-Guidelines
- QUDT Commensurability guidance: https://github.com/qudt/qudt-public-repo/wiki/Commensurability-Composition-Semantics-and-Context
- Local and published QUDT resources cited in `decisions.csv`

## Concerns and Recommendations

1. Confirm the four-QK set for `KiloJ-PER-M2`; the scaled precedents are inconsistent.
2. Review English labels, symbols, UCUM codes, and generic descriptions before submission.
3. Add verified translated labels only when language review or exact analogous QUDT labels support them.
4. Run full QUDT Maven/SHACL validation before submission.

## Validation

- RDF parsing and preflight passed for all three Turtle files with zero
	warnings.
- Every compound Unit has explicit `qudt:expression` and factor nodes matching
	its qname:
	- `KiloM-PER-KiloM2`: `KiloM¹`, `KiloM⁻²`
	- `M2-PER-M3`: `M²`, `M⁻³`
	- `KiloJ-PER-M2`: `KiloJ¹`, `M⁻²`
- Exact Decimal arithmetic over verified factor multipliers reproduced all
	declared multipliers: `0.001`, `1.0`, and `1000.0`.
- Decimal and scientific multiplier representations are numerically equivalent.
- All referenced factor Units, QuantityKinds, and DimensionVectors exist in
	both local and published QUDT.
- All three proposed exact qnames are absent from both local and published QUDT.
- Proposed symbols and UCUM codes do not collide in the local aggregate unit
	vocabulary.
- The decisions ledger contains three `ready-for-review` rows with conversion,
	dimensional, semantic, and description evidence.

Artifact checksums:

```text
e3580c08c5d58926c1f09d6d93c4bf3a3593702176292d023d4f813fc13d9461  units.ttl
f941f13928c5e3687d8a9acded27406377246765e94fa75c0535fc6b500e2f82  quantitykinds.ttl
ab1cb2b3938412ad03b5402a940018a18c165911990098d2f180e672974f9fc9  dimensionvectors.ttl
cfcf1dadae9d12d2c7823ec3ef7726a27d9768dc29c7ee4602fee3ae3830b398  decisions.csv
b7dff97f45d00624c89d4b5a8d0b65f0124774426a0d8c5bf9348aae4a768fad  intake.csv
```

### Validation limitation

Full QUDT Maven/SHACL validation was not run because `mvn`, a Maven wrapper,
and `java` are unavailable in the current environment. The rerun is
review-ready, not submission-ready. Run the configured QUDT checkout's current
build before submission.

## Workflow Feedback

| Field | Value |
| --- | --- |
| Category | `identity` |
| Candidate | `KiloM-PER-KiloM2`, `M2-PER-M3` |
| Observation | The original pilot collapsed exact submitted factor expressions into algebraically simpler existing Units and omitted two valid absent qnames. |
| Evidence | Current QUDT retains `M-PER-M2` beside `PER-M` and `J-PER-M2` beside `N-PER-M`; QUDT factor-unit inference and validation operate on qname factorization. |
| Impact | Two of three candidate Unit drafts were incorrectly omitted. |
| Frequency | First observed occurrence; high-impact systemic defect. |
| Proposed change | Treat factor expression as Unit identity, record algebraic equivalents as evidence, and require explicit expressions and factor units in review drafts. |
| Disposition | `accepted` and implemented with positive and negative compound-unit fixtures. |
