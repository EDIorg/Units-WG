# QUDT Unit Contribution Review: pilot-01

> **Superseded:** This run contains an incorrect duplicate-identity decision for
> `KiloM-PER-KiloM2` and `M2-PER-M3`. Use
> `../2026-08-15-pilot-01-rerun-01/` for the corrected output. This directory is
> retained as immutable evidence of the workflow defect.

## Provenance

- Run timestamp: `2026-08-15T12:13:54-07:00`
- Original input: `/tmp/test_units.csv`
- Copied input: `input/test_units.csv`
- Input SHA-256: `ab53e4813915c671d30d7512edb0e1d2cad384d12e244d79a4daddef4c57c3ae`
- Units-WG: `agentic` at `ea2487037a5fe4a361db5e83eb809beb15f4113e`
- QUDT source: `https://github.com/qudt/qudt-public-repo.git`, `main` at `6b8df6f429c45bc6fb0b25659d34f51954a148f9`
- Published QUDT endpoint: `https://www.qudt.org/fuseki/qudt/sparql`
- QUDT wiki checked: `2026-08-15`
- Optional unit-registry: `b9433fea496211fa3728c75055e0daa7e6b0efcb`

## Summary

| Candidate | Status | Disposition |
| --- | --- | --- |
| `KiloM-PER-KiloM2` | `existing-unit` | Reuse `unit:PER-KiloM` |
| `M2-PER-M3` | `existing-unit` | Reuse `unit:PER-M` |
| `KiloJ-PER-M2` | `ready-for-review` | Review one new Unit draft; reuse existing QKs and DV |

No new QuantityKinds or DimensionVectors are proposed.

## Duplicate and Equivalence Decisions

### `KiloM-PER-KiloM2`

The submitted qname is absent locally and in published QUDT, but the expression reduces exactly to reciprocal kilometre:

```text
(1000 m) / (1000000 m²) = 0.001 m⁻¹ = km⁻¹
```

QUDT already defines `unit:PER-KiloM` with multiplier `0.001`, vector `qkdv:A0E0L-1I0M0H0T0D0`, and generic `quantitykind:InverseLength` applicability. Creating the submitted qname would duplicate an existing unit.

### `M2-PER-M3`

The expression reduces exactly to reciprocal metre:

```text
m² / m³ = m⁻¹
```

QUDT already defines `unit:PER-M` with multiplier `1.0` and vector `qkdv:A0E0L-1I0M0H0T0D0`. Creating the submitted qname would duplicate an existing unit.

### `KiloJ-PER-M2`

The exact qname is absent locally and in published QUDT. It is not rejected merely because `unit:KiloN-PER-M` is algebraically equal: QUDT separately retains `unit:J-PER-M2` and `unit:N-PER-M`, reflecting different expression and semantic families.

Current QUDT contains `J-PER-M2`, `GigaJ-PER-M2`, `MegaJ-PER-M2`, and `MilliJ-PER-M2`, providing strong precedent for the missing kilo scale. The exact conversion is:

```text
(1000 J) / (1 m²) = 1000 J/m²
```

The derived vector is:

```text
KiloJ: A0E0L2I0M1H0T-2D0
minus M2: A0E0L2I0M0H0T0D0
result: A0E0L0I0M1H0T-2D0
```

The draft assigns the four QKs used by the base, giga, and milli precedents. `MegaJ-PER-M2` currently carries only `EnergyPerArea` and `StrainEnergyReleaseRate`, so the complete four-QK set requires human confirmation.

## Description Research

The new Unit description is intentionally limited to composition and exact magnitude. It makes no ecological-use claim because no measurement context was supplied.

The description is synthesized from current QUDT expression-family precedents and the BIPM definition of the kilo prefix. The optional EDI/LTER unit-registry contained no exact candidate match and was not used.

## Sources

- BIPM, SI prefixes: https://www.bipm.org/en/measurement-units/si-prefixes
- BIPM, SI base units: https://www.bipm.org/en/measurement-units/si-base-units
- QUDT Unit Vocabulary Submission Guidelines: https://github.com/qudt/qudt-public-repo/wiki/Unit-Vocabulary-Submission-Guidelines
- QUDT Commensurability guidance: https://github.com/qudt/qudt-public-repo/wiki/Commensurability-Composition-Semantics-and-Context
- Local and published QUDT resources named in `decisions.csv`

## Concerns and Recommendations

1. Confirm whether `KiloJ-PER-M2` should carry all four QKs used by `J-PER-M2`, `GigaJ-PER-M2`, and `MilliJ-PER-M2`, or only the two used by `MegaJ-PER-M2`.
2. Do not add `KiloM-PER-KiloM2` or `M2-PER-M3`; normalize source metadata to the existing reciprocal units.
3. Review the label, symbol, UCUM code, and generic description before submission.
4. Full QUDT Maven/SHACL validation is required before submission.

## Validation

- RDF parsing and local preflight passed for `units.ttl`,
	`quantitykinds.ttl`, and `dimensionvectors.ttl` with zero warnings.
- `conversionMultiplier 1000.0` and `conversionMultiplierSN 1.0E3` were
	confirmed numerically equivalent by preflight.
- All reused Unit, QuantityKind, and DimensionVector URIs were found in both the
	configured local QUDT checkout and the published SPARQL endpoint.
- The three submitted qnames were absent as `qudt:Unit` resources in both local
	and published QUDT.
- The proposed symbol `kJ/m²` and UCUM code `kJ.m-2` did not collide in the
	local aggregate unit vocabulary and follow the existing giga-, mega-, and
	milli-joule-per-square-metre pattern.
- Current QUDT declares no `qudt:exactMatch` or `qudt:scalingOf` relation between
	`unit:J-PER-M2` and `unit:N-PER-M`; the draft therefore does not invent one
	between the kilo expressions.
- The decision ledger contains three rows with explicit local and published
	evidence.

Artifact checksums:

```text
4bbfc69c4ec7fe0cf93bdf574c22ed8163372e19863e06961c466f794377e374  units.ttl
f941f13928c5e3687d8a9acded27406377246765e94fa75c0535fc6b500e2f82  quantitykinds.ttl
ab1cb2b3938412ad03b5402a940018a18c165911990098d2f180e672974f9fc9  dimensionvectors.ttl
ed4679254b3891d00648dc304bdf0d814e49613b301ad42da2450199c1e636dc  decisions.csv
b7dff97f45d00624c89d4b5a8d0b65f0124774426a0d8c5bf9348aae4a768fad  intake.csv
```

### Validation limitation

Full QUDT Maven/SHACL validation was not run because neither `mvn`, a Maven
wrapper, nor `java` is available in the current environment. The output is
review-ready, not submission-ready. Run the configured QUDT checkout's current
documented build after Maven and the required Java version are available.

Human domain review remains required, especially for the proposed QuantityKind
set on `unit:KiloJ-PER-M2`.
