# Comparison with the Earlier Combined Unit Effort

## Scope

This report compares the output of the `2026-09-07-mob-units-gpt` run with
[`new_units_concat_jp_mm_mob.ttl`](../../new_units_ttl_files/new_units_concat_jp_mm_mob.ttl),
an earlier combined effort initially generated with Claude.ai and subsequently
edited by project contributors.

The comparison covers candidate disposition, RDF validity, QuantityKind and
DimensionVector assignments, conversion multipliers, and metadata completeness.
The earlier file is treated as historical evidence, not as current QUDT
authority.

## Executive Summary

The recent run is a more conservative and auditable review baseline. The earlier
file defines a Unit for all 84 submitted expressions. The recent run reviews the
same 84 candidates but emits only 51 new Unit drafts because it separately
identifies 5 existing QUDT resources, 24 candidates needing semantic context,
and 4 candidates blocked by the meaning of `MO`.

All 51 recent drafts are also present in the earlier file. Its 33 additional
Unit definitions correspond exactly to the candidates that the recent workflow
withheld or classified as existing. The difference is therefore deliberate
triage rather than missed input.

The earlier file currently fails Turtle parsing because the description for
`MicroMOL-PER-L-DAY` lacks a terminating semicolon. The recent `units.ttl`
passes the workflow preflight with zero warnings.

## Candidate Coverage

| Disposition in recent run |   Earlier file | Recent `units.ttl` |
|---------------------------|---------------:|-------------------:|
| `ready-for-review`        | 51 definitions |     51 definitions |
| `existing-unit`           |  5 definitions |      0 definitions |
| `needs-clarification`     | 24 definitions |      0 definitions |
| `blocked-conversion`      |  4 definitions |      0 definitions |
| Total candidates reviewed |             84 |                 84 |
| Unit subjects emitted     |             84 |                 51 |

The five definitions omitted because equivalent QUDT resources already exist
are:

- `W-HR-PER-M2`
- `GM-PER-CM3`, canonically `GM-PER-CentiM3`
- `MicroMOL-PER-MOLE`, canonically `MicroMOL-PER-MOL`
- `CentiM3-PER-CentiM3`
- `GM-PER-HectoGM`

The four conversion blocks are:

- `MilliGM-PER-KiloGM-MO`
- `KiloGM-PER-HA-MO`
- `GM-PER-M2-MO`
- `MilliGM-PER-GM-MO`

QUDT `unit:MO` is a synodic month, while a Julian month and a calendar month
have different durations. Because month occurs in the denominator, choosing a
meaning changes each SI conversion multiplier.

The remaining 24 historical definitions were withheld because the submitted
expressions do not establish enough semantic context to select a QuantityKind.
They include count-normalized, mass-normalized rate, mole-ratio, and PPM-rate
expressions. Their individual rationales are recorded in
[`decisions.csv`](decisions.csv).

## RDF and Modeling Quality

| Check                          | Earlier combined file | Recent run |
|--------------------------------|----------------------:|-----------:|
| Turtle parses unchanged        |                    No |        Yes |
| Current preflight              |        1 syntax error | 0 warnings |
| Description                    |              84 of 84 |   51 of 51 |
| Plain-text description         |              80 of 84 |   51 of 51 |
| Conversion multiplier          |              84 of 84 |   51 of 51 |
| Scientific-notation multiplier |              82 of 84 |   51 of 51 |
| DimensionVector                |              84 of 84 |   51 of 51 |
| Explicit expression            |               3 of 84 |   50 of 51 |
| Explicit factor decomposition  |              10 of 84 |   50 of 51 |
| Symbol                         |              84 of 84 |   51 of 51 |
| UCUM code                      |              10 of 84 |    0 of 51 |
| Applicable system              |              84 of 84 |    0 of 51 |

The earlier file uses `qudt:hasQuantityKind`; the recent run uses
`qudt:unitForQuantityKind`. The earlier file also mixes unversioned, `2.1`, and
`3.1.2` `rdfs:isDefinedBy` targets. The recent output consistently uses the
QUDT build placeholder:

```turtle
rdfs:isDefinedBy <http://qudt.org/$$QUDT_VERSION$$/vocab/unit>
```

The recent descriptions are deliberately generic and composition-based. The
earlier file contains useful domain-oriented descriptions, selected UCUM codes,
and application-system assertions, but some descriptions infer ecological uses
that were not supplied with the candidate expressions. Those fields should be
recovered only after their source and applicability are verified.

## QuantityKind Differences

The files agree on the QuantityKind set for 36 of the 51 shared drafts. They
differ for the following 15:

| Unit                  | Earlier assignment                   | Recent assignment                             |
|-----------------------|--------------------------------------|-----------------------------------------------|
| `BQ-PER-GM`           | `SpecificActivity`                   | `MassicActivity`                              |
| `BQ-PER-MilliGM`      | `MassicRadioactivity`                | `MassicActivity`                              |
| `GM-PER-YR`           | `MassFlowRate`                       | `MassPerTime`                                 |
| `KiloJ-PER-M2-DAY`    | `Irradiance`                         | `PowerPerArea`                                |
| `KiloM3`              | `SectionModulus`, `Volume`           | `Volume`                                      |
| `M2-PER-DAY`          | `AreaPerTime`, three diffusion kinds | `AreaPerTime`                                 |
| `M3-PER-M2-DAY`       | `Velocity`                           | `VolumetricFlux`                              |
| `MI_US-PER-SEC`       | `Velocity`                           | `Speed`                                       |
| `MOL-PER-L-ATM`       | `PressureNormalizedMolarity`         | `PressureBasedAmountOfSubstanceConcentration` |
| `MegaJ-PER-M2-YR`     | `Irradiance`                         | `PowerPerArea`                                |
| `MicroBQ-PER-GM`      | `MassicRadioactivity`                | `MassicActivity`                              |
| `MicroM3-PER-L`       | `DimensionlessRatio`                 | `VolumeFraction`                              |
| `MilliL-PER-MilliL`   | `VolumeRatio`                        | `VolumeFraction`                              |
| `MilliMOL-PER-M3-MIN` | `MolarVolumetricRate`                | `CatalyticActivityConcentration`              |
| `TON_US-PER-AC-FT`    | `Density`                            | `MassDensity`                                 |

The recent workflow generally selects a current, generic, or
composition-specific QUDT kind and avoids attaching specialized meanings that
cannot be established from the unit expression alone. These 15 assignments are
priority items for human ontology review.

Both efforts assign all four supported QuantityKinds to `KiloJ-PER-M2`:

- `EnergyFluence`
- `EnergyPerArea`
- `RadiantFluence`
- `StrainEnergyReleaseRate`

## Conversion Comparison

Twenty shared units have numerically identical conversion multipliers. Of the
31 differences, 24 are rounding or added-precision changes with relative
differences no greater than $10^{-8}$.

Four historical values have large scale errors:

| Unit             |     Earlier multiplier |      Recent multiplier |          Difference |
|------------------|-----------------------:|-----------------------:|--------------------:|
| `KiloM3`         |              $10^{-9}$ |                 $10^9$ | factor of $10^{18}$ |
| `GM-PER-HA-YR`   | $3.1688\times10^{-12}$ | $3.1688\times10^{-15}$ |     factor of 1,000 |
| `PicoMOL-PER-GM` |              $10^{-6}$ |              $10^{-9}$ |     factor of 1,000 |
| `GM-PER-YR`      | $3.1688\times10^{-10}$ | $3.1688\times10^{-11}$ |        factor of 10 |

Three smaller differences exceed the comparison threshold and arise from the
constituent values used for US customary units:

| Unit               |       Earlier multiplier |        Recent multiplier | Relative difference |
|--------------------|-------------------------:|-------------------------:|--------------------:|
| `OZ-PER-AC`        | $7.0094724\times10^{-6}$ | $7.0053197\times10^{-6}$ | $5.93\times10^{-4}$ |
| `MI_US-PER-SEC`    |               $1609.344$ |            $1609.347219$ | $2.00\times10^{-6}$ |
| `TON_US-PER-AC-FT` |            $0.735467496$ |            $0.735466638$ | $1.17\times10^{-6}$ |

The recent values are reproducibly derived from the constituent multipliers in
the recorded QUDT checkout. They should be rechecked against the current QUDT
checkout before submission, particularly where QUDT's stored customary-unit
value is rounded.

## DimensionVector Difference

`MOL-PER-L-ATM` is the only shared Unit with a different DimensionVector:

- Earlier: `A1E0L2I0M-1H0T2D0`
- Recent: `A1E0L-2I0M-1H0T2D0`

For amount of substance divided by volume and pressure, the length exponent is
$-2$, so the recent vector is dimensionally consistent.

## Auditability and Remaining Gaps

The recent run adds artifacts that the earlier standalone Turtle file does not
provide:

- normalized intake preserving the original expressions;
- one decision record per candidate;
- local and published evidence;
- explicit conversion equations and exactness flags;
- confidence, issues, and recommendations;
- a review summary separating drafts from unresolved candidates.

The recent package remains review-only. Its local QUDT checkout was behind
upstream, and the full Maven/SHACL build was not available. These limitations
are documented in [`review.md`](review.md).

## Recommendation

Use the recent package as the baseline for further review and eventual QUDT
submission. Do not copy the earlier file's additional 33 definitions into the
new Turtle wholesale. Instead:

1. Resolve the 24 semantic questions from source-variable context.
2. Confirm the intended meaning of `MO` before generating the four month-based
   conversions.
3. Review the 15 QuantityKind differences with domain experts.
4. Recheck all identities and constituent values against a current QUDT
   checkout.
5. Selectively recover verified translations, UCUM codes, and applicable-system
   metadata from the earlier file.
6. Run QUDT's full Maven/SHACL validation before proposing a contribution.

## Method Notes

Counts in the invalid earlier file were first obtained lexically. For graph-level
comparison, its missing semicolon was repaired in memory only; the historical
file was not modified. That single repair allowed RDF parsing and comparison of
all 84 Unit subjects. Numeric differences were evaluated with decimal arithmetic;
$10^{-8}$ relative difference was used only to separate rounding-scale changes
from values needing closer inspection.
