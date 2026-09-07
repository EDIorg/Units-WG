# QUDT Unit Contribution Review: 2026-09-07-mob-units-gpt

## Review Status

This is a review-only package, not a submission-ready QUDT change. Human domain
review and validation against a current QUDT checkout are required. The resolved
local checkout is stale relative to official upstream, and the user explicitly
prohibited modifying or building that checkout.

## Provenance

- Run timestamp: `2026-09-07T22:25:18Z`
- Original input: `/tmp/mob_units_raw.csv`
- Copied input: `input/mob_units_raw.csv`
- Input SHA-256:
  `de37bf420b655941a30cda7a031ec1f465d5cc014403cc1009d88db575560443`
- Units-WG: `agentic` at `c4b8434cd93f940acf531c13959d9999f6667282`
- QUDT source: `/home/srearl/localRepos/qudt-public-repo`,
  `https://github.com/qudt/qudt-public-repo.git`, `main` at
  `6b8df6f429c45bc6fb0b25659d34f51954a148f9` (`3.5.1-SNAPSHOT`)
- Official upstream `main` checked 2026-09-07:
  `75e5ef6a3bde9a01760007d148c6d52409f6a067`
- Published endpoint: `https://www.qudt.org/fuseki/qudt/sparql`, checked
  2026-09-07
- Official QUDT wiki checked: 2026-09-07
- Optional EDI/LTER `unit-registry`: not used

### Prominent Source Limitation

The local QUDT source commit `6b8df6f429c45bc6fb0b25659d34f51954a148f9` differs
from current official upstream `75e5ef6a3bde9a01760007d148c6d52409f6a067`. Local
vocabulary structure controlled this review package, while the live endpoint was
used for published identity checks. Rebase these proposals onto a current clean
checkout and repeat every URI, collision, and build check before submission.

## Summary

| Status                | Count |
|-----------------------|------:|
| `ready-for-review`    |    51 |
| `existing-unit`       |     5 |
| `needs-clarification` |    24 |
| `blocked-conversion`  |     4 |
| Total                 |    84 |

- New Unit drafts: 51
- New QuantityKind drafts: 0
- New DimensionVector drafts: 0
- All drafted Units reuse verified current QuantityKinds and DimensionVectors.
- Unresolved candidates are retained in `decisions.csv` but omitted from Turtle.

## Existing Resources

| Submitted expression  | Canonical QUDT Unit   | Result                                      |
|-----------------------|-----------------------|---------------------------------------------|
| `W-HR-PER-M2`         | `W-HR-PER-M2`         | Exact local and published match             |
| `GM-PER-CM3`          | `GM-PER-CentiM3`      | Canonical prefix spelling; do not duplicate |
| `MicroMOL-PER-MOLE`   | `MicroMOL-PER-MOL`    | Canonical factor spelling; do not duplicate |
| `CentiM3-PER-CentiM3` | `CentiM3-PER-CentiM3` | Exact local and published match             |
| `GM-PER-HectoGM`      | `GM-PER-HectoGM`      | Exact local and published match             |

The existing `MicroMOL-PER-MOL` resource is categorized by
`quantitykind:Unknown`; this package records that fact instead of silently
assigning MoleFraction.

## Drafted Units by QuantityKind

| Reused QuantityKind                           | Count | Candidate Units                                                                                                                       |
|-----------------------------------------------|------:|---------------------------------------------------------------------------------------------------------------------------------------|
| `ActivityConcentration`                       |     1 | `BQ-PER-MilliL`                                                                                                                       |
| `AmountOfSubstanceConcentration`              |     2 | `NanoMOL-PER-MilliL`, `MicroMOL-PER-M3`                                                                                               |
| `AmountOfSubstancePerMass`                    |     2 | `MicroMOL-PER-MilliGM`, `PicoMOL-PER-GM`                                                                                              |
| `AreaPerTime`                                 |     1 | `M2-PER-DAY`                                                                                                                          |
| `CatalyticActivityConcentration`              |     2 | `MicroMOL-PER-L-DAY`, `MilliMOL-PER-M3-MIN`                                                                                           |
| `EnergyFluence`                               |     1 | `KiloJ-PER-M2`                                                                                                                        |
| `EnergyPerArea`                               |     1 | `KiloJ-PER-M2`                                                                                                                        |
| `InverseLength`                               |     2 | `KiloM-PER-KiloM2`, `M2-PER-M3`                                                                                                       |
| `Mass`                                        |     1 | `GigaGM`                                                                                                                              |
| `MassConcentrationRateOfChange`               |     6 | `MilliGM-PER-L-HR`, `MilliGM-PER-L-SEC`, `MilliGM-PER-MilliL-DAY`, `MicroGM-PER-MilliL-DAY`, `GM-PER-M3-DAY`, `GM-PER-M3-YR`          |
| `MassDensity`                                 |     1 | `TON_US-PER-AC-FT`                                                                                                                    |
| `MassPerArea`                                 |     6 | `TON_US-PER-AC`, `MicroGM-PER-M2`, `MicroGM-PER-MilliM2`, `OZ-PER-AC`, `TON_US-PER-HA`, `TONNE-PER-AC`                                |
| `MassPerAreaTime`                             |     7 | `GM-PER-HA-DAY`, `KiloGM-PER-KiloM2-YR`, `KiloGM-PER-M2-YR`, `GM-PER-HA-HR`, `NanoGM-PER-M2-HR`, `MilliGM-PER-M2-MIN`, `GM-PER-HA-YR` |
| `MassPerTime`                                 |     1 | `GM-PER-YR`                                                                                                                           |
| `MassicActivity`                              |     3 | `BQ-PER-GM`, `BQ-PER-MilliGM`, `MicroBQ-PER-GM`                                                                                       |
| `MolarFlowRate`                               |     3 | `MilliMOL-PER-SEC`, `MOL-PER-DAY`, `NanoMOL-PER-MIN`                                                                                  |
| `PowerPerArea`                                |     2 | `KiloJ-PER-M2-DAY`, `MegaJ-PER-M2-YR`                                                                                                 |
| `PressureBasedAmountOfSubstanceConcentration` |     1 | `MOL-PER-L-ATM`                                                                                                                       |
| `RadiantFluence`                              |     1 | `KiloJ-PER-M2`                                                                                                                        |
| `Speed`                                       |     1 | `MI_US-PER-SEC`                                                                                                                       |
| `StrainEnergyReleaseRate`                     |     1 | `KiloJ-PER-M2`                                                                                                                        |
| `Volume`                                      |     1 | `KiloM3`                                                                                                                              |
| `VolumeFraction`                              |     2 | `MicroM3-PER-L`, `MilliL-PER-MilliL`                                                                                                  |
| `VolumePerArea`                               |     4 | `CORD-PER-AC`, `FT3-PER-AC`, `MegaL-PER-KiloM2`, `MicroM3-PER-MilliM2`                                                                |
| `VolumetricFlux`                              |     1 | `M3-PER-M2-DAY`                                                                                                                       |

Descriptions are deliberately generic and composition-based. No ecological use
was inferred from an expression alone. Every compound or powered draft retains
its submitted expression and explicit factor nodes.

`KiloJ-PER-M2` retains all four QuantityKinds used by the base, giga-, and
milli-joule-per-square-metre precedents. The user approved this treatment after
reviewing the inconsistent `MegaJ-PER-M2` precedent, whose two-kind assignment
appears to reflect incomplete curation rather than a scale-dependent semantic
difference.

## Questions Requiring Human Review

1. Count semantics (6 candidates): For `GM-PER-NUM`, `MilliM-PER-NUM`,
   `NUM-PER-L-MIN`, `NUM-PER-NanoGM`, `NUM-PER-GM-MIN`, and `NUM-PER-KiloM`,
   identify what `NUM` counts and the entity or sample represented by each
   denominator.
2. Mass-normalized rates (9 candidates): For rows 25, 26, 32, 35, 44, 45, 47,
   62, and 75, identify the numerator measurand, denominator material, and
   whether the quantity is a process rate or a fractional rate of change.
3. Amount-per-mass rates (4 candidates): For rows 48, 55, 64, and 72, identify
   the substance, process, and denominator entity. `BiogeochemicalRate` is
   plausible only with that evidence.
4. Ratio semantics (2 candidates): For `NanoMOL-PER-MOL` and
   `MilliMOL-PER-MOL-SEC`, identify whether these represent mole fraction,
   amount fraction, stoichiometric ratio, or a normalized process rate.
5. PPM rates (3 candidates): For `PPM-PER-SEC`, `PPM-PER-HR`, and `PPM-PER-MIN`,
   identify whether ppm is mass, amount, volume, or another ratio basis and what
   changes over time.
6. Month convention (4 candidates): `MilliGM-PER-KiloGM-MO`,
  `KiloGM-PER-HA-MO`, `GM-PER-M2-MO`, and `MilliGM-PER-GM-MO` are deferred
  pending confirmation that `MO` means QUDT's synodic month, a Julian month,
  or a calendar month.

## Conversion Notes

- SI prefixes, litre, hour, day, and year arithmetic follow the BIPM SI Brochure
  and current QUDT constituent values.
- Customary-unit arithmetic follows current QUDT factors checked against NIST SP
  811 Appendix B.8. Cord is treated as 128 cubic international feet; acre is
  43,560 square international feet; the US short ton and avoirdupois ounce use
  their exact SI definitions.
- `MI_US-PER-SEC` preserves current QUDT's rounded `MI_US` multiplier
  (`1609.347219 m`) so explicit factor inference remains consistent. The exact
  survey-mile ratio is `6336000/3937 m`, a difference of approximately `3.06e-7
  m`; this draft is marked `conversion_exact=false` in the ledger.
- The four `MO` candidates are `blocked-conversion`, not rejected. QUDT's
  `unit:MO` is a synodic month (`2551442.976 s`, approximately 29.53 days), a
  Julian month is `2629800 s` (30.4375 days), and a calendar month varies from
  28 to 31 days. Because month appears in the denominator, each interpretation
  produces a different SI multiplier. The four candidates remain in
  `decisions.csv` but are omitted from `units.ttl` until the intended convention
  is confirmed. If `MO` intentionally denotes QUDT's existing `unit:MO`, use
  the synodic-month value and generate all four drafts; if it denotes calendar
  aggregation, do not assign a universal seconds-based conversion.
- Repeating rational conversions are represented by high-precision finite
  decimals and retain their exact defining equations in `decisions.csv`.

## Sources

- BIPM SI Brochure: https://www.bipm.org/en/publications/si-brochure/
- NIST SP 811: https://www.nist.gov/pml/special-publication-811
- NIST SP 811 Appendix B.8:
  https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b8
- UCUM specification: https://ucum.org/ucum.html
- QUDT Unit Vocabulary Submission Guidelines:
  https://github.com/qudt/qudt-public-repo/wiki/Unit-Vocabulary-Submission-Guidelines
- QUDT Commensurability guidance:
  https://github.com/qudt/qudt-public-repo/wiki/Commensurability-Composition-Semantics-and-Context
- Local and published QUDT resources cited per row in `decisions.csv`

## Validation

The deterministic artifact checks completed during generation:

- `decisions.csv` contains exactly 84 rows and one row for every intake
  `record_id`.
- Exactly 51 `ready-for-review` resources are emitted in `units.ttl`.
- `quantitykinds.ttl` and `dimensionvectors.ttl` contain no placeholder
  resources.
- All unresolved candidates are omitted from Turtle.

Post-generation validation results:

- The skill RDF preflight passed all three Turtle files with zero warnings.
- Independent `rdflib` parsing found 51 `qudt:Unit` subjects, exactly matching
  the 51 `ready-for-review` ledger rows.
- Exact Decimal arithmetic over current QUDT factor multipliers reproduced all
  50 compound or powered Unit multipliers; `GigaGM` independently matched the
  exact SI prefix result of `1000000 kg`.
- Every reused local factor Unit, reference Unit, QuantityKind, and
  DimensionVector resolved; every Unit/QuantityKind vector pair agreed.
- Live SPARQL revalidation found all 51 draft URIs absent and all reused URIs
  present.
- Exact English-label and symbol searches found zero collisions in either the
  local aggregate vocabulary or live endpoint.
- The external QUDT checkout remained clean after validation.

Final artifact checksums:

```text
89fa3bfa85caf2345e12a6d97f79e96bba629bf878e9c68584681159fd8a02cc  units.ttl
f941f13928c5e3687d8a9acded27406377246765e94fa75c0535fc6b500e2f82  quantitykinds.ttl
ab1cb2b3938412ad03b5402a940018a18c165911990098d2f180e672974f9fc9  dimensionvectors.ttl
ace18bf5128691f1b1293cc72085f8d3887f2a48b4695b05a134575699948d1e  decisions.csv
1361a6d721dcf68eb0dbf134c1ef583c1212d445fc0ac1adcb1d4b5ade1fcf30  intake.csv
de37bf420b655941a30cda7a031ec1f465d5cc014403cc1009d88db575560443  input/mob_units_raw.csv
```

### Validation Limitation

The full QUDT Maven/SHACL build was not run because the user prohibited
modifying or building `/home/srearl/localRepos/qudt-public-repo`. The source
checkout remains read-only. This package must not be described as
submission-ready.

## Well-Architected Trade-off

_Optimizing for **reliability** (immutable input, exact factor equations,
explicit unresolved statuses) and **operational excellence** (one complete
ledger and reproducible validation evidence). Trading draft coverage and review
speed for semantic and conversion conservatism._

## Workflow Feedback

| Field           | Value                                                                                                                                                                                                                        |
|-----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Category        | `input`, `source`, `semantics`                                                                                                                                                                                               |
| Candidate       | `GM-PER-CM3`, `MicroMOL-PER-MOLE`, all `MO`, `NUM`, and normalized-rate rows                                                                                                                                                 |
| Run directory   | `AI_create_ttl/runs/2026-09-07-mob-units-gpt`                                                                                                                                                                                |
| Observation     | Canonical token normalization found two existing resources that exact-qname checks missed; expression-only input also leaves ratio and count QuantityKinds underdetermined. The local checkout was behind official upstream. |
| Evidence        | Current local and published `GM-PER-CentiM3` and `MicroMOL-PER-MOL`; QUDT same-vector QK inventory; QUDT/UCUM month disagreement; local/upstream commit comparison.                                                          |
| Impact          | Prevented two duplicate resources and 28 unsupported drafts, including four silently convention-dependent conversions.                                                                                                       |
| Frequency       | Repeated across spelling, count, ratio-rate, and month families in this 84-row batch.                                                                                                                                        |
| Proposed change | Add deterministic canonical token alias checks (`CM3`/`CentiM3`, `MOLE`/`MOL`) before exact-qname triage and request measurement context in future intake exports.                                                           |
| Disposition     | `needs-evidence` pending review across additional batches; no active skill rule changed in this run.                                                                                                                         |
