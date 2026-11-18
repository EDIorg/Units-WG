# Findings snapshot (2026-11-18)

A summary of the corpus analysis. The live version, recomputed on every
run, is the report's **Findings** page (`report/findings.qmd`). The shortlist
is in [`data/reference/qk_shortlist.csv`](../data/reference/qk_shortlist.csv),
and its evidence is in `data/derived/qk_shortlist.csv`.

## Key findings

- **Corpus:** 10,634 EML files (snapshot 2026-11-12) from 36 scopes, with
  0 parse errors.
- **Units map well to QUDT:** 91.4% of unit uses map to a current QUDT unit.
  2,285 of 3,187 unit groups appear in only one dataset.
- **Units do not imply ecological meaning:** 62 of 154 frequent units serve
  two or more concepts. QKs must be attached using concept evidence, not
  dimension alone.
- **Annotations are rare:** only 1,479 datasets carry semantic annotations.
- **MOB candidates:** 79 of the 84 candidates from the 2026-09-07 run occur
  in the corpus.
- **Automated candidates:**
  - 41 concept × vector pairs are candidates for a new QK;
  - 3 match an existing QK;
  - 17 are dimensionless.

  Many of the new-QK candidates differ only by substance.
- **QUDT precedent:** `BiogeochemicalRate` and
  `MassSpecificBiogeochemicalRate` are already generic over substance.

## Recommended strategy

1. Reuse existing QUDT QKs where the meaning fits, and propose additive unit
   links only.
2. Extend the biogeochemical-rate family with generic per-area members.
3. Add named QKs only for standard ecological indices (LAI, SLA, cover).
4. Express substance, medium, and organism through annotations, not QK names.
5. Attach units only when the corpus provides evidence (at least 3
   datasets).

## QuantityKinds to consider

Dataset counts are taken from the 2026-11-18 run.

| Tier | Proposal | QUDT QK (reuse or parent) | Datasets |
|---|---|---|---|
| 1 Reuse | Stream discharge | `VolumeFlowRate` | 297 |
| 1 Reuse | PAR | `PhotosyntheticPhotonFluxDensity` | 358 |
| 1 Reuse | Volumetric soil water content | `VolumeFraction` | 501 |
| 1 Reuse | Enzyme activity | `BiogeochemicalRate` | 41 |
| 1 Reuse | Mass-specific N transformation rates | `MassSpecificBiogeochemicalRate` | 59 |
| 1 Reuse | Organisms per volume | `VolumetricEntityDensity` | 67 |
| 1 Reuse | Organism counts | `Count` | 611 |
| 2 New, generic | Areic biogeochemical rate (mass) | `MassPerAreaTime` | 147 |
| 2 New, generic | Areic biogeochemical rate (molar) | `MolarFluxDensity` | 331 |
| 2 New, generic | Organisms per area | `InverseArea` | 71 |
| 3 New, named | Leaf area index | `AreaRatio` | 77 |
| 3 New, named | Specific leaf area | `SpecificSurfaceArea` | 47 |
| 3 New, named | Cover fraction | `AreaRatio` | 623 |
| 4 Defer | Net primary production rate | `MassPerAreaTime` | 55 |
| 4 Defer | Eddy-covariance fluxes (GPP, ER, NEE) | `MolarFluxDensity` | 79 |
| 4 Defer | Nutrient uptake length | `Length` | 31 |
| 4 Defer | Nutrient uptake velocity | `LinearVelocity` | 34 |
| 4 Defer | Soil bulk density | `MassDensity` | 184 |
| 5 Annotate | Substance concentrations (DO, chlorophyll *a*) | `MassConcentration` | 692 |
| 5 Annotate | Areal stocks and biomass | `AreicMass` | 409 |
| 5 Annotate | Individual mass | `Mass` | 704 |
| 5 Annotate | Soil organic carbon fraction | `MassFraction` | 105 |

## Decisions needed

1. Adopt the provisional thresholds (20 datasets and 3 scopes).
2. Accept criterion 8: a QK names a quantity type, not a substance.
3. Choose between new areal rate QKs and reusing the generic parents.
4. Decide whether named process QKs (NPP, GPP, NEE) are wanted.
5. Assign reviewers for the concept-match audit.
6. Decide whether to enable LLM labeling.
7. Set when to consult the QUDT community, ideally before drafting Turtle.
