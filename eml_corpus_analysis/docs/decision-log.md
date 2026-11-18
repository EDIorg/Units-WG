# Decision log

Decisions about thresholds, modeling, and criteria, newest first. Each entry
states the decision, its rationale, and its status. "Proposed" means waiting
for working-group review.

## 2026-11-18: Curated QK shortlist and biogeochemical-rate family

- **Decision:**
  - Consolidate the automated candidates into a 22-row shortlist
    (`data/reference/qk_shortlist.csv`) in five tiers: reuse, new generic,
    new named, defer, and annotate.
  - Extend QUDT's existing ecological family (`BiogeochemicalRate`,
    `MassSpecificBiogeochemicalRate`) with areal members on a mass basis
    (parent `MassPerAreaTime`) and a molar basis (parent `MolarFluxDensity`),
    instead of one QK per process or substance.
  - Correct the automated parent suggestions where QUDT has a closer QK:
    `SpecificSurfaceArea` (SLA), `AreaRatio` (LAI, cover), `VolumeFraction`
    (soil moisture), `VolumetricEntityDensity`, `InverseArea`, `AreicMass`,
    `MassConcentration`, `MassFraction`, and `MassDensity`.
  - Prefer `MassPerAreaTime` over `MassFluxDensity` as the areal-rate parent,
    because QUDT defines `MassFluxDensity` as transport (velocity × density).
- **Rationale:** QUDT already defines biogeochemical rates generically over
  substance, so per-area members follow precedent and satisfy criterion 8.
  The pipeline checks every `qudt_qk` against current QUDT.
- **Status:** Proposed. Tiers and working names are for team review;
  `Count` for organism counts may be better as a population QK.

## 2026-11-18: Ancillary-column noise in concept matches

- **Decision:** Add exclude patterns to five seeds (soil moisture, organism
  respiration, PAR, percent cover, chlorophyll *a*). Report `vector_share` in
  `qk_candidates.csv` as a review aid, not as a filter.
- **Rationale:** The full-corpus run showed seeds matching co-located
  columns: probe temperature in °C for soil moisture, sensor depth in m for
  PAR, fluorometer voltage for chlorophyll, and transect distance for cover.
  These pairs had low shares (about 0.04–0.13), but so did some legitimate
  pairs, such as organism abundance per m² (0.089). A share cut-off would
  drop real evidence.
- **Effect:** `candidate-new-qk` pairs fell from 46 to 41.
- **Status:** Adopted. Seed precision still needs the audit.

## 2026-11-18: Criterion 8, quantity type rather than substance

- **Decision:** Add a manual criterion. A QK names a kind of quantity, not
  the substance, organism, or medium. A substance-specific QK needs a
  comparable QUDT precedent. Otherwise, the context goes in annotations
  (ECSO, ChEBI, ENVO) on a generic QK.
- **Rationale:** QUDT has some substance-specific QKs
  (`MassConcentrationOfWater`, `BloodGlucoseLevel_Mass`), but most are
  generic. Many automated candidates (dissolved oxygen, chlorophyll *a*,
  CO₂ and methane flux, soil organic carbon) differ only in substance.
- **Status:** Proposed.

## 2026-11-18: Parent QK suggestions are advisory

- **Decision:** Keep the automated parent suggestion, but require a person to
  choose the parent.
- **Rationale:** The suggestion follows QUDT's current unit-to-QK
  attachments, so it inherits QUDT's choices. Molar fluxes get
  `PhotosyntheticPhotonFluxDensity` instead of `MolarFluxDensity`, and mass
  concentrations get `Density` instead of `MassConcentration`.
- **Status:** Adopted.

## 2026-11-18: Modeling recommendation, Option 3 (hybrid)

- **Decision:** Recommend a hybrid approach.
  - Propose QUDT QKs (as `qudt:specializationOf` a same-vector parent) only
    for concepts that pass all admission criteria.
  - Carry other ecological context through external vocabularies on generic
    QKs.
- **Options compared:**
  1. Ecological QKs throughout QUDT.
  2. Generic QKs plus annotations only.
- **Rationale:**
  - The corpus shows heavy polysemy. Common units such as g/m² and g/m²/d
    serve many concepts, so one unit cannot imply one ecological QK.
  - A small set of concepts (for example, net primary production, specific
    leaf area, and nitrogen mineralization rates) recur across many datasets
    and scopes with stable definitions.
  - Only about 14% of datasets carry semantic annotations, so annotations
    alone do not give units ecological meaning today.
- **Status:** Proposed. Consult the QUDT community before bulk proposals.

## 2026-11-18: Attachment rules

- **Decision:**
  - Use `qudt:unitForQuantityKind` only when the unit is commensurate with
    the QK and the corpus uses it for the concept in at least
    `attach_min_datasets` (3) datasets.
  - Use `qudt:categorizedByQuantityKind` only for organizational grouping.
  - Never attach on dimensional equality alone.
  - Changes to existing QUDT units are additive only.
- **Status:** Proposed.

## 2026-11-18: Provisional thresholds

- **Decision:** A concept × DimensionVector pair meets recurrence at 20 or
  more datasets across 3 or more scopes. Attachment needs 3 or more datasets.
- **Rationale:** In the corpus, 2,285 of 3,187 unit groups appear in only
  one dataset, and 55 appear in 100 or more. Twenty datasets across three
  scopes excludes single-site conventions but keeps cross-site concepts.
  Three scopes stops one LTER site's many packages from qualifying alone.
- **Status:** Provisional. Compare the candidate list at other values (for
  example 10/2 and 40/5) before adopting.

## 2026-11-18: LLM labeling off by default

- **Decision:** Ship deterministic TF-IDF clustering. LLM labeling is opt-in
  ([llm-protocol.md](llm-protocol.md)).
- **Rationale:** Labeling sends text to a third party and costs money. The
  deterministic outputs are enough for the first team review.
- **Status:** Adopted. Enabling it needs working-group approval.

## 2026-11-18: Unit grouping and mapping

- **Decision:**
  - `unit_group` is the verified QUDT unit when one exists, otherwise the
    spelling-insensitive match key.
  - RCode_JP mappings are used only when their QUDT URI still exists.
- **Rationale:** This groups variant spellings without trusting stale
  mappings. 91.4% of unit uses map to current QUDT units.
- **Status:** Adopted.
