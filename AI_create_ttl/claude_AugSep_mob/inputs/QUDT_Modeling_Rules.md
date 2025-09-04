
# ✅ QUDT Modeling Rules for Units, QuantityKinds, and DimensionVectors

## 🧭 General Principles

| Area | Rule |
|------|------|
| **Strict Uniqueness** | **Only create new entries when necessary.** Before defining a new `DimensionVector` or `QuantityKind`, you **must search QUDT** to confirm it does **not already exist**. |
| **No Redundancy** | Do **not recreate** existing `DimensionVectors`, `QuantityKinds`, or `Units`. |
| **Follow Flowchart and ERD** | The **flowchart** determines whether to create or reuse entities; the **ERD** governs relationships and required fields. |

---

## 🧮 Dimension Vector Rules

| Topic | Rule |
|-------|------|
| **Dimension Vector Labels ** | `rdfs:label` for `DimensionVectors` should be the **exact vector string** (e.g., `"A0E0L3I0M0H0T-1D0"`). |
| **Notation** | Use 7 base SI exponents: `A`, `E`, `L`, `I`, `M`, `H`, `T`, plus `D` (dimensionless indicator). |
| **Dimensionless** | Use `D1` **only if the entire vector is dimensionless**. Never use it to indicate just one component is dimensionless. If all other components are dimensionless, then use D1.|
| **Prefixes and Roles** | `A` (amount of substance) is **only for moles**. Do **not** use it for **"number of things"** — use dimensionless instead. |
| **Reusability** | Reuse exact vector strings already defined in QUDT. Only create a new `qkdv:` resource if a **new numeric combination** of exponents is required. |

---

## 📏 Quantity Kind Rules

| Topic | Rule |
|-------|------|
| **Naming** | Names must reflect the **physical quantity**, not the unit (e.g., `MassPerAreaTime`, not `kg/ha/day`). |
| **Semantic Accuracy** | Avoid misclassifying quantities. E.g., if a vector has `L` and `T^-2` both in the **denominator**, it’s **not** `Acceleration`. |
| **Existing QKs** | Check existing QUDT quantity kinds and only define a new one if the DV is also new. |
| **Cardinality with DV** | A dimension vector may map to **multiple** `QuantityKind`, per ERD. |


<!-- comment 
| Original rules, which mob removed/edited | Rule |
|-------|------|
| **One-to-One with DV** | Each new `DimensionVector` must map to a **unique** `QuantityKind`. |
| **No Redundant Names** | Do not create multiple quantity kinds for the **same dimension vector** with different names. |
-->

---

## 📐 Unit Rules

| Topic | Rule |
|-------|------|
| **Unit Construction** | Compose units logically using SI/metric components (e.g., `MicroMOL-PER-M-SEC2` = µmol/m/s²). |
| **Must Link to QK + DV** | Every `Unit` must have exactly one `qudt:hasQuantityKind` and one `qudt:hasDimensionVector`. |
| **No Orphans** | Do not create units that don’t point to both a `QuantityKind` and a `DimensionVector`. |
| **Only Define New Units If Necessary** | If an equivalent unit exists in QUDT with the same structure and links, **reuse** it. |
| **Number of Entities** | Units may be dimensionless but must **not reuse A1** for simple counts. Use `D1` if truly dimensionless. |

---

## 📊 File Structure

- Keep separate `.ttl` files for:
  1. `units.ttl`
  2. `quantitykinds.ttl`
  3. `dimensionvectors.ttl`
- Each file must include all properties defined in the ERD:
  - `rdfs:label`, `rdfs:description`, `qudt:hasDimensionVector`, `qudt:plainTextDescription`, `rdfs:isDefinedBy`, etc.
  - for Dimension Vectors, express rdfs:isDefinedBy like this: "rdfs:isDefinedBy <http://qudt.org/$$QUDT_VERSION$$/vocab/unit>"
 

---

## ✅ Validation Checklist

1. 🔎 Search QUDT for existing **Units**, **QuantityKinds**, and **DimensionVectors**.
2. ✅ Only define new `DimensionVectors` when the exponent pattern is **not already defined**.
3. ✅ Only define new `QuantityKinds` for genuinely **new dimension vectors**.
4. ✅ Only define new `Units` if they are not already modeled in QUDT.
6. ⚠️ All `Units` must have exactly **one** QK and **one** DV.
7. ✅ Use proper SI dimensions (especially for **Amount of Substance** vs **Dimensionless Count**).
