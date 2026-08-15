# Current QUDT Modeling Checklist

Reconfirm these rules against the current QUDT source and wiki during every
run. This summary intentionally corrects assumptions found in earlier Units-WG
experiments.

## Reuse and Identity

- Search local and published QUDT by qname, label, symbol, aliases, spelling
  variants, dimension, and semantics before creating a resource.
- Reuse only when identity and meaning match, not merely dimensionality.
- Preserve valid factor expressions as distinct Unit identities when the exact
  qname is absent. Algebraic equality to a simpler unit is evidence, not an
  automatic duplicate decision.
- Use `skos:altLabel` for alternate names of one unit. Distinct established
  synonymous units may require separate resources linked by `qudt:exactMatch`.

## Qnames and Labels

- Prefer an unambiguous common symbol in uppercase for the qname.
- Separate concept qualifiers with underscores and order them as dimensional,
  contextual, system, jurisdictional, then other qualifiers.
- Write supported prefixes in TitleCase without a separator.
- Put nonnegative exponents directly after the unit.
- Use `-` for multiplication and one `-PER-` between numerator and denominator.
- Apply qualifiers, prefixes, exponents, and hyphens in that precedence order.
- Current QUDT guidance prefers singular international-English Title Case
  labels with `@en` and adds `@en-us` when US spelling differs. Keep “per”
  lowercase. Follow current target-vocabulary precedents; use or omit language
  tags intentionally when another representation is appropriate.

## Dimension Vectors

- Use the QUDT vector order `A`, `E`, `L`, `I`, `M`, `H`, `T`, `D`.
- `A` is amount of substance, not a count of entities.
- Use the current QUDT dimensionless/angle convention only when supported by
  the relevant QuantityKind and current precedent; do not append `D1` merely
  because one component is dimensionless.
- Reuse an exact existing vector. Draft a new vector only for a genuinely new
  exponent combination.
- A new vector uses the dimension-vector graph in `rdfs:isDefinedBy`, never the
  unit graph.

## QuantityKinds and Commensurability

- Select QuantityKinds from physical meaning and context.
- Multiple QuantityKinds may share a DimensionVector.
- A new QuantityKind may use an existing DimensionVector.
- Dimensional equality is necessary but does not establish semantic
  interchangeability.
- Treat `qudt:specializationOf`, `qudt:organizedUnder`, and `qudt:exactMatch`
  according to current QUDT commensurability guidance.
- Do not invent a QK URI to avoid a clarification or review step.

A normal new QuantityKind contribution includes:

- `a qudt:QuantityKind`
- `qudt:hasDimensionVector`
- `qudt:plainTextDescription`; current submission guidance prefers no language
  tag, but justified language tags are allowed when appropriate
- `rdfs:isDefinedBy <http://qudt.org/$$QUDT_VERSION$$/vocab/quantitykind>`
- `rdfs:label` following current QUDT label conventions
- `qudt:specializationOf` or `qudt:organizedUnder` when grouped under another
  QuantityKind

Current aggregate entries may also use `dcterms:description`; including it does
not replace the submission guide's `qudt:plainTextDescription` requirement.

## Unit Properties

A normal contribution includes:

- `a qudt:Unit`
- `dcterms:description`; current submission guidance prefers no language tag,
  but justified language tags are allowed when appropriate
- `qudt:conversionMultiplier` as `xsd:decimal`
- `qudt:conversionMultiplierSN` as `xsd:double`
- `qudt:hasDimensionVector`
- one or more dimensionally compatible QuantityKind relations, preferably
  `qudt:unitForQuantityKind` for commensurate assignments or
  `qudt:categorizedByQuantityKind` for organizational categories;
  `qudt:hasQuantityKind` is the legacy super-property
- `rdfs:isDefinedBy <http://qudt.org/$$QUDT_VERSION$$/vocab/unit>`
- at least one ASCII `rdfs:label`; current submission guidance prefers a
  language tag

Compound and powered units also include:

- `qudt:expression`
- explicit `qudt:hasFactorUnit` nodes whose units and integer exponents match
  the qname; `a qudt:FactorUnit` is valid but not required by QUDT's inference
  output

Use `qudt:conversionOffset` only when the conversion requires an offset. Bare
Turtle values such as `0.001` and `1.0E-3` already denote `xsd:decimal` and
`xsd:double`; do not quote numeric literals.

`qudt:plainTextDescription`, `qudt:symbol`, applicable systems, standard codes,
and authoritative references are valuable when supported, but distinguish
project quality preferences from current QUDT schema requirements.

Templates use the current common QUDT tagging convention by default. Preflight
warns about other choices rather than rejecting them. Record intentional
language-tag deviations and supporting precedents in the decisions ledger.

Follow [`research-and-derivation.md`](./research-and-derivation.md) for exact
conversion arithmetic, factor-vector composition, semantic hypothesis testing,
and evidence-backed description synthesis.

## Historical Lessons Retained

- Count-like expressions need explicit semantic review and do not imply `A1`.
- Generated QK names must describe the quantity, not the unit spelling.
- Conversion arithmetic and QuantityKind selection require evidence.
- Placeholder QKs, unresolved TODOs, stale version URIs, and orphan resources
  are not acceptable in review-ready Turtle.
