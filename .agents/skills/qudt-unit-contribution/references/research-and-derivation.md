# Research and Derivation Protocol

Use this protocol for every candidate before generating RDF. Its purpose is to
make conversion, dimensional, semantic, and descriptive decisions reproducible
from cited evidence rather than model memory.

## Evidence Classes

### QUDT Authority

Use the resolved current QUDT checkout, official QUDT wiki, and published QUDT
SPARQL endpoint according to [`source-precedence.md`](./source-precedence.md).

- The checkout controls current schema, vocabulary structure, graph names,
  literal style, and build behavior.
- The wiki controls current contribution intent and naming guidance.
- The endpoint establishes what is present in the published release.
- Record disagreements rather than silently choosing one source.

For example, older DimensionVector wiki examples may differ from the current
checkout's classes and graph URI. Follow the checkout for generated structure
and record the discrepancy in `review.md`.

### Unit Identity and Equivalent Expressions

Unit identity includes the submitted factorization and expression, not only the
reduced dimension and multiplier. QUDT intentionally retains algebraically
equal Units when their expressions or semantic families differ.

For each candidate:

1. Search for the exact qname and same modeled expression.
2. Separately search for numerically or algebraically equivalent Units.
3. Reuse only an exact expression match or a resource for which the candidate is
   merely an alternate label or spelling.
4. If the exact qname is absent, draft the valid candidate expression and record
   equivalent Units as related evidence, not as replacements.
5. Do not add `qudt:exactMatch` or `qudt:scalingOf` solely from algebraic
   equality; follow current QUDT precedent and semantics.

Examples in current QUDT include both `M-PER-M2` and `PER-M`, and both
`J-PER-M2` and `N-PER-M`.

### Primary Definition and Conversion Sources

Prefer the most specific applicable primary source:

1. BIPM SI Brochure and official BIPM publications
2. NIST publications, including SP 811 and current unit guidance
3. ISO, IEC, OIML, or other applicable standards
4. Statutory or jurisdictional definitions for customary units
5. Authoritative domain bodies, such as IUPAC for chemical quantities
6. Official UCUM specification and other maintained code-system definitions

Use secondary references only to locate primary definitions or corroborate a
claim. Do not use an uncited conversion website as sole evidence.

### Domain and Usage Sources

Use these to establish how a unit expression is used and what is measured:

1. Dataset metadata, variable definitions, methods, and protocols
2. Maintained domain vocabularies, standards, and scientific reference works
3. Units-WG cross-references and documented curation decisions
4. The EDI/LTER `unit-registry`, only as a lead for description research and
   within the limitations below
5. Peer-reviewed literature and other traceable secondary sources

Usage frequency does not establish meaning. A unit string may be polysemous;
the measured variable or method is stronger evidence than the unit spelling.

## EDI/LTER unit-registry Assessment

Repository: `https://github.com/EDIorg/unit-registry`

The repository is useful only as a secondary lead for researching Unit
descriptions. It contains material inherited from LTER, including historical
description text that may identify concepts or references worth checking in
current authoritative sources.

Its permitted use in this workflow is limited to:

- Locate historical description ideas or terminology for a candidate Unit
- Identify claims in those descriptions that should be checked against current
   QUDT, standards, dataset documentation, or authoritative domain sources
- Corroborate that synthesized description wording addresses ecology-specific
   usage, without treating the registry as proof of that usage

Do not use it to generate or decide:

- Qnames, labels, symbols, aliases, codes, or deprecation relations
- Whether a Unit exists or should exist in current QUDT
- Conversion multipliers, offsets, reference units, or exactness
- QuantityKind identity, hierarchy, relation, or description
- DimensionVectors or factor-unit composition
- Measurement context or semantic identity
- Any RDF property other than source-backed Unit description content

Never copy registry descriptions verbatim. Treat each statement as a research
lead, verify it independently, and synthesize original Unit description wording.

Reasons for these limits:

- Latest repository activity is from June 2022; the EML export is from 2020.
- The EDI analysis is explicitly marked work in progress.
- Export notes document a manually removed duplicate.
- Annotated custom-unit data identifies spelling and logic errors.
- Project notes describe unresolved confusion between `unitType`, quantity, and
  dimensional grouping, and warn that some modeled quantities are wrong.
- The model predates current QUDT commensurability relations and conventions.
- No license file is visible in the repository. Treat text reuse cautiously;
  cite facts and synthesize original descriptions instead of copying prose.

When using this repository, record the repository URL, commit hash, file, row or
key, and data cutoff in `description_sources`. Record the independent sources
that verify each retained claim. If no independent support is found, omit the
claim from the Unit description.

## Conversion Derivation

For each new unit:

1. Parse the expression into numerator and denominator factors, prefixes,
   powers, qualifiers, and any affine component.
2. Resolve each factor to a current QUDT unit or an authoritative definition.
3. Identify the reference unit URI to which the QUDT multiplier converts.
4. Record each factor's exact multiplier and source.
5. Apply prefixes and integer powers using exact decimal or rational arithmetic.
6. Multiply numerator factors and divide denominator factors.
7. Handle offsets only when the authoritative definition and QUDT transform
   model support them. Do not compose affine units algebraically without
   verifying the resulting transform.
8. Emit numerically equivalent `xsd:decimal` and `xsd:double` representations.
9. Compare the result with analogous current QUDT units and explain any
   difference.

Preserve the submitted factor expression in `qudt:expression` and explicit
`qudt:hasFactorUnit` nodes. Do not collapse factors merely because their powers
can be simplified.

Record the reference unit, equation, factor table, arithmetic, exactness, value,
offset, and citations. If the conversion depends on an analyte, valence, time
horizon, method, or other external parameter, block a universal unit conversion
and document the required context.

## DimensionVector Derivation

1. Resolve every constituent unit to its verified DimensionVector.
2. Multiply each vector's exponents by the constituent's power.
3. Add numerator exponents and subtract denominator exponents.
4. Write the result in `A E L I M H T D` order with every component present.
5. Verify whether the exact vector exists locally and in published QUDT.
6. Cross-check the result against every selected QuantityKind.

Do not infer `A1` from a count. Do not use `D1` as a generic marker for ratios.
Record the factor vectors and exponent arithmetic in `dv_rationale`.

## QuantityKind Research

1. Develop one or more semantic hypotheses from the expression and available
   domain evidence.
2. Search current QUDT by label, definition, aliases, hierarchy, and vector.
3. Compare same-vector QuantityKinds; dimensional equality alone is not a
   semantic match.
4. Determine whether the unit is genuinely a unit for the kind or merely an
   organizational category.
5. For new data, prefer `qudt:unitForQuantityKind` for a commensurate assignment
   and `qudt:categorizedByQuantityKind` for non-commensurate categorization.
   Treat `qudt:hasQuantityKind` as the legacy super-property.
6. If proposing a new QuantityKind, document why existing kinds fail, choose
   `qudt:specializationOf` or `qudt:organizedUnder` deliberately, and flag the
   proposal for domain review.

Record hypotheses considered, evidence for and against each, the selected
relation, rationale, confidence, and unresolved context.

## Description Research and Synthesis

Descriptions are evidence-backed summaries, not expansions of the qname.

For a Unit `dcterms:description`, establish where supported:

- What the unit measures or expresses
- Its composition and reference unit
- Conversion, offset, exactness, and defining conditions
- Meaning of contextual, system, or jurisdictional qualifiers
- Scope limitations and common aliases

For a QuantityKind `qudt:plainTextDescription`, establish:

- The measurable phenomenon or property
- Operational or mathematical definition
- Distinctions from same-dimension QuantityKinds
- Relevant entity, medium, process, denominator, or context boundaries
- Its dimensional character and semantic parent relation

Use current QUDT definitions and nearby precedents first, then primary domain
sources, dataset documentation, and traceable scientific references. The
`unit-registry` may provide leads for Unit descriptions only; it does not inform
QuantityKind descriptions. Synthesize original wording and cite the underlying
claims in the decision ledger. Do not invent ecological use cases from the unit
expression or copy third-party prose.

`qudt:plainTextDescription` must remain plain text without LaTeX or HTML. A rich
`dcterms:description` may include markup when current QUDT style supports it.
Language-tag choices follow current target-vocabulary precedent and are noted
when they depart from the common QUDT submission convention.

If composition and conversion are established but measurement meaning remains
ambiguous, draft no QuantityKind description. Keep the candidate in
`needs-clarification` and present the researched hypotheses to the user.

## Evidence Quality Gate

Before marking a candidate `ready-for-review`, confirm:

- Conversion values and exactness are supported by primary definitions.
- Decimal and scientific representations are numerically equivalent.
- DimensionVector arithmetic is recorded and agrees with selected QKs.
- QK relation and commensurability rationale are explicit.
- Description claims are traceable to sources.
- Every Unit-description claim suggested by `unit-registry` has independent
   support; the registry supplied no conversion, QK, DV, or other RDF value.
- Remaining uncertainty is recorded in `issues` and `review.md`.
