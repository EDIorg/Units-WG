# Source Precedence and Evidence

Use sources in this order when they address the same question:

1. **Current checked-out QUDT source** for file paths, schema constraints,
   literal style, build behavior, and branch content.
2. **Current official QUDT wiki** for contribution semantics, naming, labeling,
   and submission procedures.
3. **Published QUDT SPARQL endpoint** for resources available in the live
   release.
4. **Authoritative metrology and jurisdictional standards** for definitions,
   multipliers, offsets, and codes.
5. **Units-WG source data and cross-references** for candidate provenance and
  ecological meaning.
6. **EDI/LTER unit-registry** only as a secondary lead for Unit-description
  research. Independently verify every retained claim. Do not use it for any
  other generated value or ontology decision.
7. **Historical Units-WG AI artifacts** only for lessons, candidate evidence,
  and known failure modes.

The local checkout and published endpoint can legitimately differ. Record both
results, the local branch and commit, and the query timestamp. Do not silently
replace one with the other.

Useful official sources include:

- QUDT source: `https://github.com/qudt/qudt-public-repo`
- Unit submission guidelines:
  `https://github.com/qudt/qudt-public-repo/wiki/Unit-Vocabulary-Submission-Guidelines`
- Commensurability guidance:
  `https://github.com/qudt/qudt-public-repo/wiki/Commensurability-Composition-Semantics-and-Context`
- Published SPARQL endpoint: `https://www.qudt.org/fuseki/qudt/sparql`
- EDI/LTER unit-registry: `https://github.com/EDIorg/unit-registry`

Do not treat dimensional equality, similar labels, generated historical RDF,
or model memory as proof of semantic identity.

See [`research-and-derivation.md`](./research-and-derivation.md) for source
roles, unit-registry limitations, and required evidence records.
