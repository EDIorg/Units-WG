# Recommended Source Catalog

This catalog extends the agent's reusable research sources. Add a source here
when it is likely to help future candidate batches, not merely one unit.
Candidate-specific links, citations, and context belong in the candidate
template's `notes` field.

Catalog entries do not override
[`source-precedence.md`](./source-precedence.md). The agent must still assess
whether each source is authoritative for the specific claim being researched.

## Active Sources

### QUDT

| Source | Location | Authority and allowed use | Limitations | Review |
| --- | --- | --- | --- | --- |
| QUDT source repository | `https://github.com/qudt/qudt-public-repo` | Current schema, vocabulary structure, local branch content, build behavior, and precedents | A checkout may differ from the published release; record commit and branch | Review each run |
| QUDT wiki submission guides | `https://github.com/qudt/qudt-public-repo/wiki` | Current contribution intent, qname, label, property, and submission guidance | Some examples and paths may lag the source repository | Review each run |
| QUDT SPARQL endpoint | `https://www.qudt.org/fuseki/qudt/sparql` | Resources and assertions in the published QUDT dataset | May lag or differ from the configured checkout | Query each run |

### Metrology and Codes

| Source | Location | Authority and allowed use | Limitations | Review |
| --- | --- | --- | --- | --- |
| BIPM SI Brochure and unit pages | `https://www.bipm.org/en/publications/si-brochure/` | Primary SI definitions, prefixes, base and derived units | Does not define every domain or customary unit | Check current edition |
| NIST SI guidance | `https://www.nist.gov/pml/owm/si-units` | Authoritative US SI guidance, definitions, and conversions | Use statutory or jurisdictional sources for non-SI legal definitions | Check page date |
| UCUM specification | `https://ucum.org/ucum` | UCUM syntax and codes | A valid UCUM code does not establish QUDT identity or semantics | Check current release |
| IUPAC Gold Book | `https://goldbook.iupac.org/` | Authoritative chemistry terminology and quantity definitions | Chemistry scope only | Check cited entry/version |

### Ecology and Description Leads

| Source | Location | Authority and allowed use | Limitations | Review |
| --- | --- | --- | --- | --- |
| EDI/LTER unit-registry | `https://github.com/EDIorg/unit-registry` | Unit-description research leads only | Stale, WIP, legacy model, no visible license; independently verify every retained claim; never use for conversion, QK, DV, context, or other values | Commit `b9433fe`; reassess before use |
| Units-WG cross-reference files | `AI_create_ttl/JP_Crossref_files/` | Candidate history and pointers to same-vector QUDT resources | Historical project aids, not current ontology authority | Verify against current QUDT |

## Add a Reusable Source

A user may request an addition in chat, for example:

```text
Evaluate <SOURCE URL OR WORKSPACE PATH> for the QUDT workflow. If suitable,
add it to the recommended source catalog with its allowed uses, limitations,
authority class, and review requirements.
```

Before adding it, the agent must inspect and record:

1. **Publisher and provenance**: who maintains it and where its data originates.
2. **Authority class**: primary standard, authoritative domain source, official
   dataset documentation, maintained secondary source, or historical lead.
3. **Allowed claims**: descriptions, conversions, terminology, codes, context,
   labels, or discovery leads.
4. **Prohibited claims**: uses the source cannot support independently.
5. **Scope**: relevant disciplines, jurisdictions, unit systems, or versions.
6. **Maintenance**: latest update, release model, stable identifiers, and access.
7. **Quality signals**: citations, review process, tests, known errors, and
   conflicts with current authoritative sources.
8. **Licensing**: whether facts may be cited and whether text or data may be
   reused. Prefer synthesis even when reuse is allowed.
9. **Review requirement**: each run, annually, per release, or before use.

Add the source to the appropriate Active Sources table only when its role is
clear and bounded. If evidence is incomplete, document it as a proposed source
in Workflow Feedback with disposition `needs-evidence`; do not add it yet.

## Maintain the Catalog

- Verify catalog sources during periodic workflow review.
- Update version, commit, or access notes when material changes occur.
- Downgrade or remove sources that become stale, inaccessible, or unreliable.
- Record material catalog changes in `AI_WORKLOG.md`.
- Never promote a source because it agrees with generated output or appears
  frequently. Authority and provenance control its role.