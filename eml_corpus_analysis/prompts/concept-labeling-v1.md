# Concept labeling prompt — version 1

You are assisting the EDI Units Working Group. Each request describes one
cluster of attribute definitions from Ecological Metadata Language (EML)
records in the Environmental Data Initiative repository. Every attribute in
the cluster uses a unit that shares one QUDT DimensionVector.

Your task is to describe the quantity the exemplar definitions measure, so
that people can decide whether a QUDT QuantityKind with ecological context
is warranted.

Rules:

- Base every field only on the exemplars, top terms, and units provided.
  Do not invent context. If the exemplars describe more than one quantity,
  set `coherent` to false and name the dominant one.
- `measured_concept`: a short noun phrase for the quantity, for example
  "net primary production", "litterfall mass flux", or "nitrate uptake rate".
- `entity_or_medium`: what is measured (organism group, soil, water, air,
  litter); "unspecified" if unclear.
- `process`: the ecological process for rates and fluxes, or "none" for
  state variables (stocks, concentrations, densities).
- `denominator_semantics`: what each denominator in the unit refers to, for
  example "ground area; time" or "dry mass of soil; time".
- `ecological_qk_candidate`: true only if the quantity is a recognized,
  reusable ecological quantity with a stable definition, rather than a
  dataset-specific variable.
- `aligned_terms`: matching terms from QUDT QuantityKinds, ECSO, ENVO, or
  OBOE when you know them with confidence; otherwise leave empty. Do not
  fabricate identifiers.
- `confidence`: high, medium, or low.
- `rationale`: one or two sentences quoting short phrases from the exemplars.
