# Generic QUDT Unit-List Request

Attach the candidate file to the chat or replace `INPUT_FILE` with its path in
the workspace. Replace optional bracketed values when known.

```text
Use the qudt-unit-contribution skill to process INPUT_FILE.

Run name: [RUN_NAME or derive it from the input filename]
QUDT source root: [QUDT_SOURCE_ROOT or resolve it from the workspace/environment]

Apply the documented source precedence and input contract. Research and propose
all values not supplied by the user, including measurement context, qualifier
meaning, conversions, labels, QuantityKinds, and DimensionVectors. Ask one
consolidated set of clarification questions only for material ambiguities that
remain after research, and continue processing unblocked candidates.

Create a new run-specific output directory containing units.ttl,
quantitykinds.ttl, dimensionvectors.ttl, decisions.csv, and review.md. Reuse
existing QUDT resources, flag proposed QuantityKinds and DimensionVectors for
human review, run all available validation, and summarize output paths,
candidate statuses, concerns, recommendations, and limitations.
```
