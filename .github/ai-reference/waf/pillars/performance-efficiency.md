# Pillar: Performance Efficiency (research-software context)

Right-sized compute and data handling, without premature optimization.

## Checklist
- [ ] Appropriate data structures; vectorized over row-wise where it matters.
- [ ] Avoid needless recomputation (cache/memoize; `targets` for R).
- [ ] Stream or chunk large datasets instead of loading everything.
- [ ] Profile before optimizing; optimize the measured bottleneck.

## Trade-off prompts
- Is this fast enough for the real data volume, or just the sample?
- Are we adding complexity for performance we haven't measured a need for?
