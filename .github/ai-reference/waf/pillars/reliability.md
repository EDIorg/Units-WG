# Pillar: Reliability (research-software context)

Optimize for results that are reproducible and recoverable.

## Checklist
- [ ] Runs end-to-end from a clean checkout (documented entrypoint).
- [ ] Dependencies pinned (suggest `renv` lockfile for R; use Astral `uv.lock`
	for Python).
- [ ] Randomness seeded; results deterministic where claimed.
- [ ] Raw data treated as immutable; derived outputs regenerable.
- [ ] Failures are explicit (clear errors), not silent.
- [ ] Long pipelines are resumable / idempotent (e.g. `targets`, caching).
- [ ] Validation at data boundaries (schema/units/missing-value checks).

## Trade-off prompts
- What breaks reproducibility if a collaborator runs this next year?
- Where could a partial failure leave inconsistent outputs?
- Are we trading reliability for speed anywhere? Is that recorded?
