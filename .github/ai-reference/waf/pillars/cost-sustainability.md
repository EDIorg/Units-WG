# Pillar: Cost / Sustainability (research-software context)

Mindful compute and storage footprint.

## Checklist
- [ ] Batch vs. interactive: use the cheaper mode where latency isn't needed.
- [ ] Cache intermediate results to avoid re-running expensive steps.
- [ ] Storage footprint reasonable; avoid committing large data to git.
- [ ] Prefer incremental pipelines that only recompute what changed.

## Trade-off prompts
- What does this cost to run repeatedly over a full season of data?
- Is there a smaller/cheaper approach that still meets success criteria?
