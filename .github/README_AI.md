# QUDT AI Workflow

The active cross-platform workflow is documented in
[AI_create_ttl/QUDT_AI_WORKFLOW.md](../AI_create_ttl/QUDT_AI_WORKFLOW.md).
Its **Submit a File** section includes attachment steps and a generic request
that works with either Copilot or Codex.

Active customization files are:

- Shared Copilot and Codex skill:
  [`.agents/skills/qudt-unit-contribution/SKILL.md`](../.agents/skills/qudt-unit-contribution/SKILL.md)
- GitHub Copilot agent:
  [`.github/agents/qudt-ttl-curator.agent.md`](agents/qudt-ttl-curator.agent.md)
- GitHub Copilot prompt:
  [`.github/prompts/process-qudt-unit-list.prompt.md`](prompts/process-qudt-unit-list.prompt.md)

The shared skill is the sole authority for the QUDT batch procedure. The agent
and prompt are thin Copilot adapters. Codex invokes the shared skill directly.

Files under `archive/` are historical and non-authoritative. Do not use them in
current work unless a user explicitly requests a retrospective.
