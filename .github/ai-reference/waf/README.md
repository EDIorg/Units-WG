# Well-Architected Framework — local reference

Version-controlled reference material for design decisions. The agent reads
these files on demand (native workspace file access) and cites the relevant
pillar inline in code comments or commit messages. Material AI-assisted work
is summarized in `AI_WORKLOG.md`; user-facing release history belongs in a root
`CHANGELOG.md` when the repository has one.

## Why a local folder instead of an MCP filesystem server?

`.github/ai-reference/waf/` is inside the workspace, so the agent already has
read access via its native file tools. A filesystem MCP server here would add
process overhead without adding capability. Use an MCP server only to reach
docs **outside** the workspace or a live external source (e.g. `context7`,
`microsoft-learn`).

## Layout

```
.github/ai-reference/waf/
├── README.md          # this file
└── pillars/
    ├── reliability.md
    ├── security.md
    ├── operational-excellence.md
    ├── performance-efficiency.md
    └── cost-sustainability.md
```

## How to use

When making a non-trivial design choice, open the relevant pillar checklist(s),
score your options, and note the trade-off in a brief inline comment or commit
message. No separate spec file required.

## Sources to ground these checklists (verify before asserting)

- AWS Well-Architected Framework — https://aws.amazon.com/architecture/well-architected/
- Azure Well-Architected Framework — https://learn.microsoft.com/azure/well-architected/
- Google Cloud Architecture Framework — https://cloud.google.com/architecture/framework

Adapt the cloud-centric guidance to a **research-software** context (R/Python,
reproducibility, data packages). The pillar files below are tailored that way.
