# Pillar: Security (research-software context)

Least privilege, clean provenance, no leaked secrets or sensitive data.

## Checklist
- [ ] No credentials, tokens, or PII committed (use env vars / `keyring`).
- [ ] Dependencies from trusted sources; provenance/versions pinned.
- [ ] Inputs validated at system boundaries (OWASP Top 10 where web-facing).
- [ ] Sensitive ecological data (e.g. protected-species locations) handled per
      EDI guidance — generalize, don't expose.
- [ ] MCP servers and tools restricted to least privilege (read-only where
      possible).

## Trade-off prompts
- What is the blast radius if a token here leaks?
- Does any convenience (broad tool access) expand privilege unnecessarily?
