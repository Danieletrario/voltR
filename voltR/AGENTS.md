# AGENTS.md - voltR

Status: STRUCTURE_ONLY

This package/domain follows the EIKOS operating workflow:

```text
autonomous package -> DIA -> competence indices -> convergedR orchestrator -> FARO opinion
```

Role: VOLT ranking and comparison among alternatives admissible after FARO.

Rules for agents:

- Do not invent implementation history, approvals, tests, releases, sources, or validation outcomes.
- Preserve package autonomy.
- Record unknowns as PENDING_VERIFICATION, NOT_STARTED, or TO_BE_DEFINED.
- Do not mark checks as passed without command, output, and version evidence.
- Do not silently duplicate cross-domain authority.
