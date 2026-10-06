# DECISION - voltR

Status: STRUCTURE_ONLY

Package/domain role:

```text
VOLT ranking and comparison among alternatives admissible after FARO.
```

Architecture workflow:

```text
autonomous package -> DIA -> competence indices -> convergedR orchestrator -> FARO opinion
```

## Current Decisions

No package-local historical decision is asserted here unless supported by existing evidence.

Recorded operating decision:

- `EIKOS-DEC-VERT-DIA-ORCH-001`: autonomous packages update DIA; DIA updates competence indices; convergedR orchestrates; FARO emits the non-compensatory traffic-light opinion.
