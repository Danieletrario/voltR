# ARCHITECTURE - voltR

Status: STRUCTURE_ONLY

Package/domain role:

```text
VOLT ranking and comparison among alternatives admissible after FARO.
```

Architecture workflow:

```text
autonomous package -> DIA -> competence indices -> convergedR orchestrator -> FARO opinion
```

## Boundary

This package/domain should remain autonomous and exchange evidence through explicit handoff contracts.

It must not become an implicit substitute for DIA, convergedR, FARO, or another vertical package.
