# NEWS - voltR

## Unreleased

- Status: OPEN_SOURCE_CORE.
- The commercial dashboard and internal manuals have been separated from the
  public `voltR` package.
- `voltR` now contains only the open source R core: S3 objects, ranking
  functions, AHP helpers, DIA-aware MCDA, report packs, imports, exports and
  synthetic demos.

# voltR 0.0.0.9000

* Added first operational VOLT ranking layer for FARO-admissible alternatives.
* Added S3 objects and helpers for projects, alternatives, criteria, performance matrices, ranking, and report packs.
* Added `volt_demo_project()` synthetic demo.
* Added bundled compiled upload examples for alternatives, criteria, and performances.
* Added DIA-aware VOLT workflow with five clusters, atomic indicators, two-level weights, completeness checks, cluster contributions, sensitivity table, and final DIA/VOLT report pack.
* Added DIA upload templates and bundled examples for clusters, indicators, and indicator performances.
* Added standalone-first, agoRa-compatible workflow mode, agoRa handoff helpers, explicit MCDA wrappers, and AHP cluster-weight support.
* Added complete synthetic Valtaro-Valceno participation and MCDA demo.
* Added agoRa-style demography and sampling objects for the Valtaro-Valceno demo, including municipality and meeting-site views.
* Added explicit sampling-error parameter for the Valtaro-Valceno demo, supporting 10% and 5% use cases.
* Added final facility ranking data for decision/report reading.
* Added event-level partial rankings and cumulative progressive rankings for multi-meeting participation paths.
* Added initial testthat coverage.
