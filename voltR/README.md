# voltR

Status: OPEN_SOURCE_CORE

`voltR` is the open source R core for transparent ranking and comparison of
alternatives declared admissible after FARO.

It provides inspectable R functions for:

- defining VOLT projects, alternatives, criteria and performance matrices;
- ranking FARO-admissible alternatives with weighted-sum MCDA;
- using DIA-style clusters and atomic indicators;
- deriving AHP weights from pairwise comparison matrices;
- preserving agoRa-compatible handoff metadata;
- building report tables, audit tables, limitations and provenance;
- running synthetic demos, including the Valtaro-Valceno participation and
  energy-planning example.

`voltR` does not replace DIA, convergedR, FARO, agoRa or domain-specific
packages. It does not turn a ranking into an automatic decision.

The commercial Eikos dashboard is not part of this open source package.

## Basic Use

```r
install.packages("devtools")
devtools::install_github("Danieletrario/voltR")
library(voltR)

demo <- volt_demo_project()
summary(demo$ranking)
volt_report_pack(demo$ranking, demo$project)$final_report
```

## DIA-Aware Workflow

```r
library(voltR)

demo <- volt_dia_demo_project()
summary(demo$ranking)
volt_dia_completeness(demo$ranking)
volt_dia_report_pack(demo$ranking, demo$project)$final_report
```

## Complete Valtaro-Valceno Demo

`voltR` includes a complete synthetic participation and decision demo for the
Valtaro-Valceno energy path:

- 10 information and co-decision meetings from November 2026 to March 2027;
- five 30 kWp photovoltaic proposals and one 30 kW micro-hydro proposal;
- synthetic territorial population frame, sample formula and sample check;
- anonymous citizen and stakeholder participants across in-person, remote and
  questionnaire participation modes;
- briefing, questionnaire, pre/post opinion movement, DIA/VOLT indicators,
  MCDA ranking, final facility ranking, motivated decision and final report
  sections;
- partial rankings for each meeting and cumulative progressive rankings that
  become definitive after the last event.

```r
demo <- volt_valtaro_valceno_demo_project(sample_error = 0.05)

demo$ranking$result
demo$final_facility_ranking
demo$event_rankings
demo$progressive_rankings
demo$final_progressive_ranking
demo$report_sections
```

## Examples

Bundled CSV examples are available in:

```text
inst/examples/upload/
```

Use:

```r
volt_upload_examples()
volt_example_path("esempio_alternative.csv")
```

## Public/Private Boundary

This repository directory is intended to contain only the open source R core.
The Eikos dashboard, Shiny application, commercial manuals and logo assets are
kept outside the open source package.

## License and Intellectual Property

The intellectual property holder for `voltR` is **Eikòs Analytics S.r.l.
Società Benefit**.

The R source code is released under **GPL-3**.

Documentation, methodological descriptions, examples and synthetic demo
materials are released under **Creative Commons Attribution 4.0 International
(CC BY 4.0)**, unless otherwise stated.
