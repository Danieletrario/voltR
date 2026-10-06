# Esempi di importazione per voltR

Questa cartella contiene file CSV dimostrativi gia compilati per mostrare come
devono presentarsi i dati importabili con le funzioni base di `voltR`.

## File disponibili

- `esempio_alternative.csv`: elenco delle alternative e stato FARO.
- `esempio_criteri.csv`: criteri, direzione e peso.
- `esempio_prestazioni.csv`: valori delle alternative rispetto ai criteri.

## Uso da R

```r
library(voltR)

demo <- volt_demo_project()
alternatives <- volt_import_file(volt_example_path("esempio_alternative.csv"))
criteria <- volt_import_file(volt_example_path("esempio_criteri.csv"))
performance <- volt_import_file(volt_example_path("esempio_prestazioni.csv"))

project <- volt_apply_import(demo, alternatives, "alternatives")
project <- volt_apply_import(project, criteria, "criteria")
project <- volt_apply_import(project, performance, "performance")
project$ranking$result
```

## Nota metodologica

I dati sono sintetici e servono solo a mostrare la forma corretta dei file.
Nel lavoro reale ogni valore della tabella prestazioni deve avere una fonte
documentata.
