test_that("VOLT demo project builds and ranks only FARO-admissible alternatives", {
  demo <- volt_demo_project()
  ranking <- demo$ranking

  expect_s3_class(demo$project, "volt_project")
  expect_s3_class(demo$alternatives, "volt_alternatives")
  expect_s3_class(demo$criteria, "volt_criteria")
  expect_s3_class(demo$performance, "volt_performance")
  expect_s3_class(ranking, "volt_ranking")
  expect_equal(nrow(ranking$admitted), 3)
  expect_equal(nrow(ranking$excluded), 1)
  expect_false("ALT_WIND" %in% ranking$result$alternative_id)
  expect_true(all(c("alternative_id", "score", "rank") %in% names(ranking$result)))
  expect_equal(sum(ranking$criteria$normalized_weight), 1)
})

test_that("VOLT ranking preserves audit and language guard", {
  ranking <- volt_demo_project()$ranking
  pack <- volt_report_pack(ranking)
  s <- summary(ranking)

  expect_true(all(c("check", "status", "message") %in% names(ranking$audit)))
  expect_match(ranking$language_guard, "non sostituisce la decisione")
  expect_true(is.data.frame(pack$overview))
  expect_true(is.data.frame(pack$result))
  expect_true(is.data.frame(pack$excluded))
  expect_true(is.data.frame(pack$audit))
  expect_true(is.data.frame(pack$formulas))
  expect_true(is.data.frame(pack$limitations))
  expect_true(is.data.frame(pack$final_report))
  expect_true(is.data.frame(pack$provenance))
  expect_equal(nrow(pack$final_report), 7)
  expect_true(any(grepl("decisione", pack$limitations$lettura, ignore.case = TRUE)))
  expect_equal(s$alternatives_ranked, 3)
  expect_equal(s$alternatives_excluded, 1)
})

test_that("VOLT validates required inputs", {
  expect_error(volt_alternatives(data.frame(label = "A", faro_status = "ADMISSIBLE")), "alternative_id")
  expect_error(volt_criteria(data.frame(criterion_id = "c", label = "C", direction = "up", weight = 1)), "direction")
  expect_error(volt_performance(data.frame(alternative_id = "a", criterion_id = "c")), "value")
})

test_that("VOLT project update and import helpers work", {
  demo <- volt_demo_project()
  criteria <- demo$criteria
  criteria$weight[criteria$criterion_id == "production"] <- 0.70
  updated <- volt_update_project(demo, criteria = criteria)

  expect_s3_class(updated$ranking, "volt_ranking")
  expect_equal(sum(updated$ranking$criteria$normalized_weight), 1)
  expect_equal(updated$criteria$weight[updated$criteria$criterion_id == "production"], 0.70)

  path <- tempfile(fileext = ".csv")
  utils::write.csv(as.data.frame(demo$alternatives), path, row.names = FALSE)
  imported <- volt_import_file(path)
  applied <- volt_apply_import(demo, imported, "alternatives")
  expect_s3_class(applied$alternatives, "volt_alternatives")
  expect_s3_class(applied$ranking, "volt_ranking")
})

test_that("VOLT projects support standalone and agoRa-linked modes", {
  standalone <- volt_project("P1", "Standalone")
  linked <- volt_project("P2", "Linked", workflow_mode = "agora_linked")
  allowed <- volt_allowed_values()

  expect_equal(standalone$workflow_mode, "standalone")
  expect_equal(linked$workflow_mode, "agora_linked")
  expect_true(all(c("standalone", "agora_linked") %in% allowed$workflow_mode))
})

test_that("VOLT templates and saved projects support base R workflow", {
  allowed <- volt_allowed_values()
  alternatives <- volt_template("alternatives")
  criteria <- volt_template("criteria")
  performance <- volt_template("performance")
  demo <- volt_demo_project()
  path <- tempfile(fileext = ".rds")
  saved <- volt_save_project(demo, path)
  opened <- volt_open_project(saved)
  applied <- volt_apply_import(demo, opened, "auto")

  expect_true(all(c("ADMISSIBLE", "NOT_ADMISSIBLE", "PENDING_VERIFICATION") %in% allowed$faro_status))
  expect_true(all(c("max", "min") %in% allowed$direction))
  expect_true(all(c("alternative_id", "label", "faro_status") %in% names(alternatives)))
  expect_true(all(c("criterion_id", "label", "direction", "weight") %in% names(criteria)))
  expect_true(all(c("alternative_id", "criterion_id", "value") %in% names(performance)))
  expect_true(file.exists(saved))
  expect_s3_class(opened$ranking, "volt_ranking")
  expect_s3_class(applied$ranking, "volt_ranking")
})

test_that("VOLT upload guide explains data purposes", {
  guide <- volt_upload_guide()

  expect_true(all(c("destinazione", "quando_usarla", "colonne_richieste", "scopo", "dopo_il_caricamento") %in% names(guide)))
  expect_equal(nrow(guide), 8)
  expect_true(any(grepl("Alternative", guide$destinazione)))
  expect_true(any(grepl("Criteri", guide$destinazione)))
  expect_true(any(grepl("Prestazioni", guide$destinazione)))
  expect_true(any(grepl("alternative_id", guide$colonne_richieste)))
  expect_true(any(grepl("Ricalcola ranking", guide$dopo_il_caricamento)))
})

test_that("Bundled upload examples are valid and loadable", {
  examples <- volt_upload_examples()
  demo <- volt_demo_project()
  alternatives <- volt_import_file(volt_example_path("esempio_alternative.csv"))
  criteria <- volt_import_file(volt_example_path("esempio_criteri.csv"))
  performance <- volt_import_file(volt_example_path("esempio_prestazioni.csv"))
  project <- volt_apply_import(demo, alternatives, "alternatives")
  project <- volt_apply_import(project, criteria, "criteria")
  project <- volt_apply_import(project, performance, "performance")

  expect_equal(nrow(examples), 6)
  expect_true(all(file.exists(vapply(examples$file, volt_example_path, character(1)))))
  expect_s3_class(project$alternatives, "volt_alternatives")
  expect_s3_class(project$criteria, "volt_criteria")
  expect_s3_class(project$performance, "volt_performance")
  expect_s3_class(project$ranking, "volt_ranking")
  expect_true("ALT_EOLICO" %in% project$ranking$excluded$alternative_id)
})

test_that("VOLT formula reference documents transparent calculation", {
  formulas <- volt_formula_reference()

  expect_true(all(c("passaggio", "formula", "lettura") %in% names(formulas)))
  expect_true(any(grepl("Filtro FARO", formulas$passaggio)))
  expect_true(any(grepl("normalized_value", formulas$formula)))
})

test_that("DIA-aware VOLT demo builds five clusters and atomic indicators", {
  demo <- volt_dia_demo_project()
  ranking <- demo$ranking

  expect_s3_class(demo$clusters, "volt_cluster_plan")
  expect_s3_class(demo$indicators, "volt_indicator_registry")
  expect_s3_class(demo$indicator_performance, "volt_indicator_performance")
  expect_s3_class(ranking, "volt_dia_ranking")
  expect_equal(nrow(demo$clusters), 5)
  expect_equal(nrow(demo$indicators), 50)
  expect_equal(nrow(ranking$admitted), 3)
  expect_equal(nrow(ranking$excluded), 1)
  expect_false("ALT_EOLICO" %in% ranking$result$alternative_id)
})

test_that("DIA-aware VOLT keeps two-level weights explicit", {
  ranking <- volt_dia_demo_project()$ranking
  by_cluster <- stats::aggregate(normalized_indicator_weight ~ cluster_id, ranking$indicators, sum)

  expect_equal(sum(ranking$clusters$normalized_cluster_weight), 1)
  expect_true(all(abs(by_cluster$normalized_indicator_weight - 1) < 1e-12))
  expect_true(all(c("cluster_score", "weighted_cluster_score") %in% names(ranking$cluster_scores)))
})

test_that("DIA-aware VOLT exposes completeness, sensitivity and report", {
  demo <- volt_dia_demo_project()
  ranking <- demo$ranking
  pack <- volt_dia_report_pack(ranking, demo$project)
  s <- summary(ranking)

  expect_true(any(ranking$completeness$missing_values > 0))
  expect_true(all(c("scenario", "cluster_id", "top_alternative", "top_changed") %in% names(ranking$sensitivity)))
  expect_equal(nrow(ranking$sensitivity), nrow(ranking$clusters))
  expect_true(is.data.frame(pack$cluster_scores))
  expect_true(is.data.frame(pack$final_report))
  expect_true(any(grepl("DIA", pack$provenance$valore)))
  expect_match(ranking$language_guard, "non sostituisce la decisione")
  expect_equal(s$clusters, 5)
  expect_equal(s$indicators, 50)
})

test_that("DIA-aware import helpers apply uploaded cluster files", {
  demo <- volt_dia_demo_project()
  clusters <- demo$clusters
  clusters$cluster_weight[clusters$cluster_id == "C1"] <- 2
  updated <- volt_apply_dia_import(demo, clusters, "clusters")

  expect_s3_class(updated$ranking, "volt_dia_ranking")
  expect_equal(updated$clusters$cluster_weight[updated$clusters$cluster_id == "C1"], 2)
  expect_equal(sum(updated$ranking$clusters$normalized_cluster_weight), 1)

  path <- tempfile(fileext = ".rds")
  saved <- volt_save_project(updated, path)
  opened <- volt_open_project(saved)
  expect_s3_class(opened$ranking, "volt_dia_ranking")
})

test_that("agoRa handoff can create DIA-aware and flat VOLT projects", {
  dia_demo <- volt_dia_demo_project()
  dia_handoff <- volt_agora_handoff(
    alternatives = dia_demo$alternatives,
    clusters = dia_demo$clusters,
    indicators = dia_demo$indicators,
    indicator_performance = dia_demo$indicator_performance
  )
  dia_project <- volt_from_agora(dia_handoff)

  flat_demo <- volt_demo_project()
  flat_handoff <- volt_agora_handoff(
    alternatives = flat_demo$alternatives,
    criteria = flat_demo$criteria,
    performance = flat_demo$performance
  )
  flat_project <- volt_from_agora(flat_handoff)

  expect_s3_class(dia_handoff, "volt_agora_handoff")
  expect_s3_class(dia_project$ranking, "volt_dia_ranking")
  expect_s3_class(flat_project$ranking, "volt_ranking")
  expect_equal(dia_project$project$workflow_mode, "agora_linked")
  expect_equal(flat_project$project$workflow_mode, "agora_linked")
})

test_that("DIA templates and examples are valid and loadable", {
  clusters <- volt_template("clusters")
  indicators <- volt_template("indicators")
  performance <- volt_template("indicator_performance")
  examples <- volt_upload_examples()

  expect_true(all(c("cluster_id", "cluster_name", "cluster_weight") %in% names(clusters)))
  expect_true(all(c("cluster_id", "indicator_id", "direction", "indicator_weight") %in% names(indicators)))
  expect_true(all(c("alternative_id", "indicator_id", "value") %in% names(performance)))
  expect_true(any(grepl("DIA", examples$destinazione)))

  cluster_file <- volt_example_path("dia/esempio_cluster_dia.csv")
  indicator_file <- volt_example_path("dia/esempio_indicatori_dia.csv")
  performance_file <- volt_example_path("dia/esempio_prestazioni_indicatori_dia.csv")
  expect_true(file.exists(cluster_file))
  expect_true(file.exists(indicator_file))
  expect_true(file.exists(performance_file))

  demo <- volt_dia_demo_project()
  project <- volt_apply_dia_import(demo, volt_import_file(cluster_file), "clusters")
  project <- volt_apply_dia_import(project, volt_import_file(indicator_file), "indicators")
  project <- volt_apply_dia_import(project, volt_import_file(performance_file), "indicator_performance")
  expect_s3_class(project$ranking, "volt_dia_ranking")
})

test_that("DIA validation rejects unsupported directions and statuses", {
  expect_error(
    volt_indicator_registry(data.frame(
      cluster_id = "C1",
      indicator_id = "I1",
      indicator_name = "Bad",
      direction = "up",
      indicator_weight = 1
    )),
    "direction"
  )
  expect_error(
    volt_indicator_performance(data.frame(
      alternative_id = "ALT",
      indicator_id = "I1",
      value = 1,
      evidence_status = "CERTAIN"
    )),
    "evidence_status"
  )
})

test_that("AHP derives and applies cluster weights without hidden thresholds", {
  demo <- volt_dia_demo_project()
  mat <- volt_ahp_matrix_from_weights(demo$clusters$cluster_weight, demo$clusters$cluster_id)
  ahp <- volt_ahp_weights(mat)
  updated <- volt_apply_ahp_to_clusters(demo, ahp)

  expect_s3_class(ahp, "volt_ahp_weights")
  expect_equal(sum(ahp$weights$weight), 1)
  expect_equal(ahp$diagnostics$reciprocal_gap, 0)
  expect_match(ahp$interpretation, "non applica soglie occulte")
  expect_s3_class(updated$ranking, "volt_dia_ranking")
  expect_equal(sum(updated$clusters$normalized_cluster_weight), 1)
})

test_that("MCDA wrappers preserve source labels", {
  flat <- volt_demo_project()
  dia <- volt_dia_demo_project()
  flat_rank <- volt_mcda(flat$alternatives, flat$criteria, flat$performance, source = "standalone")
  dia_rank <- volt_mcda_dia(dia$alternatives, dia$clusters, dia$indicators, dia$indicator_performance, source = "agora_linked")

  expect_s3_class(flat_rank, "volt_ranking")
  expect_s3_class(dia_rank, "volt_dia_ranking")
  expect_equal(flat_rank$source, "standalone")
  expect_equal(dia_rank$source, "agora_linked")
})

test_that("DIA operational checklist exposes data readiness without scores", {
  ranking <- volt_dia_demo_project()$ranking
  check <- volt_dia_validation_check(ranking)

  expect_true(all(c("controllo", "esito", "lettura") %in% names(check)))
  expect_true(any(check$esito == "DA DICHIARARE"))
  expect_false(any(grepl("score|punteggio", check$controllo, ignore.case = TRUE)))
  expect_true(any(grepl("FARO", check$controllo)))
})

test_that("DIA decision view and narrative report are inspectable", {
  demo <- volt_dia_demo_project()
  view <- volt_dia_decision_view(demo$ranking)
  decision <- volt_decision_log(data.frame(
    decisione = "Si procede con approfondimento istruttorio su ALT_IDRO.",
    motivazione = "Risulta prima nel ranking demo, con cautele sulle fonti.",
    scostamento_dal_ranking = "Nessuno scostamento nel caso demo.",
    condizioni = "Verifica fonti mancanti.",
    tempi = "Entro 60 giorni.",
    responsabili = "Gruppo tecnico.",
    feedback_previsto = "Restituzione pubblica degli esiti.",
    stringsAsFactors = FALSE
  ))
  report <- volt_dia_narrative_report(demo$ranking, demo$project, decision)

  expect_s3_class(decision, "volt_decision_log")
  expect_true(all(c("domanda", "risposta") %in% names(view)))
  expect_true(any(grepl("non sostituisce", view$risposta, ignore.case = TRUE)))
  expect_equal(nrow(report), 9)
  expect_true(any(grepl("Restituzione pubblica", report$testo)))
})

test_that("Complete Valtaro-Valceno demo exposes participation, MCDA and reports", {
  demo <- volt_valtaro_valceno_demo_project()
  demo_005 <- volt_valtaro_valceno_demo_project(sample_error = 0.05)

  expect_s3_class(demo, "volt_valtaro_valceno_demo_project")
  expect_s3_class(demo$project, "volt_project")
  expect_s3_class(demo$ranking, "volt_dia_ranking")
  expect_equal(nrow(demo$meetings), 10)
  expect_equal(nrow(demo$facilities), 6)
  expect_true(is.data.frame(demo$final_facility_ranking))
  expect_equal(nrow(demo$final_facility_ranking), 6)
  expect_equal(demo$final_facility_ranking$alternative_id[1], demo$ranking$result$alternative_id[1])
  expect_true(all(c("posizione", "impianto", "punteggio_finale", "punto_di_forza", "criticita_principale", "raccomandazione") %in% names(demo$final_facility_ranking)))
  expect_true(is.data.frame(demo$event_rankings))
  expect_true(is.data.frame(demo$progressive_rankings))
  expect_true(is.data.frame(demo$final_progressive_ranking))
  expect_equal(length(unique(demo$event_rankings$meeting_id)), 10)
  expect_equal(length(unique(demo$progressive_rankings$meeting_id)), 10)
  expect_true(all(demo$final_progressive_ranking$stato == "definitiva"))
  expect_true(is.data.frame(demo$agora_connection))
  expect_true(is.data.frame(demo$municipality_population))
  expect_true(is.data.frame(demo$municipality_sample_check))
  expect_true(all(c("Borgo Val di Taro", "Bardi") %in% demo$municipality_population$comune))
  expect_true(all(c("campione_ideale", "campione_osservato", "scostamento") %in% names(demo$municipality_sample_check)))
  expect_equal(nrow(demo$clusters), 5)
  expect_equal(nrow(demo$indicators), 15)
  expect_equal(nrow(demo$ranking$result), 6)
  expect_equal(demo$ranking$result$alternative_id[1], "IDRO_BARDI_SORBA")
  expect_true(all(c("in presenza", "da remoto", "questionario") %in% demo$participants$modalita_partecipazione))
  expect_true(all(c("cittadino", "stakeholder") %in% demo$participants$ruolo))
  expect_true(any(grepl("Sorba", demo$facilities$localizzazione)))
  expect_true(any(grepl("sintetici", demo$assumptions$assunzione)))
  expect_true(any(grepl("Popolazione demo", demo$report_sections$testo)))
  expect_true(demo$sample_formula$valore[demo$sample_formula$parametro == "n_target"] > 0)
  expect_gt(
    demo_005$sample_formula$valore[demo_005$sample_formula$parametro == "n_target"],
    demo$sample_formula$valore[demo$sample_formula$parametro == "n_target"]
  )
  expect_error(volt_valtaro_valceno_demo_project(sample_error = 0), "sample_error")
})

test_that("VOLT decision log validates required fields", {
  expect_s3_class(volt_decision_log(), "volt_decision_log")
  expect_error(
    volt_decision_log(data.frame(decisione = "x")),
    "motivazione"
  )
})
