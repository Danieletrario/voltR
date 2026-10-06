.volt_now <- function() Sys.time()

#' @importFrom graphics barplot par plot.new text
#' @importFrom utils read.csv
NULL

.volt_new <- function(x, class) {
  class(x) <- c(class, class(x))
  x
}

.volt_required <- function(data, cols, name) {
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop(name, " must contain: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

.volt_as_df <- function(x, name) {
  if (!is.data.frame(x)) stop(name, " must be a data.frame.", call. = FALSE)
  as.data.frame(x, stringsAsFactors = FALSE)
}

.volt_first <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[1])) default else as.character(x[1])
}

#' Create a VOLT project
#'
#' @param project_id Project identifier.
#' @param title Project title.
#' @param description Plain-language project description.
#' @param faro_status FARO status of the upstream handoff.
#' @param provenance Optional provenance list.
#' @param workflow_mode Workflow mode: `standalone` or `agora_linked`.
#' @return An object of class `volt_project`.
#' @export
volt_project <- function(project_id, title, description = "", faro_status = "FARO_ADMISSIBLE_ONLY",
                         provenance = list(), workflow_mode = c("standalone", "agora_linked")) {
  workflow_mode <- match.arg(workflow_mode)
  if (missing(project_id) || !nzchar(project_id)) stop("project_id is required.", call. = FALSE)
  if (missing(title) || !nzchar(title)) stop("title is required.", call. = FALSE)
  .volt_new(list(
    project_id = as.character(project_id),
    title = as.character(title),
    description = as.character(description),
    faro_status = as.character(faro_status),
    workflow_mode = workflow_mode,
    provenance = provenance,
    created_at = .volt_now()
  ), "volt_project")
}

#' @export
print.volt_project <- function(x, ...) {
  cat("<volt_project> ", x$project_id, "\n", sep = "")
  cat("  title: ", x$title, "\n", sep = "")
  cat("  FARO: ", x$faro_status, "\n", sep = "")
  cat("  mode: ", .volt_first(x$workflow_mode, "standalone"), "\n", sep = "")
  invisible(x)
}

#' Create a VOLT alternative table
#'
#' @param data Data frame with `alternative_id`, `label`, and `faro_status`.
#' @return A `volt_alternatives` data frame.
#' @export
volt_alternatives <- function(data) {
  data <- .volt_as_df(data, "data")
  .volt_required(data, c("alternative_id", "label", "faro_status"), "data")
  if (anyDuplicated(data$alternative_id)) stop("alternative_id values must be unique.", call. = FALSE)
  if (!"notes" %in% names(data)) data$notes <- ""
  .volt_new(data, c("volt_alternatives", "data.frame"))
}

#' Create a VOLT criteria table
#'
#' @param data Data frame with `criterion_id`, `label`, `direction`, and `weight`.
#' @return A `volt_criteria` data frame.
#' @export
volt_criteria <- function(data) {
  data <- .volt_as_df(data, "data")
  .volt_required(data, c("criterion_id", "label", "direction", "weight"), "data")
  if (anyDuplicated(data$criterion_id)) stop("criterion_id values must be unique.", call. = FALSE)
  bad <- setdiff(unique(data$direction), c("max", "min"))
  if (length(bad)) stop("direction must be 'max' or 'min'.", call. = FALSE)
  data$weight <- suppressWarnings(as.numeric(data$weight))
  if (any(is.na(data$weight)) || any(data$weight < 0)) stop("weights must be non-negative numbers.", call. = FALSE)
  if (sum(data$weight) <= 0) stop("at least one weight must be positive.", call. = FALSE)
  if (!"unit" %in% names(data)) data$unit <- ""
  if (!"description" %in% names(data)) data$description <- ""
  .volt_new(data, c("volt_criteria", "data.frame"))
}

#' Create a VOLT performance matrix
#'
#' @param data Data frame with `alternative_id`, `criterion_id`, and `value`.
#' @return A `volt_performance` data frame.
#' @export
volt_performance <- function(data) {
  data <- .volt_as_df(data, "data")
  .volt_required(data, c("alternative_id", "criterion_id", "value"), "data")
  data$value <- suppressWarnings(as.numeric(data$value))
  if (!"source" %in% names(data)) data$source <- ""
  .volt_new(data, c("volt_performance", "data.frame"))
}

#' Allowed values used by VOLT workflows
#'
#' @return Named list of allowed categorical values.
#' @export
volt_allowed_values <- function() {
  list(
    faro_status = c("ADMISSIBLE", "NOT_ADMISSIBLE", "PENDING_VERIFICATION"),
    workflow_mode = c("standalone", "agora_linked"),
    direction = c("max", "min"),
    indicator_validation_status = c("VALIDATED", "PENDING_VERIFICATION", "MISSING_SOURCE"),
    evidence_status = c("VALIDATED", "PENDING_VERIFICATION", "MISSING", "SYNTHETIC")
  )
}

#' Create an agoRa-to-VOLT handoff object
#'
#' This object lets `voltR` receive project material from agoRa without taking a
#' package dependency on agoRa. It records provenance and preserves the
#' distinction between standalone and agoRa-linked workflows.
#'
#' @param alternatives Alternative table.
#' @param clusters Optional DIA/VOLT cluster table.
#' @param indicators Optional DIA/VOLT indicator registry.
#' @param indicator_performance Optional DIA/VOLT indicator performance table.
#' @param criteria Optional flat criteria table.
#' @param performance Optional flat performance table.
#' @param project Optional `volt_project`.
#' @param metadata Optional metadata list.
#' @return A `volt_agora_handoff` list.
#' @export
volt_agora_handoff <- function(alternatives,
                               clusters = NULL,
                               indicators = NULL,
                               indicator_performance = NULL,
                               criteria = NULL,
                               performance = NULL,
                               project = NULL,
                               metadata = list()) {
  alternatives <- volt_alternatives(alternatives)
  if (!is.null(project) && !inherits(project, "volt_project")) {
    stop("project must be a volt_project.", call. = FALSE)
  }
  if (is.null(project)) {
    project <- volt_project(
      "VOLT-AGORA-HANDOFF",
      "Valutazione importata da agoRa",
      "Oggetto di handoff compatibile con agoRa.",
      provenance = list(source = "agoRa handoff"),
      workflow_mode = "agora_linked"
    )
  }
  if (!is.null(clusters)) clusters <- volt_cluster_plan(clusters)
  if (!is.null(indicators)) indicators <- volt_indicator_registry(indicators)
  if (!is.null(indicator_performance)) indicator_performance <- volt_indicator_performance(indicator_performance)
  if (!is.null(criteria)) criteria <- volt_criteria(criteria)
  if (!is.null(performance)) performance <- volt_performance(performance)
  .volt_new(list(
    project = project,
    alternatives = alternatives,
    clusters = clusters,
    indicators = indicators,
    indicator_performance = indicator_performance,
    criteria = criteria,
    performance = performance,
    metadata = metadata,
    created_at = .volt_now()
  ), "volt_agora_handoff")
}

#' Create a VOLT project bundle from an agoRa handoff
#'
#' @param handoff A `volt_agora_handoff`.
#' @return A flat or DIA-aware VOLT project bundle.
#' @export
volt_from_agora <- function(handoff) {
  if (!inherits(handoff, "volt_agora_handoff")) {
    stop("handoff must be a volt_agora_handoff.", call. = FALSE)
  }
  handoff$project$workflow_mode <- "agora_linked"
  if (!is.null(handoff$clusters) && !is.null(handoff$indicators) && !is.null(handoff$indicator_performance)) {
    out <- list(
      project = handoff$project,
      alternatives = handoff$alternatives,
      clusters = handoff$clusters,
      indicators = handoff$indicators,
      indicator_performance = handoff$indicator_performance
    )
    return(volt_update_dia_project(out))
  }
  if (!is.null(handoff$criteria) && !is.null(handoff$performance)) {
    out <- list(
      project = handoff$project,
      alternatives = handoff$alternatives,
      criteria = handoff$criteria,
      performance = handoff$performance
    )
    return(volt_update_project(out))
  }
  stop("handoff must contain either DIA objects or flat criteria/performance.", call. = FALSE)
}

#' Run flat MCDA ranking
#'
#' @param alternatives Alternatives table.
#' @param criteria Criteria table.
#' @param performance Performance table.
#' @param method Method. Currently `WSM`.
#' @param source Data source label.
#' @return A `volt_ranking`.
#' @export
volt_mcda <- function(alternatives, criteria, performance, method = "WSM",
                      source = c("standalone", "agora_linked")) {
  source <- match.arg(source)
  ranking <- volt_rank(alternatives, criteria, performance, method = method)
  ranking$source <- source
  ranking
}

#' Run DIA-aware MCDA ranking
#'
#' @param alternatives Alternatives table.
#' @param clusters Cluster table.
#' @param indicators Indicator registry.
#' @param performance Indicator performance table.
#' @param method Method. Currently `DIA_WSM`.
#' @param source Data source label.
#' @return A `volt_dia_ranking`.
#' @export
volt_mcda_dia <- function(alternatives, clusters, indicators, performance,
                          method = "DIA_WSM", source = c("standalone", "agora_linked")) {
  source <- match.arg(source)
  ranking <- volt_rank_dia(alternatives, clusters, indicators, performance, method = method)
  ranking$source <- source
  ranking
}

#' Derive AHP weights from a pairwise comparison matrix
#'
#' @param matrix Square positive reciprocal matrix.
#' @param labels Optional labels for rows/columns.
#' @return A `volt_ahp_weights` object with weights and consistency diagnostics.
#' @export
volt_ahp_weights <- function(matrix, labels = NULL) {
  matrix <- as.matrix(matrix)
  if (nrow(matrix) != ncol(matrix)) stop("matrix must be square.", call. = FALSE)
  if (any(is.na(matrix)) || any(matrix <= 0)) stop("matrix must contain positive numeric values.", call. = FALSE)
  if (is.null(labels)) labels <- rownames(matrix)
  if (is.null(labels)) labels <- paste0("item_", seq_len(nrow(matrix)))
  if (length(labels) != nrow(matrix)) stop("labels length must match matrix size.", call. = FALSE)
  reciprocal_gap <- max(abs(matrix * t(matrix) - 1))
  ev <- eigen(matrix)
  idx <- which.max(Re(ev$values))
  lambda_max <- Re(ev$values[idx])
  weights <- abs(Re(ev$vectors[, idx]))
  weights <- weights / sum(weights)
  n <- nrow(matrix)
  ri <- c("1" = 0, "2" = 0, "3" = 0.58, "4" = 0.90, "5" = 1.12,
    "6" = 1.24, "7" = 1.32, "8" = 1.41, "9" = 1.45, "10" = 1.49)
  consistency_index <- if (n <= 1) 0 else (lambda_max - n) / (n - 1)
  random_index <- if (as.character(n) %in% names(ri)) unname(ri[as.character(n)]) else NA_real_
  consistency_ratio <- if (!is.na(random_index) && random_index > 0) consistency_index / random_index else NA_real_
  out <- list(
    matrix = matrix,
    weights = data.frame(item = labels, weight = weights, stringsAsFactors = FALSE),
    diagnostics = data.frame(
      n = n,
      lambda_max = lambda_max,
      consistency_index = consistency_index,
      random_index = random_index,
      consistency_ratio = consistency_ratio,
      reciprocal_gap = reciprocal_gap,
      stringsAsFactors = FALSE
    ),
    interpretation = "AHP produce pesi dichiarati e diagnostiche di coerenza; VOLT non applica soglie occulte."
  )
  .volt_new(out, "volt_ahp_weights")
}

#' Build an AHP pairwise matrix from a weight vector
#'
#' @param weights Numeric positive weights.
#' @param labels Optional item labels.
#' @return Reciprocal pairwise comparison matrix.
#' @export
volt_ahp_matrix_from_weights <- function(weights, labels = NULL) {
  weights <- as.numeric(weights)
  if (any(is.na(weights)) || any(weights <= 0)) stop("weights must be positive numbers.", call. = FALSE)
  if (is.null(labels)) labels <- paste0("item_", seq_along(weights))
  if (length(labels) != length(weights)) stop("labels length must match weights length.", call. = FALSE)
  matrix <- outer(weights, weights, "/")
  dimnames(matrix) <- list(labels, labels)
  matrix
}

#' Apply AHP-derived weights to DIA clusters
#'
#' @param project DIA-aware VOLT project bundle.
#' @param ahp A `volt_ahp_weights` object.
#' @return Updated DIA-aware project bundle.
#' @export
volt_apply_ahp_to_clusters <- function(project, ahp) {
  if (!inherits(ahp, "volt_ahp_weights")) stop("ahp must be a volt_ahp_weights object.", call. = FALSE)
  if (!is.list(project) || !"clusters" %in% names(project)) {
    stop("project must be a DIA-aware VOLT project bundle.", call. = FALSE)
  }
  clusters <- as.data.frame(project$clusters, stringsAsFactors = FALSE)
  matched <- match(clusters$cluster_id, ahp$weights$item)
  if (any(is.na(matched))) matched <- match(clusters$cluster_name, ahp$weights$item)
  if (any(is.na(matched))) stop("AHP items must match cluster_id or cluster_name.", call. = FALSE)
  clusters$cluster_weight <- ahp$weights$weight[matched]
  volt_update_dia_project(project, clusters = clusters)
}

.volt_normalize <- function(values, direction) {
  values <- as.numeric(values)
  out <- rep(NA_real_, length(values))
  ok <- !is.na(values)
  if (!any(ok)) return(out)
  rng <- range(values[ok])
  if (identical(rng[1], rng[2])) {
    out[ok] <- 1
    return(out)
  }
  if (identical(direction, "max")) out[ok] <- (values[ok] - rng[1]) / (rng[2] - rng[1])
  if (identical(direction, "min")) out[ok] <- (rng[2] - values[ok]) / (rng[2] - rng[1])
  out
}

#' Rank FARO-admissible alternatives with VOLT
#'
#' `volt_rank()` applies a transparent weighted-sum ranking only to alternatives
#' whose `faro_status` is `ADMISSIBLE`. Non-admissible alternatives are retained
#' in the audit table but excluded from ranking.
#'
#' @param alternatives A `volt_alternatives` data frame.
#' @param criteria A `volt_criteria` data frame.
#' @param performance A `volt_performance` data frame.
#' @param method Ranking method. Currently `WSM`.
#' @return A `volt_ranking` list.
#' @export
volt_rank <- function(alternatives, criteria, performance, method = "WSM") {
  alternatives <- volt_alternatives(alternatives)
  criteria <- volt_criteria(criteria)
  performance <- volt_performance(performance)
  if (!identical(method, "WSM")) stop("Only WSM is implemented in voltR v0.0.0.9000.", call. = FALSE)

  weights <- criteria$weight / sum(criteria$weight)
  criteria$normalized_weight <- weights
  admitted <- alternatives[alternatives$faro_status == "ADMISSIBLE", , drop = FALSE]
  excluded <- alternatives[alternatives$faro_status != "ADMISSIBLE", , drop = FALSE]

  if (nrow(admitted)) {
    grid <- merge(
      admitted[c("alternative_id", "label")],
      criteria[c("criterion_id", "label", "direction", "normalized_weight")],
      by = NULL
    )
    names(grid)[names(grid) == "label.x"] <- "alternative_label"
    names(grid)[names(grid) == "label.y"] <- "criterion_label"
    grid <- merge(grid, performance, by = c("alternative_id", "criterion_id"), all.x = TRUE)
    grid$normalized_value <- NA_real_
    for (criterion_id in unique(grid$criterion_id)) {
      idx <- grid$criterion_id == criterion_id
      direction <- unique(grid$direction[idx])[1]
      grid$normalized_value[idx] <- .volt_normalize(grid$value[idx], direction)
    }
    grid$weighted_contribution <- grid$normalized_value * grid$normalized_weight
    grid$contribution_for_score <- ifelse(is.na(grid$weighted_contribution), 0, grid$weighted_contribution)
    scores <- stats::aggregate(contribution_for_score ~ alternative_id + alternative_label, grid, sum)
    names(scores)[names(scores) == "contribution_for_score"] <- "score"
    scores <- scores[order(-scores$score, scores$alternative_id), , drop = FALSE]
    scores$rank <- seq_len(nrow(scores))
    row.names(scores) <- NULL
  } else {
    grid <- data.frame(
      alternative_id = character(),
      criterion_id = character(),
      alternative_label = character(),
      criterion_label = character(),
      direction = character(),
      normalized_weight = numeric(),
      value = numeric(),
      source = character(),
      normalized_value = numeric(),
      weighted_contribution = numeric(),
      contribution_for_score = numeric(),
      stringsAsFactors = FALSE
    )
    scores <- data.frame(
      alternative_id = character(),
      alternative_label = character(),
      score = numeric(),
      rank = integer(),
      stringsAsFactors = FALSE
    )
  }

  audit <- data.frame(
    check = c("Filtro FARO", "Valori prestazione mancanti", "Normalizzazione pesi"),
    status = c(
      if (nrow(admitted)) "PASS" else "FAIL",
      if (any(is.na(grid$value))) "WARN" else "PASS",
      "PASS"
    ),
    message = c(
      paste(nrow(admitted), "admissible alternative(s);", nrow(excluded), "excluded."),
      paste(sum(is.na(grid$value)), "missing performance value(s) among admissible alternatives."),
      paste("Weights normalized to sum", signif(sum(criteria$normalized_weight), 6))
    ),
    stringsAsFactors = FALSE
  )

  .volt_new(list(
    method = method,
    alternatives = alternatives,
    criteria = criteria,
    performance = performance,
    admitted = admitted,
    excluded = excluded,
    contributions = grid,
    result = scores,
    audit = audit,
    generated_at = .volt_now(),
    language_guard = "VOLT ordina solo alternative FARO-ammissibili; il ranking supporta il confronto e non sostituisce la decisione."
  ), "volt_ranking")
}

#' Create a VOLT cluster plan from DIA handoff
#'
#' @param data Data frame with `cluster_id`, `cluster_name`, and
#'   `cluster_weight`.
#' @return A `volt_cluster_plan` data frame.
#' @export
volt_cluster_plan <- function(data) {
  data <- .volt_as_df(data, "data")
  .volt_required(data, c("cluster_id", "cluster_name", "cluster_weight"), "data")
  if (anyDuplicated(data$cluster_id)) stop("cluster_id values must be unique.", call. = FALSE)
  data$cluster_weight <- suppressWarnings(as.numeric(data$cluster_weight))
  if (any(is.na(data$cluster_weight)) || any(data$cluster_weight < 0)) {
    stop("cluster_weight must be non-negative numbers.", call. = FALSE)
  }
  if (sum(data$cluster_weight) <= 0) stop("at least one cluster_weight must be positive.", call. = FALSE)
  if (!"description" %in% names(data)) data$description <- ""
  data$normalized_cluster_weight <- data$cluster_weight / sum(data$cluster_weight)
  .volt_new(data, c("volt_cluster_plan", "data.frame"))
}

#' Create a VOLT indicator registry from DIA atomic indicators
#'
#' @param data Data frame with `cluster_id`, `indicator_id`,
#'   `indicator_name`, `direction`, and `indicator_weight`.
#' @return A `volt_indicator_registry` data frame.
#' @export
volt_indicator_registry <- function(data) {
  data <- .volt_as_df(data, "data")
  .volt_required(data, c("cluster_id", "indicator_id", "indicator_name", "direction", "indicator_weight"), "data")
  if (anyDuplicated(data$indicator_id)) stop("indicator_id values must be unique.", call. = FALSE)
  bad_direction <- setdiff(unique(data$direction), c("max", "min"))
  if (length(bad_direction)) stop("direction must be 'max' or 'min'.", call. = FALSE)
  data$indicator_weight <- suppressWarnings(as.numeric(data$indicator_weight))
  if (any(is.na(data$indicator_weight)) || any(data$indicator_weight < 0)) {
    stop("indicator_weight must be non-negative numbers.", call. = FALSE)
  }
  if (!"unit" %in% names(data)) data$unit <- ""
  if (!"source" %in% names(data)) data$source <- ""
  if (!"validation_status" %in% names(data)) data$validation_status <- "PENDING_VERIFICATION"
  if (!"description" %in% names(data)) data$description <- ""
  bad_status <- setdiff(unique(data$validation_status), volt_allowed_values()$indicator_validation_status)
  if (length(bad_status)) stop("validation_status contains unsupported values.", call. = FALSE)
  .volt_new(data, c("volt_indicator_registry", "data.frame"))
}

#' Create a VOLT atomic indicator performance table
#'
#' @param data Data frame with `alternative_id`, `indicator_id`, and `value`.
#' @return A `volt_indicator_performance` data frame.
#' @export
volt_indicator_performance <- function(data) {
  data <- .volt_as_df(data, "data")
  .volt_required(data, c("alternative_id", "indicator_id", "value"), "data")
  data$value <- suppressWarnings(as.numeric(data$value))
  if (!"source" %in% names(data)) data$source <- ""
  if (!"evidence_status" %in% names(data)) {
    data$evidence_status <- ifelse(is.na(data$value), "MISSING", "PENDING_VERIFICATION")
  }
  bad_status <- setdiff(unique(data$evidence_status), volt_allowed_values()$evidence_status)
  if (length(bad_status)) stop("evidence_status contains unsupported values.", call. = FALSE)
  .volt_new(data, c("volt_indicator_performance", "data.frame"))
}

.volt_indicator_weights <- function(clusters, indicators) {
  indicators <- merge(
    indicators,
    clusters[c("cluster_id", "cluster_name", "normalized_cluster_weight")],
    by = "cluster_id",
    all.x = TRUE
  )
  if (any(is.na(indicators$normalized_cluster_weight))) {
    stop("Every indicator cluster_id must exist in clusters.", call. = FALSE)
  }
  indicators$normalized_indicator_weight <- NA_real_
  for (cluster_id in unique(indicators$cluster_id)) {
    idx <- indicators$cluster_id == cluster_id
    total <- sum(indicators$indicator_weight[idx])
    if (total <= 0) stop("Each cluster must have at least one positive indicator_weight.", call. = FALSE)
    indicators$normalized_indicator_weight[idx] <- indicators$indicator_weight[idx] / total
  }
  indicators
}

.volt_rank_from_cluster_scores <- function(cluster_scores, cluster_weights) {
  weighted <- merge(cluster_scores, cluster_weights, by = "cluster_id", all.x = TRUE)
  weighted$weighted_cluster_score <- weighted$cluster_score * weighted$normalized_cluster_weight
  scores <- stats::aggregate(weighted_cluster_score ~ alternative_id + alternative_label, weighted, sum)
  names(scores)[names(scores) == "weighted_cluster_score"] <- "score"
  scores <- scores[order(-scores$score, scores$alternative_id), , drop = FALSE]
  scores$rank <- seq_len(nrow(scores))
  row.names(scores) <- NULL
  scores
}

#' Rank FARO-admissible alternatives using DIA atomic indicators
#'
#' `volt_rank_dia()` keeps the two-level structure explicit: cluster weights
#' are normalised across clusters, and indicator weights are normalised only
#' inside their cluster.
#'
#' @param alternatives A `volt_alternatives` data frame.
#' @param clusters A `volt_cluster_plan` data frame.
#' @param indicators A `volt_indicator_registry` data frame.
#' @param performance A `volt_indicator_performance` data frame.
#' @param method Ranking method. Currently `DIA_WSM`.
#' @return A `volt_dia_ranking` list.
#' @export
volt_rank_dia <- function(alternatives, clusters, indicators, performance, method = "DIA_WSM") {
  alternatives <- volt_alternatives(alternatives)
  clusters <- volt_cluster_plan(clusters)
  indicators <- volt_indicator_registry(indicators)
  performance <- volt_indicator_performance(performance)
  if (!identical(method, "DIA_WSM")) stop("Only DIA_WSM is implemented in voltR v0.0.0.9000.", call. = FALSE)

  admitted <- alternatives[alternatives$faro_status == "ADMISSIBLE", , drop = FALSE]
  excluded <- alternatives[alternatives$faro_status != "ADMISSIBLE", , drop = FALSE]
  indicators <- .volt_indicator_weights(clusters, indicators)

  if (nrow(admitted)) {
    grid <- merge(
      admitted[c("alternative_id", "label")],
      indicators[c("cluster_id", "cluster_name", "indicator_id", "indicator_name", "direction",
        "normalized_cluster_weight", "normalized_indicator_weight", "source", "validation_status")],
      by = NULL
    )
    names(grid)[names(grid) == "label"] <- "alternative_label"
    names(grid)[names(grid) == "source"] <- "indicator_source"
    grid <- merge(grid, performance, by = c("alternative_id", "indicator_id"), all.x = TRUE)
    grid$normalized_value <- NA_real_
    for (indicator_id in unique(grid$indicator_id)) {
      idx <- grid$indicator_id == indicator_id
      direction <- unique(grid$direction[idx])[1]
      grid$normalized_value[idx] <- .volt_normalize(grid$value[idx], direction)
    }
    grid$indicator_contribution <- grid$normalized_value * grid$normalized_indicator_weight
    grid$contribution_for_score <- ifelse(is.na(grid$indicator_contribution), 0, grid$indicator_contribution)
    cluster_scores <- stats::aggregate(
      contribution_for_score ~ alternative_id + alternative_label + cluster_id + cluster_name,
      grid,
      sum
    )
    names(cluster_scores)[names(cluster_scores) == "contribution_for_score"] <- "cluster_score"
    cluster_scores <- merge(
      cluster_scores,
      unique(indicators[c("cluster_id", "normalized_cluster_weight")]),
      by = "cluster_id",
      all.x = TRUE
    )
    cluster_scores$weighted_cluster_score <- cluster_scores$cluster_score * cluster_scores$normalized_cluster_weight
    scores <- .volt_rank_from_cluster_scores(
      cluster_scores[c("alternative_id", "alternative_label", "cluster_id", "cluster_score")],
      unique(indicators[c("cluster_id", "normalized_cluster_weight")])
    )
  } else {
    grid <- data.frame()
    cluster_scores <- data.frame()
    scores <- data.frame(
      alternative_id = character(),
      alternative_label = character(),
      score = numeric(),
      rank = integer(),
      stringsAsFactors = FALSE
    )
  }

  completeness <- volt_dia_completeness_table(admitted, indicators, grid)
  audit <- data.frame(
    check = c(
      "Confine FARO",
      "Passaggio DIA",
      "Struttura cluster",
      "Copertura indicatori",
      "Valori prestazione mancanti",
      "Fonti provvisorie o mancanti",
      "Normalizzazione pesi"
    ),
    status = c(
      if (nrow(admitted)) "PASS" else "FAIL",
      "PASS",
      if (nrow(clusters) == 5) "PASS" else "WARN",
      if (all(completeness$missing_values == 0)) "PASS" else "WARN",
      if (any(is.na(grid$value))) "WARN" else "PASS",
      if (any(indicators$validation_status != "VALIDATED") || any(grid$evidence_status != "VALIDATED", na.rm = TRUE)) "WARN" else "PASS",
      "PASS"
    ),
    message = c(
      paste(nrow(admitted), "FARO-admissible alternative(s);", nrow(excluded), "excluded or pending."),
      "DIA fornisce indicatori atomici; VOLT usa solo campi, pesi, direzioni e stato evidenza dichiarati.",
      paste(nrow(clusters), "cluster(s) declared; the current VOLT project contract expects five clusters."),
      paste(sum(completeness$missing_values), "missing alternative-indicator value(s)."),
      paste(sum(is.na(grid$value)), "missing performance value(s) among FARO-admissible alternatives."),
      paste(sum(indicators$validation_status != "VALIDATED"), "indicator(s) not validated;",
        sum(grid$evidence_status != "VALIDATED", na.rm = TRUE), "performance row(s) not validated."),
      paste("Cluster weights sum", signif(sum(clusters$normalized_cluster_weight), 6),
        "and indicator weights sum to 1 within each cluster.")
    ),
    stringsAsFactors = FALSE
  )

  sensitivity <- volt_dia_sensitivity_table(scores, cluster_scores, clusters, cluster_delta = 0.10)

  .volt_new(list(
    method = method,
    alternatives = alternatives,
    clusters = clusters,
    indicators = indicators,
    performance = performance,
    admitted = admitted,
    excluded = excluded,
    indicator_contributions = grid,
    cluster_scores = cluster_scores,
    result = scores,
    completeness = completeness,
    sensitivity = sensitivity,
    audit = audit,
    generated_at = .volt_now(),
    language_guard = "VOLT usa indicatori atomici DIA e ordina solo alternative FARO-ammissibili; supporta il confronto e non sostituisce la decisione."
  ), "volt_dia_ranking")
}

#' Summarise DIA indicator completeness
#'
#' @param ranking A `volt_dia_ranking`, or admitted alternatives plus indicators
#'   and contribution grid when used internally.
#' @return Data frame with completeness by cluster.
#' @export
volt_dia_completeness <- function(ranking) {
  if (!inherits(ranking, "volt_dia_ranking")) stop("ranking must be a volt_dia_ranking.", call. = FALSE)
  ranking$completeness
}

#' Build an operational validation checklist for a DIA-aware VOLT ranking
#'
#' @param ranking A `volt_dia_ranking`.
#' @return Data frame with checklist item, status and operational reading.
#' @export
volt_dia_validation_check <- function(ranking) {
  if (!inherits(ranking, "volt_dia_ranking")) stop("ranking must be a volt_dia_ranking.", call. = FALSE)
  missing_sources <- sum(!nzchar(ranking$indicators$source))
  missing_performance_sources <- sum(!nzchar(ranking$performance$source))
  data.frame(
    controllo = c(
      "Alternative FARO ammissibili",
      "Cluster dichiarati",
      "Indicatori collegati ai cluster",
      "Prestazioni collegate agli indicatori",
      "Valori mancanti",
      "Fonti indicatori",
      "Fonti prestazioni",
      "Pesi cluster",
      "Pesi indicatori",
      "Stati non validati"
    ),
    esito = c(
      if (nrow(ranking$admitted) > 0) "OK" else "BLOCCANTE",
      if (nrow(ranking$clusters) == 5) "OK" else "DA VERIFICARE",
      if (all(ranking$indicators$cluster_id %in% ranking$clusters$cluster_id)) "OK" else "BLOCCANTE",
      if (all(ranking$performance$indicator_id %in% ranking$indicators$indicator_id)) "OK" else "BLOCCANTE",
      if (sum(ranking$completeness$missing_values) == 0) "OK" else "DA DICHIARARE",
      if (missing_sources == 0) "OK" else "DA COMPLETARE",
      if (missing_performance_sources == 0) "OK" else "DA COMPLETARE",
      if (sum(ranking$clusters$cluster_weight) > 0) "OK" else "BLOCCANTE",
      if (all(ranking$indicators$indicator_weight >= 0)) "OK" else "BLOCCANTE",
      if (any(ranking$indicators$validation_status != "VALIDATED") ||
          any(ranking$performance$evidence_status != "VALIDATED", na.rm = TRUE)) "DA DICHIARARE" else "OK"
    ),
    lettura = c(
      paste(nrow(ranking$admitted), "alternative entrano nel ranking;", nrow(ranking$excluded), "restano escluse o sospese."),
      paste(nrow(ranking$clusters), "cluster presenti; il contratto VOLT/DIA corrente ne prevede cinque."),
      "Ogni indicatore deve appartenere a un cluster dichiarato.",
      "Ogni prestazione deve riferirsi a un indicatore presente nel registro.",
      paste(sum(ranking$completeness$missing_values), "valori mancanti restano visibili nel report."),
      paste(missing_sources, "indicatore/i senza fonte compilata."),
      paste(missing_performance_sources, "prestazione/i senza fonte compilata."),
      "I pesi cluster sono normalizzati esplicitamente prima del calcolo.",
      "I pesi indicatore sono normalizzati dentro ciascun cluster.",
      "Stati provvisori o mancanti non bloccano il ranking, ma devono accompagnarne la lettura."
    ),
    stringsAsFactors = FALSE
  )
}

#' Build a plain-language decision view for a DIA-aware VOLT ranking
#'
#' @param ranking A `volt_dia_ranking`.
#' @return Data frame with key decision questions and answers.
#' @export
volt_dia_decision_view <- function(ranking) {
  if (!inherits(ranking, "volt_dia_ranking")) stop("ranking must be a volt_dia_ranking.", call. = FALSE)
  if (!nrow(ranking$result)) {
    return(data.frame(
      domanda = "Quale alternativa risulta prima?",
      risposta = "Nessuna alternativa FARO-ammissibile e' disponibile per il ranking.",
      stringsAsFactors = FALSE
    ))
  }
  top <- ranking$result[1, , drop = FALSE]
  top_clusters <- ranking$cluster_scores[ranking$cluster_scores$alternative_id == top$alternative_id, , drop = FALSE]
  top_clusters <- top_clusters[order(-top_clusters$weighted_cluster_score), , drop = FALSE]
  weak_clusters <- top_clusters[order(top_clusters$weighted_cluster_score), , drop = FALSE]
  data.frame(
    domanda = c(
      "Quale alternativa risulta prima?",
      "Perche risulta prima?",
      "In quali cluster e' piu forte?",
      "In quali cluster e' piu debole?",
      "Quali dati sono incompleti?",
      "Che cosa non si puo concludere?"
    ),
    risposta = c(
      paste(top$alternative_label, "(", top$alternative_id, ") con score", signif(top$score, 4)),
      "Per la combinazione dichiarata di alternative FARO-ammissibili, cluster, indicatori, pesi e valori disponibili.",
      paste(utils::head(top_clusters$cluster_name, 2), collapse = ", "),
      paste(utils::head(weak_clusters$cluster_name, 2), collapse = ", "),
      paste(sum(ranking$completeness$missing_values), "valori mancanti;",
        sum(ranking$indicators$validation_status != "VALIDATED"), "indicatori non pienamente validati."),
      "Il ranking non prova da solo quale decisione adottare e non sostituisce valutazione politica, amministrativa o partecipativa."
    ),
    stringsAsFactors = FALSE
  )
}

#' Create or validate a VOLT decision log
#'
#' @param data Optional data frame with decision fields.
#' @return A `volt_decision_log` data frame.
#' @export
volt_decision_log <- function(data = NULL) {
  if (is.null(data)) {
    data <- data.frame(
      decisione = "",
      motivazione = "",
      scostamento_dal_ranking = "",
      condizioni = "",
      tempi = "",
      responsabili = "",
      feedback_previsto = "",
      stringsAsFactors = FALSE
    )
  }
  data <- .volt_as_df(data, "data")
  .volt_required(data, c("decisione", "motivazione", "scostamento_dal_ranking",
    "condizioni", "tempi", "responsabili", "feedback_previsto"), "data")
  .volt_new(data, c("volt_decision_log", "data.frame"))
}

#' Build an extended narrative report for a DIA-aware VOLT workflow
#'
#' @param ranking A `volt_dia_ranking`.
#' @param project Optional `volt_project`.
#' @param decision Optional `volt_decision_log`.
#' @return Data frame with report sections.
#' @export
volt_dia_narrative_report <- function(ranking, project = NULL, decision = NULL) {
  if (!inherits(ranking, "volt_dia_ranking")) stop("ranking must be a volt_dia_ranking.", call. = FALSE)
  if (is.null(decision)) decision <- volt_decision_log()
  decision <- volt_decision_log(decision)
  project_title <- if (inherits(project, "volt_project")) project$title else "Progetto VOLT/DIA"
  decision_text <- if (nzchar(decision$decisione[1])) decision$decisione[1] else "Decisione non ancora compilata."
  data.frame(
    sezione = c(
      "1. Di cosa tratta il progetto",
      "2. Oggetto della consultazione",
      "3. Pronunciamento e briefing informativo",
      "4. Posizioni prima e dopo le informazioni",
      "5. Ranking multicriterio",
      "6. Decisione assunta",
      "7. Scostamento motivato dal ranking",
      "8. Azioni di feedback",
      "9. Note e conclusioni finali"
    ),
    testo = c(
      paste("Il progetto", project_title, "usa VOLT per confrontare alternative gia filtrate dal perimetro FARO."),
      paste("La popolazione e gli stakeholder sono chiamati a pronunciarsi sulle alternative ammissibili; VOLT organizza il confronto tramite", nrow(ranking$clusters), "cluster e", nrow(ranking$indicators), "indicatori atomici."),
      "Il briefing informativo deve essere documentato nel fascicolo del percorso; VOLT conserva qui ranking, fonti, completezza e limiti.",
      "Le variazioni delle posizioni prima/dopo briefing appartengono al modulo partecipativo collegato; VOLT ne accoglie gli esiti solo come prestazioni o evidenze dichiarate.",
      if (nrow(ranking$result)) paste("La prima alternativa classificata e", ranking$result$alternative_id[1], "con score", signif(ranking$result$score[1], 4), ".") else "Nessuna alternativa risulta classificata.",
      decision_text,
      if (nzchar(decision$scostamento_dal_ranking[1])) decision$scostamento_dal_ranking[1] else "Eventuali scostamenti dal ranking devono essere motivati esplicitamente.",
      if (nzchar(decision$feedback_previsto[1])) decision$feedback_previsto[1] else "Azioni di feedback non ancora compilate.",
      ranking$language_guard
    ),
    stringsAsFactors = FALSE
  )
}

volt_dia_completeness_table <- function(admitted, indicators, grid) {
  if (!nrow(admitted) || !nrow(indicators)) {
    return(data.frame(
      cluster_id = character(),
      cluster_name = character(),
      expected_values = integer(),
      present_values = integer(),
      missing_values = integer(),
      completezza = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  total <- stats::aggregate(indicator_id ~ cluster_id + cluster_name, indicators, length)
  names(total)[names(total) == "indicator_id"] <- "indicators"
  total$expected_values <- total$indicators * nrow(admitted)
  grid$.present_value <- !is.na(grid$value)
  present <- stats::aggregate(.present_value ~ cluster_id + cluster_name, grid, sum)
  names(present)[names(present) == ".present_value"] <- "present_values"
  out <- merge(total, present, by = c("cluster_id", "cluster_name"), all.x = TRUE)
  out$present_values[is.na(out$present_values)] <- 0
  out$missing_values <- out$expected_values - out$present_values
  out$completezza <- ifelse(out$expected_values > 0, out$present_values / out$expected_values, NA_real_)
  out[order(out$cluster_id), c("cluster_id", "cluster_name", "expected_values", "present_values", "missing_values", "completezza")]
}

volt_dia_sensitivity_table <- function(scores, cluster_scores, clusters, cluster_delta = 0.10) {
  if (!nrow(scores) || !nrow(cluster_scores)) {
    return(data.frame(
      scenario = character(),
      cluster_id = character(),
      top_alternative = character(),
      top_changed = logical(),
      stringsAsFactors = FALSE
    ))
  }
  base_top <- scores$alternative_id[1]
  base_weights <- clusters[c("cluster_id", "normalized_cluster_weight")]
  out <- lapply(seq_len(nrow(base_weights)), function(i) {
    w <- base_weights
    w$normalized_cluster_weight[i] <- w$normalized_cluster_weight[i] * (1 + cluster_delta)
    w$normalized_cluster_weight <- w$normalized_cluster_weight / sum(w$normalized_cluster_weight)
    s <- .volt_rank_from_cluster_scores(
      cluster_scores[c("alternative_id", "alternative_label", "cluster_id", "cluster_score")],
      w
    )
    data.frame(
      scenario = paste0("+", cluster_delta * 100, "% peso cluster"),
      cluster_id = w$cluster_id[i],
      top_alternative = s$alternative_id[1],
      top_changed = !identical(s$alternative_id[1], base_top),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' @export
print.volt_dia_ranking <- function(x, ...) {
  cat("<volt_dia_ranking> ", x$method, "\n", sep = "")
  cat("  alternatives ranked: ", nrow(x$result), "\n", sep = "")
  cat("  clusters: ", nrow(x$clusters), "\n", sep = "")
  cat("  atomic indicators: ", nrow(x$indicators), "\n", sep = "")
  if (nrow(x$result)) cat("  first: ", x$result$alternative_id[1], "\n", sep = "")
  invisible(x)
}

#' @export
summary.volt_dia_ranking <- function(object, ...) {
  data.frame(
    method = object$method,
    alternatives_ranked = nrow(object$result),
    alternatives_excluded = nrow(object$excluded),
    clusters = nrow(object$clusters),
    indicators = nrow(object$indicators),
    missing_values = sum(object$completeness$missing_values),
    first_alternative = if (nrow(object$result)) object$result$alternative_id[1] else NA_character_,
    language_guard = object$language_guard,
    stringsAsFactors = FALSE
  )
}

#' Plot a DIA-aware VOLT ranking
#'
#' @param x A `volt_dia_ranking`.
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
plot.volt_dia_ranking <- function(x, ...) {
  if (!nrow(x$result)) {
    plot.new()
    text(.5, .5, "No FARO-admissible alternatives to rank")
    return(invisible(x))
  }
  op <- par(mar = c(5, 10, 3, 1))
  on.exit(par(op), add = TRUE)
  d <- x$result[order(x$result$score), , drop = FALSE]
  barplot(d$score, names.arg = d$alternative_label, horiz = TRUE, las = 1,
    xlab = "VOLT score", main = "Ranking da indicatori atomici DIA")
  invisible(x)
}

#' @export
print.volt_ranking <- function(x, ...) {
  cat("<volt_ranking> ", x$method, "\n", sep = "")
  cat("  alternatives ranked: ", nrow(x$result), "\n", sep = "")
  if (nrow(x$result)) cat("  first: ", x$result$alternative_id[1], "\n", sep = "")
  invisible(x)
}

#' @export
summary.volt_ranking <- function(object, ...) {
  data.frame(
    method = object$method,
    alternatives_ranked = nrow(object$result),
    alternatives_excluded = nrow(object$excluded),
    first_alternative = if (nrow(object$result)) object$result$alternative_id[1] else NA_character_,
    language_guard = object$language_guard,
    stringsAsFactors = FALSE
  )
}

#' Plot a VOLT ranking
#'
#' @param x A `volt_ranking`.
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
plot.volt_ranking <- function(x, ...) {
  if (!nrow(x$result)) {
    plot.new()
    text(.5, .5, "No admissible alternatives to rank")
    return(invisible(x))
  }
  op <- par(mar = c(5, 10, 3, 1))
  on.exit(par(op), add = TRUE)
  d <- x$result[order(x$result$score), , drop = FALSE]
  barplot(d$score, names.arg = d$alternative_label, horiz = TRUE, las = 1,
    xlab = "VOLT score", main = "Ranking alternative ammissibili")
  invisible(x)
}

#' Create a synthetic VOLT demo project
#'
#' @return A list with project, alternatives, criteria, performance, and ranking.
#' @export
volt_demo_project <- function() {
  project <- volt_project(
    "VOLT-DEMO-001",
    "Demo ranking post-FARO",
    "Synthetic comparison among alternatives declared admissible after FARO.",
    provenance = list(source = "synthetic voltR demo", classification = "synthetic")
  )
  alternatives <- volt_alternatives(data.frame(
    alternative_id = c("ALT_FV", "ALT_BIOMASS", "ALT_HYDRO", "ALT_WIND"),
    label = c("Impianto fotovoltaico", "Biomassa locale", "Mini idroelettrico", "Eolico di crinale"),
    faro_status = c("ADMISSIBLE", "ADMISSIBLE", "ADMISSIBLE", "NOT_ADMISSIBLE"),
    notes = c("Ammissibile dopo FARO", "Ammissibile con cautele", "Ammissibile", "Escluso dal filtro FARO"),
    stringsAsFactors = FALSE
  ))
  criteria <- volt_criteria(data.frame(
    criterion_id = c("production", "investment", "land_use", "acceptability"),
    label = c("Produzione attesa", "Investimento", "Uso suolo", "Accettabilita"),
    direction = c("max", "min", "min", "max"),
    weight = c(.35, .25, .20, .20),
    unit = c("MWh/anno", "milioni EUR", "ha", "scala 0-100"),
    description = c(
      "Energia annua stimata",
      "Costo di investimento",
      "Superficie interessata",
      "Preferenza o accettabilita rilevata"
    ),
    stringsAsFactors = FALSE
  ))
  performance <- volt_performance(data.frame(
    alternative_id = rep(c("ALT_FV", "ALT_BIOMASS", "ALT_HYDRO", "ALT_WIND"), each = 4),
    criterion_id = rep(c("production", "investment", "land_use", "acceptability"), times = 4),
    value = c(950, 2.8, 3.2, 78, 620, 2.1, 1.4, 64, 510, 1.7, .8, 70, 840, 3.5, 2.7, 45),
    source = "synthetic_demo",
    stringsAsFactors = FALSE
  ))
  ranking <- volt_rank(alternatives, criteria, performance)
  .volt_new(list(project = project, alternatives = alternatives, criteria = criteria,
    performance = performance, ranking = ranking), "volt_demo_project")
}

#' Create a synthetic DIA-aware VOLT demo project
#'
#' @return A list with project, alternatives, clusters, indicators,
#'   indicator performance, and DIA-aware ranking.
#' @export
volt_dia_demo_project <- function() {
  project <- volt_project(
    "VOLT-DIA-DEMO-001",
    "Demo VOLT con indicatori atomici DIA",
    "Synthetic comparison using five clusters and DIA-style atomic indicators.",
    provenance = list(
      source = "synthetic voltR DIA demo",
      classification = "synthetic",
      upstream = "DIA atomic indicator handoff, FARO admissibility boundary"
    )
  )
  alternatives <- volt_alternatives(data.frame(
    alternative_id = c("ALT_FV", "ALT_BIOMASSA", "ALT_IDRO", "ALT_EOLICO"),
    label = c("Impianto fotovoltaico", "Biomassa locale", "Mini idroelettrico", "Eolico di crinale"),
    faro_status = c("ADMISSIBLE", "ADMISSIBLE", "ADMISSIBLE", "NOT_ADMISSIBLE"),
    notes = c("Ammissibile dopo FARO", "Ammissibile con cautele", "Ammissibile", "Escluso dal filtro FARO"),
    stringsAsFactors = FALSE
  ))
  clusters <- volt_cluster_plan(data.frame(
    cluster_id = c("C1", "C2", "C3", "C4", "C5"),
    cluster_name = c("Energia", "Ambiente", "Economia", "Territorio", "Partecipazione"),
    cluster_weight = c(.25, .25, .20, .15, .15),
    description = c(
      "Produzione, continuita e resilienza energetica.",
      "Impatto ambientale, suolo, emissioni e reversibilita.",
      "Costi, ricadute locali e sostenibilita economica.",
      "Coerenza territoriale, accessibilita e interferenze.",
      "Accettabilita, consenso informato e qualita del confronto."
    ),
    stringsAsFactors = FALSE
  ))
  indicator_grid <- expand.grid(
    cluster_id = clusters$cluster_id,
    idx = seq_len(10),
    stringsAsFactors = FALSE
  )
  indicator_grid$indicator_id <- paste0(indicator_grid$cluster_id, "_I", sprintf("%02d", indicator_grid$idx))
  indicator_grid$indicator_name <- paste(
    rep(clusters$cluster_name, each = 10),
    "indicatore", indicator_grid$idx
  )
  direction_pattern <- c("max", "max", "min", "max", "min", "max", "min", "max", "min", "max")
  indicator_grid$direction <- rep(direction_pattern, times = nrow(clusters))
  indicator_grid$indicator_weight <- rep(c(1.2, 1, 1, .8, .9, 1.1, 1, .9, 1, 1.1), times = nrow(clusters))
  indicator_grid$unit <- "scala 0-100"
  indicator_grid$source <- "DIA_demo_sintetico"
  indicator_grid$validation_status <- "VALIDATED"
  indicator_grid$validation_status[indicator_grid$indicator_id %in% c("C2_I07", "C5_I09")] <- "PENDING_VERIFICATION"
  indicator_grid$description <- "Indicatore atomico dimostrativo, sostituibile con handoff DIA reale."
  indicators <- volt_indicator_registry(indicator_grid[
    c("cluster_id", "indicator_id", "indicator_name", "direction", "indicator_weight",
      "unit", "source", "validation_status", "description")
  ])

  admitted_ids <- alternatives$alternative_id[alternatives$faro_status == "ADMISSIBLE"]
  performance <- expand.grid(
    alternative_id = admitted_ids,
    indicator_id = indicators$indicator_id,
    stringsAsFactors = FALSE
  )
  alt_offset <- match(performance$alternative_id, admitted_ids) * 4
  ind_number <- as.integer(sub(".*_I", "", performance$indicator_id))
  cluster_number <- as.integer(sub("C([0-9]+).*", "\\1", performance$indicator_id))
  performance$value <- pmax(5, pmin(100, 58 + alt_offset + (cluster_number * 3) + ((ind_number %% 5) * 4)))
  performance$value[performance$alternative_id == "ALT_BIOMASSA" & performance$indicator_id %in% c("C2_I03", "C2_I07")] <- NA_real_
  performance$value[performance$alternative_id == "ALT_IDRO" & performance$indicator_id %in% c("C4_I04")] <- NA_real_
  performance$source <- "DIA_demo_sintetico"
  performance$evidence_status <- ifelse(is.na(performance$value), "MISSING", "VALIDATED")
  performance$evidence_status[performance$indicator_id %in% c("C1_I08", "C5_I09") & !is.na(performance$value)] <- "PENDING_VERIFICATION"
  performance <- volt_indicator_performance(performance)

  ranking <- volt_rank_dia(alternatives, clusters, indicators, performance)
  .volt_new(list(
    project = project,
    alternatives = alternatives,
    clusters = clusters,
    indicators = indicators,
    indicator_performance = performance,
    ranking = ranking
  ), "volt_dia_demo_project")
}

#' Create the complete Valtaro-Valceno participation and VOLT demo
#'
#' This demo is deliberately synthetic but domain-shaped. It models a complete
#' participation path on proposed photovoltaic and micro-hydro plants across
#' Valtaro-Valceno, including meetings, briefing, anonymous participants,
#' pre/post opinions, DIA/VOLT indicators, MCDA ranking, motivated decision,
#' follow-up actions and narrative report sections.
#'
#' The function does not use official demographic or technical sources. Every
#' demographic, cost, production, participation and preference value is a
#' plausible simulation for demonstration and training purposes.
#'
#' @param sample_error Declared sampling error for the synthetic sample-size
#'   formula. Values such as `0.10` and `0.05` are typical; other positive
#'   values below 1 are accepted.
#' @return A list with participation tables, DIA/VOLT project objects, ranking,
#'   decision log and narrative reports.
#' @export
volt_valtaro_valceno_demo_project <- function(sample_error = 0.10) {
  sample_error <- suppressWarnings(as.numeric(sample_error))
  if (length(sample_error) != 1 || is.na(sample_error) || sample_error <= 0 || sample_error >= 1) {
    stop("sample_error must be a single positive number below 1.", call. = FALSE)
  }
  assumptions <- data.frame(
    ambito = c(
      "Classificazione dati",
      "Numero incontri",
      "Periodo",
      "Campione",
      "Autorizzazioni",
      "Uso della demo"
    ),
    assunzione = c(
      "Tutti i valori sono sintetici e servono solo a dimostrare il flusso operativo.",
      "L'elenco e' letto come 10 incontri, assumendo Bardi fraz. Boccolo come unica sede.",
      "Il percorso si svolge dal 1 novembre 2026 al 30 marzo 2027.",
      paste0("La numerosita minima e' calcolata per proporzioni con Z=1.96, p=0.50, errore=", sample_error, " e correzione per popolazione finita."),
      "La demo non sostituisce verifiche tecniche, paesaggistiche, ambientali o amministrative degli enti competenti.",
      "Il risultato atteso e' un esempio completo del report prodotto con dati reali."
    ),
    stringsAsFactors = FALSE
  )

  meetings <- data.frame(
    meeting_id = sprintf("INC%02d", 1:10),
    comune = c(
      "Borgo Val di Taro",
      "Borgo Val di Taro",
      "Tornolo",
      "Albareto",
      "Bardi",
      "Bardi",
      "Varsi",
      "Valmozzola",
      "Pellegrino Parmense",
      "Varano de' Melegari"
    ),
    sede = c(
      "Borgo Val di Taro capoluogo",
      "Frazione Tiedoli",
      "Tornolo capoluogo",
      "Albareto capoluogo",
      "Bardi capoluogo",
      "Frazione Boccolo",
      "Varsi capoluogo",
      "Valmozzola capoluogo",
      "Pellegrino Parmense capoluogo",
      "Varano de' Melegari capoluogo"
    ),
    data_prevista = as.character(seq(as.Date("2026-11-08"), as.Date("2027-03-14"), length.out = 10)),
    formato = c(
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario",
      "in presenza + streaming + questionario"
    ),
    obiettivo = "Informazione, confronto guidato e raccolta preferenze su impianti energetici locali.",
    stringsAsFactors = FALSE
  )

  facilities <- data.frame(
    alternative_id = c("FV_PELLEGRINO", "FV_VARSI", "FV_BARDI_SORBA", "FV_ALBARETO", "FV_BORGOTARO_ROVINAGLIA", "IDRO_BARDI_SORBA"),
    tecnologia = c("Fotovoltaico", "Fotovoltaico", "Fotovoltaico", "Fotovoltaico", "Fotovoltaico", "Micro-idroelettrico"),
    localizzazione = c(
      "Pellegrino Parmense, tetto palestra comunale",
      "Varsi, tetto edificio pubblico",
      "Bardi, loc. Sorba, area comunale a terra",
      "Albareto, tetto magazzino comunale",
      "Borgo Val di Taro, loc. Rovinaglia, proprieta ente religioso",
      "Bardi, loc. Sorba, turbina su tubazione esistente"
    ),
    potenza = c(30, 30, 30, 30, 30, 30),
    unita_potenza = c("kWp", "kWp", "kWp", "kWp", "kWp", "kW"),
    superficie_tipo = c("tetto piano", "tetto piano", "area a terra recintata", "tetto piano", "tetto piano", "tubazione esistente"),
    proprieta_o_disponibilita = c("pubblica", "pubblica", "comunale", "pubblica", "ente religioso", "acquedotto/tubazione esistente"),
    nota_impatto = c(
      "Impatto visivo ordinariamente contenuto, da verificare su copertura e vincoli.",
      "Impatto visivo ordinariamente contenuto, da verificare su copertura e vincoli.",
      "Area gia dedicata a servitu energetica; resta da dichiarare il consumo di suolo e la reversibilita.",
      "Impatto visivo ordinariamente contenuto, da verificare su copertura e vincoli.",
      "Coinvolge proprieta non pubblica; richiede accordo e verifica di compatibilita.",
      "Usa acqua gia convogliata in tubazione da troppo pieno; impatto preliminarmente contenuto ma da verificare."
    ),
    stringsAsFactors = FALSE
  )

  population_frame <- data.frame(
    territorio = meetings$sede,
    comune = meetings$comune,
    residenti_demo = c(6200, 170, 900, 2050, 1900, 95, 1050, 520, 940, 2600),
    quota_18_34 = c(.17, .12, .14, .15, .13, .10, .14, .12, .15, .16),
    quota_35_64 = c(.46, .42, .44, .45, .43, .41, .44, .43, .44, .45),
    quota_65_piu = c(.37, .46, .42, .40, .44, .49, .42, .45, .41, .39),
    indice_vecchiaia_demo = c(245, 330, 295, 270, 315, 360, 300, 335, 285, 255),
    fonte = "sintetica_demo_non_ufficiale",
    stringsAsFactors = FALSE
  )
  municipality_population <- stats::aggregate(
    residenti_demo ~ comune,
    population_frame,
    sum
  )
  municipality_population <- municipality_population[order(-municipality_population$residenti_demo), , drop = FALSE]
  row.names(municipality_population) <- NULL
  total_population <- sum(population_frame$residenti_demo)
  z <- 1.96
  p <- 0.50
  alpha <- sample_error
  n0 <- (z^2 * p * (1 - p)) / alpha^2
  n_finite <- n0 / (1 + ((n0 - 1) / total_population))
  target_n <- ceiling(n_finite)
  sample_formula <- data.frame(
    parametro = c("N", "Z", "p", "errore", "n0", "n_corretto", "n_target"),
    valore = c(total_population, z, p, alpha, n0, n_finite, target_n),
    lettura = c(
      "Popolazione sintetica dell'area demo.",
      "Livello di confidenza dimostrativo 95%.",
      "Proporzione conservativa.",
      "Errore ammesso dichiarato per demo territoriale.",
      "Ampiezza per popolazione infinita.",
      "Ampiezza con correzione per popolazione finita.",
      "Numero minimo arrotondato verso l'alto."
    ),
    stringsAsFactors = FALSE
  )
  sample_plan <- population_frame[c("territorio", "comune", "residenti_demo")]
  sample_plan$quota_popolazione <- sample_plan$residenti_demo / total_population
  sample_plan$campione_ideale <- pmax(1, round(target_n * sample_plan$quota_popolazione))
  diff_n <- target_n - sum(sample_plan$campione_ideale)
  if (diff_n != 0) sample_plan$campione_ideale[which.max(sample_plan$residenti_demo)] <-
    sample_plan$campione_ideale[which.max(sample_plan$residenti_demo)] + diff_n
  municipality_sample_plan <- stats::aggregate(
    cbind(residenti_demo, campione_ideale) ~ comune,
    sample_plan,
    sum
  )
  municipality_sample_plan$quota_popolazione <- municipality_sample_plan$residenti_demo / sum(municipality_sample_plan$residenti_demo)
  municipality_sample_plan <- municipality_sample_plan[order(-municipality_sample_plan$residenti_demo), , drop = FALSE]
  row.names(municipality_sample_plan) <- NULL

  briefing <- data.frame(
    sezione = c(
      "Obiettivo",
      "Fotovoltaico su copertura",
      "Fotovoltaico a terra Sorba",
      "Micro-idroelettrico Sorba",
      "Criteri di valutazione",
      "Limiti",
      "Esito atteso"
    ),
    testo = c(
      "Il percorso informa e raccoglie preferenze motivate su impianti energetici locali di piccola scala.",
      "Gli impianti FV su tetto usano superfici esistenti e richiedono verifica tecnica delle coperture.",
      "L'impianto FV di Sorba e' a terra, in area recintata gia dedicata a servitu energetica.",
      "L'idroelettrico usa una tubazione esistente da troppo pieno; il quadro ambientale deve comunque essere verificato.",
      "Le alternative sono confrontate su energia, ambiente, economia, territorio e partecipazione.",
      "Il ranking non e' una decisione automatica e non sostituisce autorizzazioni o valutazioni degli enti competenti.",
      "Il report finale rende visibili dati, preferenze, ranking, limiti, decisione motivata e azioni di feedback."
    ),
    stringsAsFactors = FALSE
  )

  questionnaire <- data.frame(
    question_id = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8"),
    testo = c(
      "Quanto ritiene prioritario ridurre la spesa energetica degli edifici pubblici?",
      "Quanto ritiene accettabile il fotovoltaico su edifici pubblici?",
      "Quanto ritiene accettabile il fotovoltaico a terra in area gia destinata a servitu energetica?",
      "Quanto ritiene accettabile il micro-idroelettrico su tubazione esistente?",
      "Quale alternativa considera prioritaria?",
      "La sua opinione e' cambiata dopo il briefing informativo?",
      "Quale limite ritiene piu importante dichiarare?",
      "Vuole ricevere una restituzione pubblica sugli esiti?"
    ),
    scala = c("1-5", "1-5", "1-5", "1-5", "scelta alternativa", "si/no/in parte", "scelta", "si/no"),
    uso_analitico = c(
      "Profilo di priorita generale",
      "Accettabilita FV su tetto",
      "Accettabilita FV a terra",
      "Accettabilita micro-idro",
      "Preferenza partecipativa",
      "Variazione pre/post informazione",
      "Limiti percepiti",
      "Feedback e accountability"
    ),
    stringsAsFactors = FALSE
  )

  n_participants <- 208
  participant_id <- sprintf("P%03d", seq_len(n_participants))
  territory_index <- ((seq_len(n_participants) - 1) %% nrow(meetings)) + 1
  role <- ifelse(seq_len(n_participants) %% 7 == 0, "stakeholder", "cittadino")
  age_levels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
  participants <- data.frame(
    participant_id = participant_id,
    ruolo = role,
    comune_o_sede = meetings$sede[territory_index],
    classe_eta_istat = age_levels[((seq_len(n_participants) - 1) %% length(age_levels)) + 1],
    sesso = ifelse(seq_len(n_participants) %% 2 == 0, "F", "M"),
    modalita_partecipazione = c("in presenza", "da remoto", "questionario")[(seq_len(n_participants) %% 3) + 1],
    fonte_reclutamento = c(
      "notizia sui social",
      "pubblicistica cartacea",
      "contatti personali",
      "sito istituzionale Comune",
      "altro"
    )[(seq_len(n_participants) %% 5) + 1],
    gruppo_difficile_da_raggiungere = c(
      "distanza dal centro abitato",
      "contatto facile",
      "socialmente attivo"
    )[(seq_len(n_participants) %% 3) + 1],
    preferenza_pre = facilities$alternative_id[((seq_len(n_participants) + 1) %% nrow(facilities)) + 1],
    preferenza_post = facilities$alternative_id[((seq_len(n_participants) + ifelse(seq_len(n_participants) %% 5 == 0, 5, 2)) %% nrow(facilities)) + 1],
    supporto_pre = pmin(5, 2 + (seq_len(n_participants) %% 4)),
    supporto_post = pmin(5, 3 + (seq_len(n_participants) %% 3)),
    stringsAsFactors = FALSE
  )
  participants$supporto_post[participants$preferenza_post == "IDRO_BARDI_SORBA"] <- 5
  participants$supporto_post[participants$preferenza_post == "FV_BARDI_SORBA"] <- pmax(3, participants$supporto_post[participants$preferenza_post == "FV_BARDI_SORBA"] - 1)
  participants$meeting_id <- meetings$meeting_id[match(participants$comune_o_sede, meetings$sede)]

  participant_summary <- data.frame(
    indicatore = c(
      "partecipanti_totali",
      "cittadini",
      "stakeholder",
      "in_presenza",
      "da_remoto",
      "questionario",
      "cambi_pre_post"
    ),
    valore = c(
      nrow(participants),
      sum(participants$ruolo == "cittadino"),
      sum(participants$ruolo == "stakeholder"),
      sum(participants$modalita_partecipazione == "in presenza"),
      sum(participants$modalita_partecipazione == "da remoto"),
      sum(participants$modalita_partecipazione == "questionario"),
      sum(participants$preferenza_pre != participants$preferenza_post)
    ),
    stringsAsFactors = FALSE
  )

  observed_by_territory <- as.data.frame(table(participants$comune_o_sede), stringsAsFactors = FALSE)
  names(observed_by_territory) <- c("territorio", "campione_osservato")
  sample_check <- merge(sample_plan, observed_by_territory, by = "territorio", all.x = TRUE)
  sample_check$campione_osservato[is.na(sample_check$campione_osservato)] <- 0
  sample_check$scostamento <- sample_check$campione_osservato - sample_check$campione_ideale
  sample_check$lettura <- ifelse(
    sample_check$scostamento >= 0,
    "copertura osservata pari o superiore al minimo demo",
    "copertura osservata inferiore al minimo demo; limite da dichiarare"
  )
  municipality_observed <- stats::aggregate(
    campione_osservato ~ comune,
    sample_check,
    sum
  )
  municipality_sample_check <- merge(
    municipality_sample_plan,
    municipality_observed,
    by = "comune",
    all.x = TRUE
  )
  municipality_sample_check$campione_osservato[is.na(municipality_sample_check$campione_osservato)] <- 0
  municipality_sample_check$scostamento <- municipality_sample_check$campione_osservato - municipality_sample_check$campione_ideale
  municipality_sample_check$lettura <- ifelse(
    municipality_sample_check$scostamento >= 0,
    "copertura osservata pari o superiore al minimo demo",
    "copertura osservata inferiore al minimo demo; limite da dichiarare"
  )

  agora_connection <- data.frame(
    elemento = c(
      "Origine metodologica",
      "Oggetto agoRa equivalente",
      "Demografia",
      "Campione",
      "Partecipanti",
      "Nota"
    ),
    contenuto = c(
      "Handoff compatibile con agoRa Block 01: popolazione, piano campionario, partecipanti anonimi e percorso di reclutamento.",
      "agora_valtaro_valceno_demo / agora_valtaro_valceno_population quando disponibili nel pacchetto agoRa.",
      "In questa demo VOLT sono esposte tabelle demografiche sintetiche strutturate come frame di popolazione.",
      "Il pannello campionario mostra Comune, sede, atteso, osservato e scostamento.",
      "Cittadini e stakeholder restano distinti e non vengono fusi nel campione popolazione.",
      "Il collegamento e' documentale-operativo: voltR non richiede agoRa come dipendenza obbligatoria."
    ),
    stringsAsFactors = FALSE
  )

  project <- volt_project(
    "VOLT-VALTARO-VALCENO-2026-2027",
    "Percorso partecipativo Valtaro-Valceno energia locale",
    "Demo completa su incontri informativi, co-decisione e confronto MCDA di impianti FV e micro-idroelettrici.",
    provenance = list(
      source = "synthetic Valtaro-Valceno complete demo",
      classification = "synthetic",
      period_start = "2026-11-01",
      period_end = "2027-03-30"
    ),
    workflow_mode = "agora_linked"
  )
  alternatives <- volt_alternatives(data.frame(
    alternative_id = facilities$alternative_id,
    label = paste(facilities$tecnologia, facilities$localizzazione, sep = " - "),
    faro_status = rep("ADMISSIBLE", nrow(facilities)),
    notes = facilities$nota_impatto,
    stringsAsFactors = FALSE
  ))
  clusters <- volt_cluster_plan(data.frame(
    cluster_id = c("ENER", "AMBI", "ECON", "TERR", "PART"),
    cluster_name = c("Energia", "Ambiente", "Economia", "Territorio", "Partecipazione"),
    cluster_weight = c(.25, .25, .20, .15, .15),
    description = c(
      "Produzione, continuita e maturita tecnica.",
      "Impatto ambientale, paesaggio, suolo e reversibilita.",
      "Costo, tempi e ricadute locali.",
      "Disponibilita dell'area, complessita amministrativa e coerenza pubblica.",
      "Accettabilita sociale, cambiamento informato e controversia residua."
    ),
    stringsAsFactors = FALSE
  ))
  indicator_data <- data.frame(
    cluster_id = rep(clusters$cluster_id, each = 3),
    indicator_id = c(
      "ENER_PROD_MWH", "ENER_CONTINUITA", "ENER_MATURITA",
      "AMBI_SUOLO", "AMBI_PAESAGGIO", "AMBI_REVERSIBILITA",
      "ECON_COSTO", "ECON_TEMPI", "ECON_RICADUTA",
      "TERR_DISPONIBILITA", "TERR_AUTORIZZAZIONI", "TERR_COERENZA_PUBBLICA",
      "PART_CONSENSO_POST", "PART_DELTA_CONSENSO", "PART_CONTROVERSIA"
    ),
    indicator_name = c(
      "Produzione annua stimata",
      "Continuita producibilita",
      "Maturita tecnica preliminare",
      "Consumo di suolo",
      "Impatto paesaggistico",
      "Reversibilita",
      "Costo stimato",
      "Tempo realizzazione",
      "Ricaduta locale",
      "Disponibilita area o superficie",
      "Complessita autorizzativa",
      "Coerenza con interesse pubblico",
      "Consenso post briefing",
      "Aumento consenso dopo briefing",
      "Controversia residua"
    ),
    direction = c("max", "max", "max", "min", "min", "max", "min", "min", "max", "max", "min", "max", "max", "max", "min"),
    indicator_weight = c(1.2, .9, .9, 1.2, 1, .8, 1.2, .9, .9, 1, 1, 1, 1.2, .8, 1),
    unit = c("MWh/anno", "scala 0-100", "scala 0-100", "ha equivalenti", "scala 0-100", "scala 0-100", "EUR", "mesi", "scala 0-100", "scala 0-100", "scala 0-100", "scala 0-100", "scala 0-100", "punti", "scala 0-100"),
    source = "sintetica_demo_valtaro_valceno",
    validation_status = c(rep("PENDING_VERIFICATION", 12), rep("SYNTHETIC", 3)),
    description = "Indicatore dimostrativo da sostituire con dati tecnici, DIA o risultanze agoRa reali.",
    stringsAsFactors = FALSE
  )
  indicator_data$validation_status[indicator_data$validation_status == "SYNTHETIC"] <- "PENDING_VERIFICATION"
  indicators <- volt_indicator_registry(indicator_data)

  perf_values <- data.frame(
    alternative_id = facilities$alternative_id,
    ENER_PROD_MWH = c(36, 35, 38, 34, 36, 145),
    ENER_CONTINUITA = c(58, 57, 58, 56, 57, 82),
    ENER_MATURITA = c(78, 76, 64, 79, 61, 78),
    AMBI_SUOLO = c(.02, .02, .45, .02, .02, .01),
    AMBI_PAESAGGIO = c(22, 24, 48, 23, 30, 18),
    AMBI_REVERSIBILITA = c(82, 82, 70, 84, 80, 88),
    ECON_COSTO = c(45000, 47000, 52000, 44000, 51000, 70000),
    ECON_TEMPI = c(8, 8, 11, 7, 12, 14),
    ECON_RICADUTA = c(62, 61, 66, 60, 58, 68),
    TERR_DISPONIBILITA = c(88, 84, 78, 90, 56, 84),
    TERR_AUTORIZZAZIONI = c(34, 36, 52, 32, 60, 46),
    TERR_COERENZA_PUBBLICA = c(90, 88, 84, 91, 62, 78),
    PART_CONSENSO_POST = c(72, 70, 63, 74, 60, 81),
    PART_DELTA_CONSENSO = c(8, 7, 4, 8, 3, 14),
    PART_CONTROVERSIA = c(24, 25, 42, 22, 38, 20),
    stringsAsFactors = FALSE
  )
  indicator_performance <- do.call(rbind, lapply(seq_len(nrow(perf_values)), function(i) {
    row <- perf_values[i, , drop = FALSE]
    data.frame(
      alternative_id = row$alternative_id,
      indicator_id = names(row)[-1],
      value = as.numeric(unlist(row[1, -1], use.names = FALSE)),
      source = "sintetica_demo_valtaro_valceno",
      evidence_status = "SYNTHETIC",
      stringsAsFactors = FALSE
    )
  }))
  indicator_performance <- volt_indicator_performance(indicator_performance)
  ranking <- volt_rank_dia(alternatives, clusters, indicators, indicator_performance)
  mcda_base <- ranking$result[c("alternative_id", "score")]
  names(mcda_base)[names(mcda_base) == "score"] <- "punteggio_mcda"
  mcda_base$punteggio_mcda_norm <- mcda_base$punteggio_mcda / max(mcda_base$punteggio_mcda)
  event_rankings <- do.call(rbind, lapply(seq_len(nrow(meetings)), function(i) {
    event_participants <- participants[participants$meeting_id == meetings$meeting_id[i], , drop = FALSE]
    counts <- as.data.frame(table(event_participants$preferenza_post), stringsAsFactors = FALSE)
    names(counts) <- c("alternative_id", "preferenze_evento")
    out <- merge(facilities, counts, by = "alternative_id", all.x = TRUE)
    out <- merge(out, mcda_base, by = "alternative_id", all.x = TRUE)
    out$preferenze_evento[is.na(out$preferenze_evento)] <- 0
    out$quota_preferenze_evento <- if (sum(out$preferenze_evento) > 0) out$preferenze_evento / sum(out$preferenze_evento) else 0
    out$punteggio_evento <- (.70 * out$punteggio_mcda_norm) + (.30 * out$quota_preferenze_evento)
    out <- out[order(-out$punteggio_evento, out$alternative_id), , drop = FALSE]
    out$posizione_evento <- seq_len(nrow(out))
    out$meeting_id <- meetings$meeting_id[i]
    out$sede <- meetings$sede[i]
    out$comune <- meetings$comune[i]
    out$partecipanti_evento <- nrow(event_participants)
    out[c(
      "meeting_id", "sede", "comune", "partecipanti_evento", "posizione_evento",
      "alternative_id", "tecnologia", "localizzazione", "preferenze_evento",
      "quota_preferenze_evento", "punteggio_mcda", "punteggio_evento"
    )]
  }))
  row.names(event_rankings) <- NULL

  progressive_rankings <- do.call(rbind, lapply(seq_len(nrow(meetings)), function(i) {
    cumulative_participants <- participants[participants$meeting_id %in% meetings$meeting_id[seq_len(i)], , drop = FALSE]
    counts <- as.data.frame(table(cumulative_participants$preferenza_post), stringsAsFactors = FALSE)
    names(counts) <- c("alternative_id", "preferenze_cumulate")
    out <- merge(facilities, counts, by = "alternative_id", all.x = TRUE)
    out <- merge(out, mcda_base, by = "alternative_id", all.x = TRUE)
    out$preferenze_cumulate[is.na(out$preferenze_cumulate)] <- 0
    out$quota_preferenze_cumulata <- if (sum(out$preferenze_cumulate) > 0) out$preferenze_cumulate / sum(out$preferenze_cumulate) else 0
    out$punteggio_progressivo <- (.70 * out$punteggio_mcda_norm) + (.30 * out$quota_preferenze_cumulata)
    out <- out[order(-out$punteggio_progressivo, out$alternative_id), , drop = FALSE]
    out$posizione_progressiva <- seq_len(nrow(out))
    out$meeting_id <- meetings$meeting_id[i]
    out$fino_a_sede <- meetings$sede[i]
    out$eventi_inclusi <- i
    out$partecipanti_cumulati <- nrow(cumulative_participants)
    out$stato <- ifelse(i == nrow(meetings), "definitiva", "progressiva")
    out[c(
      "meeting_id", "fino_a_sede", "eventi_inclusi", "partecipanti_cumulati",
      "stato", "posizione_progressiva", "alternative_id", "tecnologia",
      "localizzazione", "preferenze_cumulate", "quota_preferenze_cumulata",
      "punteggio_mcda", "punteggio_progressivo"
    )]
  }))
  row.names(progressive_rankings) <- NULL
  final_progressive_ranking <- progressive_rankings[progressive_rankings$stato == "definitiva", , drop = FALSE]
  final_facility_ranking <- merge(
    ranking$result,
    facilities,
    by = "alternative_id",
    all.x = TRUE
  )
  final_facility_ranking <- final_facility_ranking[order(final_facility_ranking$rank), , drop = FALSE]
  final_facility_ranking$punteggio_finale <- round(final_facility_ranking$score, 4)
  final_facility_ranking$punto_di_forza <- c(
    "Produzione continua su infrastruttura esistente e consenso post-briefing elevato.",
    "Buona prontezza realizzativa su copertura pubblica e bassa criticita territoriale.",
    "Superficie pubblica riconoscibile e accettabilita positiva del FV su tetto.",
    "Soluzione su edificio pubblico con profilo tecnico ordinario.",
    "Potenziale FV su tetto, ma con accordo proprietario e verifiche aggiuntive.",
    "Area gia energetica, ma resta piu controversa per consumo di suolo e percezione locale."
  )
  final_facility_ranking$criticita_principale <- c(
    "Verificare quadro autorizzativo, compatibilita idraulica e condizioni tecniche della tubazione.",
    "Verificare idoneita statica e tecnico-economica della copertura.",
    "Verificare stato della copertura e costi aggiornati.",
    "Verificare compatibilita della copertura e tempi di connessione.",
    "Formalizzare accordo con proprieta religiosa e verificare vincoli.",
    "Dichiarare consumo di suolo, reversibilita e accettabilita residua."
  )
  final_facility_ranking$raccomandazione <- c(
    "Priorita istruttoria alta.",
    "Seconda priorita; candidabile a pacchetto FV su coperture pubbliche.",
    "Approfondire in parallelo come intervento FV maturo.",
    "Mantenere nel pacchetto tecnico, subordinato a verifica copertura.",
    "Mantenere come opzione condizionata ad accordi e vincoli.",
    "Non escludere, ma trattare come opzione subordinata a confronto pubblico e mitigazioni."
  )
  final_facility_ranking <- final_facility_ranking[
    c(
      "rank", "alternative_id", "alternative_label", "tecnologia", "localizzazione",
      "potenza", "unita_potenza", "punteggio_finale", "punto_di_forza",
      "criticita_principale", "raccomandazione"
    )
  ]
  names(final_facility_ranking)[names(final_facility_ranking) == "rank"] <- "posizione"
  names(final_facility_ranking)[names(final_facility_ranking) == "alternative_label"] <- "impianto"

  decision <- volt_decision_log(data.frame(
    decisione = "Avviare approfondimento tecnico prioritario sull'impianto micro-idroelettrico di Sorba e, in parallelo, sugli impianti FV su coperture pubbliche con maggiore prontezza realizzativa.",
    motivazione = "La demo combina accettabilita post-briefing, uso di infrastruttura esistente, produzione stimata e tracciabilita dei limiti. La decisione resta condizionata alle verifiche tecniche e amministrative.",
    scostamento_dal_ranking = "Nessuno scostamento sostanziale: il ranking orienta la priorita istruttoria, non approva automaticamente l'intervento.",
    condizioni = "Verifica tecnica della tubazione, quadro autorizzativo, accordi proprietari, costi aggiornati, compatibilita ambientale e restituzione pubblica.",
    tempi = "Istruttoria tecnica entro 90 giorni dalla chiusura del percorso; restituzione pubblica entro 120 giorni.",
    responsabili = "Gruppo tecnico comunale/intercomunale con supporto Eikos e confronto con enti competenti.",
    feedback_previsto = "Pubblicazione del report generale, incontri di restituzione, scheda di avanzamento per ciascun impianto e aggiornamento sul recepimento delle osservazioni.",
    stringsAsFactors = FALSE
  ))
  narrative_report <- volt_dia_narrative_report(ranking, project, decision)
  report_sections <- rbind(
    narrative_report,
    data.frame(
      sezione = c(
        "10. Quadro incontri",
        "11. Quadro demografico demo",
        "12. Campione e modalita",
        "13. Impianti proposti",
        "14. Criteri MCDA",
        "15. Classifiche parziali e progressive",
        "16. Follow-up"
      ),
      testo = c(
        paste(nrow(meetings), "incontri tra", min(meetings$data_prevista), "e", max(meetings$data_prevista), "con partecipazione in presenza, remota e tramite questionario."),
        paste("Popolazione demo complessiva:", total_population, "residenti sintetici; n campionario minimo:", target_n, "."),
        paste(nrow(participants), "partecipanti anonimi simulati, di cui", sum(participants$ruolo == "cittadino"), "cittadini e", sum(participants$ruolo == "stakeholder"), "stakeholder."),
        paste(nrow(facilities), "alternative energetiche: cinque FV da 30 kWp medi e un micro-idroelettrico da 30 kW."),
        paste(nrow(clusters), "cluster e", nrow(indicators), "indicatori atomici alimentano il ranking."),
        paste(nrow(meetings), "classifiche parziali alimentano una classifica progressiva; dopo l'ultimo evento lo stato diventa definitiva."),
        "Il follow-up prevede verifica tecnica, ritorno pubblico, monitoraggio degli step e tracciamento delle decisioni."
      ),
      stringsAsFactors = FALSE
    )
  )

  .volt_new(list(
    project = project,
    assumptions = assumptions,
    agora_connection = agora_connection,
    meetings = meetings,
    facilities = facilities,
    population_frame = population_frame,
    municipality_population = municipality_population,
    sample_formula = sample_formula,
    sample_plan = sample_plan,
    municipality_sample_plan = municipality_sample_plan,
    sample_check = sample_check,
    municipality_sample_check = municipality_sample_check,
    briefing = briefing,
    questionnaire = questionnaire,
    participants = participants,
    participant_summary = participant_summary,
    event_rankings = event_rankings,
    progressive_rankings = progressive_rankings,
    final_progressive_ranking = final_progressive_ranking,
    alternatives = alternatives,
    clusters = clusters,
    indicators = indicators,
    indicator_performance = indicator_performance,
    ranking = ranking,
    final_facility_ranking = final_facility_ranking,
    decision = decision,
    narrative_report = narrative_report,
    report_sections = report_sections
  ), "volt_valtaro_valceno_demo_project")
}

#' Create an empty VOLT template
#'
#' @param type Template type.
#' @return Data frame template.
#' @export
volt_template <- function(type = c("alternatives", "criteria", "performance",
                                   "clusters", "indicators", "indicator_performance")) {
  type <- match.arg(type)
  if (identical(type, "alternatives")) {
    return(data.frame(
      alternative_id = "ALT_001",
      label = "Nome alternativa",
      faro_status = "ADMISSIBLE",
      notes = "Motivazione o nota FARO",
      stringsAsFactors = FALSE
    ))
  }
  if (identical(type, "criteria")) {
    return(data.frame(
      criterion_id = "criterion_001",
      label = "Nome criterio",
      direction = "max",
      weight = 1,
      unit = "",
      description = "Descrizione criterio",
      stringsAsFactors = FALSE
    ))
  }
  if (identical(type, "clusters")) {
    return(data.frame(
      cluster_id = "C1",
      cluster_name = "Nome cluster",
      cluster_weight = 1,
      description = "Descrizione cluster",
      stringsAsFactors = FALSE
    ))
  }
  if (identical(type, "indicators")) {
    return(data.frame(
      cluster_id = "C1",
      indicator_id = "C1_I01",
      indicator_name = "Nome indicatore atomico",
      direction = "max",
      indicator_weight = 1,
      unit = "scala 0-100",
      source = "fonte_DIA",
      validation_status = "PENDING_VERIFICATION",
      description = "Descrizione indicatore",
      stringsAsFactors = FALSE
    ))
  }
  if (identical(type, "indicator_performance")) {
    return(data.frame(
      alternative_id = "ALT_001",
      indicator_id = "C1_I01",
      value = NA_real_,
      source = "fonte_da_compilare",
      evidence_status = "MISSING",
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    alternative_id = "ALT_001",
    criterion_id = "criterion_001",
    value = NA_real_,
    source = "fonte_da_compilare",
    stringsAsFactors = FALSE
  )
}

#' Explain VOLT data imports
#'
#' @return Data frame describing accepted upload types, required columns, and
#'   operational purpose.
#' @export
volt_upload_guide <- function() {
  data.frame(
    destinazione = c("Automatica", "Alternative", "Criteri", "Prestazioni", "Cluster DIA", "Indicatori DIA", "Prestazioni indicatori DIA", "Progetto RDS"),
    quando_usarla = c(
      "Quando il file contiene colonne riconoscibili e vuoi lasciare al sistema la scelta della destinazione.",
      "Quando devi caricare o sostituire l'elenco delle alternative da confrontare.",
      "Quando devi caricare o sostituire criteri, direzioni e pesi.",
      "Quando devi caricare o sostituire i valori delle alternative rispetto ai criteri.",
      "Quando vuoi caricare la struttura dei cinque cluster del modello VOLT/DIA.",
      "Quando vuoi caricare gli indicatori atomici ricevuti o preparati secondo il contratto DIA.",
      "Quando vuoi caricare i valori delle alternative rispetto agli indicatori atomici.",
      "Quando vuoi riaprire un progetto salvato in precedenza come oggetto RDS."
    ),
    colonne_richieste = c(
      "Dipende dal contenuto: alternative_id+label+faro_status, oppure criterion_id+label+direction+weight, oppure alternative_id+criterion_id+value.",
      "alternative_id, label, faro_status. La colonna notes e' facoltativa.",
      "criterion_id, label, direction, weight. Le colonne unit e description sono facoltative.",
      "alternative_id, criterion_id, value. La colonna source e' facoltativa ma raccomandata.",
      "cluster_id, cluster_name, cluster_weight. La colonna description e' facoltativa.",
      "cluster_id, indicator_id, indicator_name, direction, indicator_weight. Fonte e stato di validazione sono raccomandati.",
      "alternative_id, indicator_id, value. Fonte e stato evidenza sono raccomandati.",
      "Oggetto RDS con project, alternatives, criteria, performance, o con struttura DIA."
    ),
    scopo = c(
      "Ridurre errori quando il file e' gia strutturato secondo i template.",
      "Definire il perimetro delle alternative: solo quelle ADMISSIBLE entrano nel ranking.",
      "Definire cosa conta nella valutazione e con quale peso.",
      "Alimentare il calcolo dello score: ogni valore collega una alternativa a un criterio.",
      "Dichiarare i pesi di livello superiore prima del calcolo sugli indicatori.",
      "Rendere visibili indicatori, direzioni, pesi intra-cluster, fonti e validazione.",
      "Alimentare il ranking da indicatori atomici, mostrando anche dati mancanti e fonti incomplete.",
      "Riprendere un lavoro senza ricostruire manualmente fogli e ranking."
    ),
    dopo_il_caricamento = c(
      "Il progetto viene aggiornato e il ranking ricalcolato se la struttura e' valida.",
      "Controlla gli stati FARO, poi premi Ricalcola ranking.",
      "Controlla max/min e pesi, poi premi Ricalcola ranking.",
      "Controlla valori e fonti, poi premi Ricalcola ranking.",
      "Controlla che i cluster rappresentino il modello scelto e premi Ricalcola ranking.",
      "Controlla pesi, direzioni e stati di validazione, poi premi Ricalcola ranking.",
      "Controlla valori mancanti e stati evidenza, poi premi Ricalcola ranking.",
      "Il progetto viene riaperto e il ranking viene ricalcolato."
    ),
    stringsAsFactors = FALSE
  )
}

#' List bundled VOLT upload examples
#'
#' @return Data frame with bundled example files and their purpose.
#' @export
volt_upload_examples <- function() {
  data.frame(
    esempio = c("Alternative", "Criteri", "Prestazioni", "Cluster DIA", "Indicatori DIA", "Prestazioni indicatori DIA"),
    file = c(
      "esempio_alternative.csv",
      "esempio_criteri.csv",
      "esempio_prestazioni.csv",
      "dia/esempio_cluster_dia.csv",
      "dia/esempio_indicatori_dia.csv",
      "dia/esempio_prestazioni_indicatori_dia.csv"
    ),
    destinazione = c("Alternative", "Criteri", "Prestazioni", "Cluster DIA", "Indicatori DIA", "Prestazioni indicatori DIA"),
    contenuto = c(
      "Alternative con stato FARO e note.",
      "Criteri, direzione max/min, pesi, unita e descrizione.",
      "Valori numerici delle alternative rispetto ai criteri.",
      "Cinque cluster dimostrativi con pesi dichiarati.",
      "Indicatori atomici dimostrativi, circa dieci per cluster.",
      "Valori delle alternative rispetto agli indicatori atomici."
    ),
    stringsAsFactors = FALSE
  )
}

#' Locate a bundled VOLT upload example
#'
#' @param file Example file name.
#' @return Installed file path.
#' @export
volt_example_path <- function(file) {
  path <- system.file("examples", "upload", file, package = "voltR")
  if (!nzchar(path)) stop("Example file not found: ", file, call. = FALSE)
  path
}

#' Write bundled DIA-style example CSV files
#'
#' @param path Output directory.
#' @return Data frame with written files.
#' @export
volt_write_dia_examples <- function(path) {
  if (missing(path) || !nzchar(path)) stop("path is required.", call. = FALSE)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  demo <- volt_dia_demo_project()
  files <- data.frame(
    object = c("clusters", "indicators", "indicator_performance"),
    file = file.path(path, c(
      "esempio_cluster_dia.csv",
      "esempio_indicatori_dia.csv",
      "esempio_prestazioni_indicatori_dia.csv"
    )),
    stringsAsFactors = FALSE
  )
  utils::write.csv(as.data.frame(demo$clusters), files$file[1], row.names = FALSE)
  utils::write.csv(as.data.frame(demo$indicators), files$file[2], row.names = FALSE)
  utils::write.csv(as.data.frame(demo$indicator_performance), files$file[3], row.names = FALSE)
  files
}

#' Save a VOLT project bundle
#'
#' @param project VOLT project bundle.
#' @param path Output RDS path.
#' @return The saved path.
#' @export
volt_save_project <- function(project, path) {
  if (!is.list(project)) stop("project must be a VOLT project bundle.", call. = FALSE)
  saveRDS(project, path)
  path
}

#' Open a saved VOLT project bundle
#'
#' @param path RDS path.
#' @return VOLT project bundle with recalculated ranking.
#' @export
volt_open_project <- function(path) {
  object <- readRDS(path)
  if (!is.list(object) || !("project" %in% names(object)) || !("alternatives" %in% names(object))) {
    stop("Saved object is not a VOLT project bundle.", call. = FALSE)
  }
  if (all(c("clusters", "indicators", "indicator_performance") %in% names(object))) {
    return(volt_update_dia_project(object))
  }
  if (!all(c("criteria", "performance") %in% names(object))) {
    stop("Saved object is not a supported VOLT project bundle.", call. = FALSE)
  }
  volt_update_project(object)
}

#' Update a VOLT demo/project bundle
#'
#' @param project A list containing `project`, `alternatives`, `criteria`, and
#'   `performance`.
#' @param alternatives Optional replacement alternatives table.
#' @param criteria Optional replacement criteria table.
#' @param performance Optional replacement performance matrix.
#' @return Updated project bundle with recalculated ranking.
#' @export
volt_update_project <- function(project,
                                alternatives = NULL,
                                criteria = NULL,
                                performance = NULL) {
  if (!is.list(project)) stop("project must be a VOLT project bundle.", call. = FALSE)
  out <- project
  if (!is.null(alternatives)) out$alternatives <- volt_alternatives(alternatives)
  if (!is.null(criteria)) out$criteria <- volt_criteria(criteria)
  if (!is.null(performance)) out$performance <- volt_performance(performance)
  out$ranking <- volt_rank(out$alternatives, out$criteria, out$performance)
  class(out) <- unique(c(class(project), "volt_demo_project", "list"))
  out
}

#' Import a local file for VOLT
#'
#' @param path Path to a CSV, RDS, RDA, or RData file.
#' @return Imported R object.
#' @export
volt_import_file <- function(path) {
  if (missing(path) || is.null(path) || !file.exists(path)) {
    stop("Input file does not exist.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "csv")) {
    return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (identical(ext, "rds")) return(readRDS(path))
  if (ext %in% c("rda", "rdata")) {
    env <- new.env(parent = emptyenv())
    nm <- load(path, envir = env)
    if (!length(nm)) stop("No object found in data file.", call. = FALSE)
    return(env[[nm[1]]])
  }
  stop("Unsupported file type. Use CSV, RDS, RDA, or RData.", call. = FALSE)
}

#' Apply imported data to a VOLT project bundle
#'
#' @param project VOLT project bundle.
#' @param object Imported object.
#' @param target One of `auto`, `alternatives`, `criteria`, or `performance`.
#' @return Updated project bundle.
#' @export
volt_apply_import <- function(project, object,
                              target = c("auto", "alternatives", "criteria", "performance")) {
  target <- match.arg(target)
  if (is.list(object) && all(c("project", "alternatives", "criteria", "performance") %in% names(object))) {
    return(volt_update_project(object))
  }
  if (target == "auto") {
    nms <- names(as.data.frame(object))
    if (all(c("alternative_id", "label", "faro_status") %in% nms)) target <- "alternatives"
    else if (all(c("criterion_id", "label", "direction", "weight") %in% nms)) target <- "criteria"
    else if (all(c("alternative_id", "criterion_id", "value") %in% nms)) target <- "performance"
    else stop("Cannot infer imported data type. Choose a target explicitly.", call. = FALSE)
  }
  if (identical(target, "alternatives")) return(volt_update_project(project, alternatives = object))
  if (identical(target, "criteria")) return(volt_update_project(project, criteria = object))
  if (identical(target, "performance")) return(volt_update_project(project, performance = object))
  project
}

#' Update a DIA-aware VOLT project bundle
#'
#' @param project A list containing DIA-aware VOLT objects.
#' @param alternatives Optional replacement alternatives.
#' @param clusters Optional replacement clusters.
#' @param indicators Optional replacement indicators.
#' @param indicator_performance Optional replacement atomic indicator performance.
#' @return Updated DIA-aware project bundle.
#' @export
volt_update_dia_project <- function(project,
                                    alternatives = NULL,
                                    clusters = NULL,
                                    indicators = NULL,
                                    indicator_performance = NULL) {
  if (!is.list(project)) stop("project must be a DIA-aware VOLT project bundle.", call. = FALSE)
  out <- project
  if (!is.null(alternatives)) out$alternatives <- volt_alternatives(alternatives)
  if (!is.null(clusters)) out$clusters <- volt_cluster_plan(clusters)
  if (!is.null(indicators)) out$indicators <- volt_indicator_registry(indicators)
  if (!is.null(indicator_performance)) out$indicator_performance <- volt_indicator_performance(indicator_performance)
  if (!all(c("alternatives", "clusters", "indicators", "indicator_performance") %in% names(out))) {
    stop("DIA-aware project must contain alternatives, clusters, indicators and indicator_performance.", call. = FALSE)
  }
  out$ranking <- volt_rank_dia(out$alternatives, out$clusters, out$indicators, out$indicator_performance)
  class(out) <- unique(c(class(project), "volt_dia_demo_project", "list"))
  out
}

#' Apply imported data to a DIA-aware VOLT project bundle
#'
#' @param project DIA-aware VOLT project bundle.
#' @param object Imported object.
#' @param target One of `auto`, `alternatives`, `clusters`, `indicators`, or
#'   `indicator_performance`.
#' @return Updated DIA-aware project bundle.
#' @export
volt_apply_dia_import <- function(project, object,
                                  target = c("auto", "alternatives", "clusters", "indicators", "indicator_performance")) {
  target <- match.arg(target)
  if (is.list(object) && all(c("project", "alternatives", "clusters", "indicators", "indicator_performance") %in% names(object))) {
    return(volt_update_dia_project(object))
  }
  if (target == "auto") {
    nms <- names(as.data.frame(object))
    if (all(c("cluster_id", "cluster_name", "cluster_weight") %in% nms)) target <- "clusters"
    else if (all(c("cluster_id", "indicator_id", "indicator_name", "direction", "indicator_weight") %in% nms)) target <- "indicators"
    else if (all(c("alternative_id", "indicator_id", "value") %in% nms)) target <- "indicator_performance"
    else if (all(c("alternative_id", "label", "faro_status") %in% nms)) target <- "alternatives"
    else stop("Cannot infer imported DIA data type. Choose a target explicitly.", call. = FALSE)
  }
  if (identical(target, "alternatives")) return(volt_update_dia_project(project, alternatives = object))
  if (identical(target, "clusters")) return(volt_update_dia_project(project, clusters = object))
  if (identical(target, "indicators")) return(volt_update_dia_project(project, indicators = object))
  if (identical(target, "indicator_performance")) return(volt_update_dia_project(project, indicator_performance = object))
  project
}

#' VOLT formula reference table
#'
#' @return Data frame with formula steps used by `volt_rank()`.
#' @export
volt_formula_reference <- function() {
  data.frame(
    passaggio = c(
      "Filtro FARO",
      "Normalizzazione pesi",
      "Normalizzazione criterio max",
      "Normalizzazione criterio min",
      "Contributo ponderato",
      "Score alternativa",
      "Ranking"
    ),
    formula = c(
      "usa solo faro_status == 'ADMISSIBLE'",
      "weight / sum(weight)",
      "(x - min(x)) / (max(x) - min(x))",
      "(max(x) - x) / (max(x) - min(x))",
      "normalized_value * normalized_weight",
      "sum(weighted_contribution) per alternative_id",
      "ordine decrescente dello score"
    ),
    lettura = c(
      "Le alternative non ammissibili restano tracciate ma non entrano nel ranking.",
      "I pesi inseriti possono non sommare a 1: il motore li normalizza in modo esplicito.",
      "Per criteri da massimizzare, valori maggiori sono preferiti.",
      "Per criteri da minimizzare, valori minori sono preferiti.",
      "Ogni criterio contribuisce secondo peso e prestazione normalizzata.",
      "Lo score e' un supporto comparativo, non una decisione automatica.",
      "Il ranking dipende da criteri, pesi, dati e filtro FARO dichiarati."
    ),
    stringsAsFactors = FALSE
  )
}

#' Build plain-language report tables for VOLT
#'
#' @param ranking A `volt_ranking`.
#' @param project Optional `volt_project`.
#' @return A list of data frames.
#' @export
volt_report_pack <- function(ranking, project = NULL) {
  if (!inherits(ranking, "volt_ranking")) stop("ranking must be a volt_ranking.", call. = FALSE)
  decision_note <- if (nrow(ranking$result)) {
    paste("La prima alternativa classificata e'", ranking$result$alternative_id[1],
      "con score", signif(ranking$result$score[1], 4), ".")
  } else {
    "Nessuna alternativa ammissibile e' stata classificata."
  }
  project_title <- if (inherits(project, "volt_project")) project$title else "Progetto VOLT"
  limitations <- data.frame(
    limite = c(
      "Perimetro post-FARO",
      "Dipendenza da pesi e prestazioni",
      "Nessuna decisione automatica",
      "Fonti da verificare"
    ),
    lettura = c(
      "VOLT classifica solo alternative gia dichiarate ammissibili: non sostituisce il filtro FARO.",
      "Il ranking cambia se cambiano criteri, pesi, valori o fonti.",
      "La prima alternativa classificata supporta la discussione, ma la decisione resta un atto motivato.",
      "La demo usa dati sintetici; in un progetto reale ogni valore deve avere fonte e versione."
    ),
    stringsAsFactors = FALSE
  )
  final_report <- data.frame(
    sezione = c(
      "1. Oggetto del confronto",
      "2. Alternative considerate",
      "3. Criteri e pesi",
      "4. Prestazioni e fonti",
      "5. Esito del ranking",
      "6. Alternative escluse",
      "7. Limiti e decisione"
    ),
    testo = c(
      paste("Il progetto", project_title, "usa VOLT per confrontare alternative ammissibili dopo FARO."),
      paste(nrow(ranking$admitted), "alternative risultano ammissibili e", nrow(ranking$excluded), "restano escluse dal ranking ma tracciate."),
      paste(nrow(ranking$criteria), "criteri sono applicati con direzione max/min e pesi normalizzati."),
      paste(nrow(ranking$performance), "valori di performance alimentano il calcolo; le fonti devono essere controllate nei progetti reali."),
      decision_note,
      if (nrow(ranking$excluded)) paste("Alternative escluse:", paste(ranking$excluded$alternative_id, collapse = ", ")) else "Nessuna alternativa esclusa.",
      ranking$language_guard
    ),
    stringsAsFactors = FALSE
  )
  provenance <- data.frame(
    campo = c("metodo", "generato_il", "classificazione_demo", "guardrail"),
    valore = c(
      ranking$method,
      as.character(ranking$generated_at),
      "I dati demo sono sintetici.",
      ranking$language_guard
    ),
    stringsAsFactors = FALSE
  )
  list(
    overview = data.frame(
      elemento = c("Scopo", "Evidenza", "Lettura"),
      testo = c(
        "Ordinare e confrontare solo alternative dichiarate ammissibili dopo FARO.",
        decision_note,
        ranking$language_guard
      ),
      stringsAsFactors = FALSE
    ),
    result = ranking$result,
    excluded = ranking$excluded,
    audit = ranking$audit,
    criteria = ranking$criteria,
    performance = ranking$performance,
    contributions = ranking$contributions,
    formulas = volt_formula_reference(),
    limitations = limitations,
    final_report = final_report,
    provenance = provenance
  )
}

#' Build plain-language report tables for DIA-aware VOLT
#'
#' @param ranking A `volt_dia_ranking`.
#' @param project Optional `volt_project`.
#' @return A list of data frames.
#' @export
volt_dia_report_pack <- function(ranking, project = NULL) {
  if (!inherits(ranking, "volt_dia_ranking")) stop("ranking must be a volt_dia_ranking.", call. = FALSE)
  project_title <- if (inherits(project, "volt_project")) project$title else "Progetto VOLT/DIA"
  decision_note <- if (nrow(ranking$result)) {
    paste("La prima alternativa classificata e'", ranking$result$alternative_id[1],
      "con score", signif(ranking$result$score[1], 4), ".")
  } else {
    "Nessuna alternativa FARO-ammissibile e' stata classificata."
  }
  cluster_summary <- ranking$cluster_scores[order(ranking$cluster_scores$alternative_id, ranking$cluster_scores$cluster_id), , drop = FALSE]
  limitations <- data.frame(
    limite = c(
      "Confine FARO",
      "Ruolo DIA",
      "Completezza dati",
      "Sensibilita ai pesi",
      "Nessuna decisione automatica"
    ),
    lettura = c(
      "VOLT usa solo alternative gia ammissibili o dichiarate tali dal passaggio FARO.",
      "DIA resta il registro degli indicatori atomici: VOLT consuma indicatori, direzioni, pesi e stati di evidenza.",
      "Valori mancanti o fonti non validate restano visibili in audit e completezza.",
      "La sensibilita mostra se piccoli cambi di peso cluster alterano il primo classificato; non e' una prova di robustezza definitiva.",
      "Il ranking supporta il confronto, ma la decisione pubblica deve essere motivata e documentata."
    ),
    stringsAsFactors = FALSE
  )
  final_report <- data.frame(
    sezione = c(
      "1. Oggetto del confronto",
      "2. Alternative e confine FARO",
      "3. Struttura a cluster",
      "4. Indicatori atomici DIA",
      "5. Prestazioni, fonti e completezza",
      "6. Esito del ranking",
      "7. Lettura per cluster",
      "8. Sensibilita dichiarata",
      "9. Limiti e decisione"
    ),
    testo = c(
      paste("Il progetto", project_title, "confronta alternative energetiche o territoriali mediante indicatori atomici organizzati in cluster."),
      paste(nrow(ranking$admitted), "alternative sono FARO-ammissibili e", nrow(ranking$excluded), "sono escluse o in attesa."),
      paste(nrow(ranking$clusters), "cluster sono applicati con pesi dichiarati e normalizzati in modo esplicito."),
      paste(nrow(ranking$indicators), "indicatori atomici alimentano il calcolo; ciascuno conserva direzione, peso intra-cluster, fonte e validazione."),
      paste(sum(ranking$completeness$missing_values), "valori risultano mancanti tra alternative ammissibili e indicatori."),
      decision_note,
      "Le tabelle per cluster mostrano quali dimensioni guidano lo score finale di ciascuna alternativa.",
      paste(sum(ranking$sensitivity$top_changed), "scenario/i di sensibilita cambiano il primo classificato con il delta dichiarato."),
      ranking$language_guard
    ),
    stringsAsFactors = FALSE
  )
  provenance <- data.frame(
    campo = c("metodo", "generato_il", "classificazione_demo", "confine_faro", "handoff_dia", "guardrail"),
    valore = c(
      ranking$method,
      as.character(ranking$generated_at),
      "I dati demo sono sintetici.",
      "FARO decide l'ammissibilita; VOLT filtra soltanto faro_status.",
      "DIA fornisce indicatori atomici, pesi, direzioni, fonti e stati di evidenza.",
      ranking$language_guard
    ),
    stringsAsFactors = FALSE
  )
  list(
    overview = data.frame(
      elemento = c("Scopo", "Evidenza", "Lettura"),
      testo = c(
        "Confrontare alternative FARO-ammissibili usando indicatori atomici DIA organizzati in cluster.",
        decision_note,
        ranking$language_guard
      ),
      stringsAsFactors = FALSE
    ),
    result = ranking$result,
    excluded = ranking$excluded,
    audit = ranking$audit,
    clusters = ranking$clusters,
    indicators = ranking$indicators,
    indicator_performance = ranking$performance,
    indicator_contributions = ranking$indicator_contributions,
    cluster_scores = cluster_summary,
    completeness = ranking$completeness,
    sensitivity = ranking$sensitivity,
    limitations = limitations,
    final_report = final_report,
    provenance = provenance
  )
}
