source_group_split <- function(cases, test_fraction, seed) {
  source_key <- sub(
    "/+$",
    "",
    sub("[?#].*$", "", trimws(cases$source_url))
  )
  groups <- match(source_key, unique(source_key))
  group_ids <- unique(groups)
  group_labels <- vapply(group_ids, function(group_id) {
    labels <- unique(cases$condition_label[groups == group_id])
    if (length(labels) != 1L) {
      stop(
        "A source URL has multiple condition labels. Split mixed-diagnosis ",
        "pages into separate, independently sourced cases before evaluation.",
        call. = FALSE
      )
    }
    labels[[1]]
  }, character(1))

  set.seed(seed)
  train <- integer()
  test <- integer()
  for (label in unique(group_labels)) {
    label_groups <- sample(which(group_labels == label))
    if (length(label_groups) < 2L) {
      next
    }
    test_count <- max(1L, floor(length(label_groups) * test_fraction))
    test_count <- min(test_count, length(label_groups) - 1L)
    heldout_groups <- label_groups[seq_len(test_count)]
    training_groups <- label_groups[(test_count + 1L):length(label_groups)]
    test <- c(test, which(groups %in% heldout_groups))
    train <- c(train, which(groups %in% training_groups))
  }
  list(train = sort(train), test = sort(test))
}

clear_evaluation_outputs <- function(config) {
  stale_files <- c(
    file.path(config$models_dir, "06_final_plant_health_classifier.rds"),
    file.path(config$models_dir, "06_verified_cluster_mapping.rds"),
    file.path(config$tables_dir, "06_verified_case_counts.csv"),
    file.path(config$tables_dir, "06_calibration_cases.csv"),
    file.path(config$tables_dir, "06_all_verified_case_metrics.csv"),
    file.path(config$tables_dir, "06_all_verified_case_predictions.csv"),
    file.path(config$tables_dir, "06_cluster_condition_mapping.csv"),
    file.path(config$tables_dir, "06_classification_metrics.csv"),
    file.path(config$tables_dir, "06_per_class_metrics.csv"),
    file.path(config$tables_dir, "06_confusion_matrix.csv"),
    file.path(config$tables_dir, "06_test_predictions.csv"),
    file.path(config$plots_dir, "06_per_class_f1_data.csv"),
    file.path(config$plots_dir, "06_per_class_f1.png")
  )
  existing <- stale_files[file.exists(stale_files)]
  if (length(existing)) {
    unlink(existing)
  }
}

assign_to_kmeans_cluster <- function(text, feature_model, kmeans_model) {
  iterator <- text2vec::itoken(
    text,
    tokenizer = text2vec::word_tokenizer,
    progressbar = FALSE
  )
  document_term_matrix <- text2vec::create_dtm(
    iterator,
    feature_model$vectorizer
  )
  tfidf_matrix <- feature_model$tfidf$transform(document_term_matrix)
  coordinates <- as.matrix(tfidf_matrix %*% feature_model$svd$v)
  row_norms <- sqrt(rowSums(coordinates^2))
  unprojectable <- row_norms == 0
  row_norms[unprojectable] <- 1
  coordinates <- coordinates / row_norms

  squared_distances <- do.call(
    cbind,
    lapply(seq_len(nrow(kmeans_model$centers)), function(i) {
      residual <- sweep(coordinates, 2, kmeans_model$centers[i, ], `-`)
      rowSums(residual^2)
    })
  )
  clusters <- max.col(-squared_distances, ties.method = "first")
  clusters[unprojectable] <- NA_integer_
  clusters
}

evaluate_verified_cluster_mapping <- function(config) {
  path <- config$verified_cases_file
  if (!file.exists(path)) {
    stop("Verified-case file is missing: ", path, call. = FALSE)
  }
  cases <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  required <- c(
    "case_id", "symptom_text", "condition_label", "source_name",
    "source_url", "verified", "verified_by", "verified_on", "evidence_type"
  )
  missing <- setdiff(required, names(cases))
  if (length(missing)) {
    stop(
      "Verified-case file is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(cases) == 0L) {
    clear_evaluation_outputs(config)
    message("No trusted-source cases were added; cluster-label evaluation was skipped.")
    return(invisible(NULL))
  }
  cases <- cases |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(required),
        ~ dplyr::coalesce(as.character(.x), "")
      )
    )
  verified_rows <- tolower(trimws(cases$verified)) == "yes"
  message(sprintf(
    "Using %d verified rows; excluding %d unverified or synthetic rows.",
    sum(verified_rows),
    sum(!verified_rows)
  ))
  cases <- cases[verified_rows, , drop = FALSE]
  if (nrow(cases) == 0L) {
    clear_evaluation_outputs(config)
    message(
      "No rows have verified=yes; cluster-label evaluation was skipped. ",
      "Unverified entries are not used."
    )
    return(invisible(NULL))
  }
  cases$evidence_type <- tolower(trimws(cases$evidence_type))
  supported_types <- c("fact_sheet_paraphrase", "confirmed_case")
  invalid_type <- !cases$evidence_type %in% supported_types
  if (any(invalid_type)) {
    stop(
      "Unsupported evidence_type. Use fact_sheet_paraphrase or confirmed_case.",
      call. = FALSE
    )
  }
  if (length(unique(cases$evidence_type)) > 1L) {
    stop(
      "Do not mix fact-sheet paraphrases and confirmed cases in one evaluation. ",
      "Evaluate each evidence type separately to keep the metric interpretable.",
      call. = FALSE
    )
  }
  evidence_type <- unique(cases$evidence_type)

  text_fields <- c("case_id", "symptom_text", "condition_label", "source_name",
                   "source_url", "verified_by", "verified_on")
  invalid <- !stats::complete.cases(cases[, text_fields, drop = FALSE]) |
    !nzchar(trimws(cases$symptom_text)) |
    !nzchar(trimws(cases$condition_label)) |
    !nzchar(trimws(cases$source_name)) |
    !grepl("^https://", trimws(cases$source_url), ignore.case = TRUE) |
    !nzchar(trimws(cases$verified_by)) |
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", trimws(cases$verified_on)) |
    is.na(as.Date(cases$verified_on, format = "%Y-%m-%d")) |
    !nzchar(trimws(cases$case_id))
  if (any(invalid)) {
    stop(
      sum(invalid),
      " verified row(s) lack symptom text, a condition, source provenance, ",
      "verifier, or verification date. Correct those rows before training.",
      call. = FALSE
    )
  }
  if (anyDuplicated(cases$case_id)) {
    stop("Verified cases contain duplicate case_id values.", call. = FALSE)
  }
  cases$text <- normalize_text(cases$symptom_text)
  cases <- cases[nzchar(cases$text), , drop = FALSE]
  cases$source_page_key <- sub(
    "/+$",
    "",
    sub("[?#].*$", "", trimws(cases$source_url))
  )
  if (anyDuplicated(cases$text)) {
    stop(
      "Verified cases contain identical normalized symptom text. Deduplicate ",
      "or revise the records before splitting training and test data.",
      call. = FALSE
    )
  }

  counts <- cases |>
    dplyr::group_by(condition_label) |>
    dplyr::summarise(
      examples = dplyr::n(),
      source_pages = dplyr::n_distinct(source_page_key),
      .groups = "drop"
    )
  readr::write_csv(
    counts,
    file.path(config$tables_dir, "06_verified_case_counts.csv")
  )
  eligible <- counts |>
    dplyr::filter(
      examples >= config$min_verified_examples_per_class,
      source_pages >= 2L
    ) |>
    dplyr::pull(condition_label)
  cases <- dplyr::filter(cases, condition_label %in% eligible)
  if (length(eligible) < 2L) {
    clear_evaluation_outputs(config)
    message(
      "Cluster-label evaluation needs at least two conditions with ",
      config$min_verified_examples_per_class,
      " verified cases from at least two source pages each."
    )
    return(invisible(NULL))
  }

  split <- source_group_split(
    cases,
    config$test_fraction,
    config$random_seed
  )
  if (!length(split$train) || !length(split$test)) {
    clear_evaluation_outputs(config)
    message(
      "No source-isolated test split could be made. Add verified examples ",
      "from more distinct source pages per condition."
    )
    return(invisible(NULL))
  }
  calibration <- cases[split$train, , drop = FALSE]
  testing <- cases[split$test, , drop = FALSE]
  if (length(intersect(calibration$source_page_key, testing$source_page_key))) {
    stop("Calibration and test source pages overlap.", call. = FALSE)
  }
  feature_model_path <- file.path(
    config$models_dir,
    "03_unsupervised_text_feature_model.rds"
  )
  cluster_model_path <- file.path(config$models_dir, "04_kmeans_model.rds")
  if (!file.exists(feature_model_path) || !file.exists(cluster_model_path)) {
    stop(
      "Run the unlabeled clustering pipeline before evaluating verified cases.",
      call. = FALSE
    )
  }
  feature_model <- readRDS(feature_model_path)
  cluster_model <- readRDS(cluster_model_path)
  calibration$cluster_id <- assign_to_kmeans_cluster(
    calibration$text,
    feature_model,
    cluster_model
  )
  testing$cluster_id <- assign_to_kmeans_cluster(
    testing$text,
    feature_model,
    cluster_model
  )
  readr::write_csv(
    calibration |>
      dplyr::select(case_id, source_url, condition_label, cluster_id),
    file.path(config$tables_dir, "06_calibration_cases.csv")
  )

  cluster_mapping <- calibration |>
    dplyr::filter(!is.na(cluster_id)) |>
    dplyr::count(cluster_id, condition_label, name = "calibration_examples") |>
    dplyr::group_by(cluster_id) |>
    dplyr::arrange(
      dplyr::desc(calibration_examples),
      condition_label,
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      cluster_total = sum(calibration_examples),
      cluster_purity = calibration_examples / cluster_total,
      mapping_rank = dplyr::row_number()
    ) |>
    dplyr::filter(mapping_rank == 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      cluster_id,
      predicted_condition = condition_label,
      calibration_examples,
      cluster_total,
      cluster_purity
    )
  readr::write_csv(
    cluster_mapping,
    file.path(config$tables_dir, "06_cluster_condition_mapping.csv")
  )

  predictions <- testing |>
    dplyr::left_join(cluster_mapping, by = "cluster_id") |>
    dplyr::mutate(
      predicted = dplyr::coalesce(predicted_condition, "Unmapped cluster")
    )
  all_cases <- cases
  all_cases$cluster_id <- assign_to_kmeans_cluster(
    all_cases$text,
    feature_model,
    cluster_model
  )
  all_predictions <- all_cases |>
    dplyr::left_join(cluster_mapping, by = "cluster_id") |>
    dplyr::mutate(
      predicted = dplyr::coalesce(predicted_condition, "Unmapped cluster"),
      evaluation_role = dplyr::if_else(
        case_id %in% calibration$case_id,
        "calibration (in-sample)",
        "held-out test"
      )
    )
  all_metrics <- tibble::tibble(
    metric = c(
      "All verified examples",
      "Mapped coverage",
      "Apparent accuracy (includes calibration cases)",
      "Balanced apparent accuracy"
    ),
    value = c(
      nrow(all_predictions),
      mean(all_predictions$predicted != "Unmapped cluster"),
      mean(all_predictions$predicted == all_predictions$condition_label),
      mean(vapply(sort(unique(all_predictions$condition_label)), function(label) {
        rows <- all_predictions$condition_label == label
        mean(all_predictions$predicted[rows] == label)
      }, numeric(1)))
    )
  )
  readr::write_csv(
    all_metrics,
    file.path(config$tables_dir, "06_all_verified_case_metrics.csv")
  )
  readr::write_csv(
    all_predictions |>
      dplyr::transmute(
        case_id,
        evaluation_role,
        source_url,
        actual = condition_label,
        cluster_id,
        predicted_condition,
        predicted,
        correct = predicted == condition_label
      ),
    file.path(config$tables_dir, "06_all_verified_case_predictions.csv")
  )
  class_levels <- sort(unique(cases$condition_label))
  actual <- testing$condition_label
  predicted <- predictions$predicted
  confusion <- table(
    actual = factor(actual, levels = class_levels),
    predicted = factor(
      predicted,
      levels = c(class_levels, "Unmapped cluster")
    )
  )

  per_class <- dplyr::bind_rows(lapply(class_levels, function(label) {
    true_positive <- confusion[label, label]
    predicted_count <- sum(confusion[, label])
    actual_count <- sum(confusion[label, ])
    precision <- if (predicted_count) true_positive / predicted_count else 0
    recall <- if (actual_count) true_positive / actual_count else 0
    tibble::tibble(
      condition_label = label,
      support = actual_count,
      precision = precision,
      recall = recall,
      f1 = if (precision + recall) {
        2 * precision * recall / (precision + recall)
      } else {
        0
      }
    )
  }))
  metrics <- tibble::tibble(
    metric = c(
      "Test examples",
      "Calibration examples",
      "Mapped test coverage",
      "Accuracy",
      "Balanced accuracy",
      "Macro precision",
      "Macro recall",
      "Macro F1"
    ),
    value = c(
      nrow(testing),
      nrow(calibration),
      mean(predicted != "Unmapped cluster"),
      mean(predicted == actual),
      mean(per_class$recall),
      mean(per_class$precision),
      mean(per_class$recall),
      mean(per_class$f1)
    )
  )

  readr::write_csv(
    metrics,
    file.path(config$tables_dir, "06_classification_metrics.csv")
  )
  readr::write_csv(
    per_class,
    file.path(config$tables_dir, "06_per_class_metrics.csv")
  )
  readr::write_csv(
    as.data.frame(confusion),
    file.path(config$tables_dir, "06_confusion_matrix.csv")
  )
  readr::write_csv(
    tibble::tibble(
      case_id = testing$case_id,
      cluster_id = predictions$cluster_id,
      cluster_condition = predictions$predicted_condition,
      source_url = testing$source_url,
      actual,
      predicted
    ),
    file.path(config$tables_dir, "06_test_predictions.csv")
  )

  saveRDS(
    list(
      cluster_mapping = cluster_mapping,
      categories = class_levels,
      calibration_case_ids = calibration$case_id,
      calibration_source_urls = calibration$source_url,
      test_case_ids = testing$case_id,
      test_source_urls = testing$source_url,
      metrics = metrics,
      evidence_type = evidence_type
    ),
    file.path(config$models_dir, "06_verified_cluster_mapping.rds")
  )
  readr::write_csv(
    per_class,
    file.path(config$plots_dir, "06_per_class_f1_data.csv")
  )
  plot <- ggplot2::ggplot(
    per_class,
    ggplot2::aes(x = reorder(condition_label, f1), y = f1)
  ) +
    ggplot2::geom_col(fill = "#327a65") +
    ggplot2::coord_flip(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Held-out F1 by verified condition",
      x = NULL,
      y = "F1"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(
    file.path(config$plots_dir, "06_per_class_f1.png"),
    plot,
    width = 8,
    height = 5,
    dpi = 150
  )
  message("Unlabeled-cluster mapping on source-held-out verified cases:")
  print(as.data.frame(metrics))
  message(
    "All verified-case predictions (apparent score includes calibration cases; ",
    "not an independent performance estimate):"
  )
  print(as.data.frame(all_metrics))
  if (nrow(testing) < 30L) {
    warning(
      "Fewer than 30 source-isolated test cases are available; metrics are ",
      "preliminary and have wide uncertainty.",
      call. = FALSE
    )
  }
  invisible(list(
    metrics = metrics,
    per_class = per_class,
    confusion = confusion,
    verified_count = nrow(cases),
    calibration_count = nrow(calibration),
    mapped_coverage = mean(predicted != "Unmapped cluster"),
    all_case_metrics = all_metrics,
    all_case_predictions = all_predictions,
    excluded_count = sum(!verified_rows),
    evidence_type = evidence_type,
    categories = sort(unique(cases$condition_label))
  ))
}
