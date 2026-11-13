choose_two <- function(values) {
  values * (values - 1) / 2
}

evaluate_topic_svm <- function(config) {
  review_path <- config$topic_review_file
  corpus_path <- file.path(config$data_dir, "02_cleaned_health_candidates.csv")
  for (path in c(review_path, corpus_path)) {
    if (!file.exists(path)) {
      stop("Required SVM evaluation file is missing: ", path, call. = FALSE)
    }
  }
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("Install the e1071 package to run the SVM evaluation.", call. = FALSE)
  }

  review <- readr::read_csv(review_path, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(
      question_id = as.character(question_id),
      human_topic_label = trimws(as.character(human_topic_label)),
      review_status = tolower(trimws(as.character(review_status))),
      source_url = trimws(as.character(source_url))
    )
  corpus <- readr::read_csv(corpus_path, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(
      question_id = as.character(question_id),
      link = trimws(as.character(link))
    )
  required_review <- c(
    "question_id", "human_topic_label", "review_status", "source_url"
  )
  required_corpus <- c("question_id", "text", "link")
  if (!all(required_review %in% names(review)) ||
      !all(required_corpus %in% names(corpus))) {
    stop("The reviewed labels or cleaned corpus are missing required columns.",
         call. = FALSE)
  }
  if (anyDuplicated(review$question_id) || anyDuplicated(corpus$question_id)) {
    stop("Question IDs must be unique in the review and corpus files.",
         call. = FALSE)
  }
  review <- review |>
    dplyr::filter(
      !is.na(human_topic_label),
      nzchar(human_topic_label),
      !is.na(review_status),
      review_status == "reviewed"
    )
  if (!nrow(review)) {
    stop("The topic-review file has no completed human labels.", call. = FALSE)
  }
  label_counts <- table(review$human_topic_label)
  if (any(label_counts < 2L)) {
    stop(
      "Every human topic needs at least two records for a train/test split. ",
      "Rare labels: ",
      paste(names(label_counts)[label_counts < 2L], collapse = ", "),
      call. = FALSE
    )
  }

  labeled <- dplyr::inner_join(
    review,
    corpus |>
      dplyr::select(
        question_id,
        text,
        corpus_source_url = link
      ),
    by = "question_id"
  )
  if (nrow(labeled) != nrow(review)) {
    stop("Some reviewed IDs are missing from the cleaned corpus.",
         call. = FALSE)
  }
  if (any(labeled$source_url != labeled$corpus_source_url)) {
    stop("Reviewed source URLs do not match the cleaned corpus.",
         call. = FALSE)
  }

  set.seed(config$random_seed)
  test_indices <- unlist(lapply(split(
    seq_len(nrow(labeled)),
    labeled$human_topic_label
  ), function(indices) {
    test_count <- max(1L, floor(length(indices) * config$test_fraction))
    test_count <- min(test_count, length(indices) - 1L)
    sample(indices, test_count)
  }), use.names = FALSE)
  test_indices <- sort(test_indices)
  train_indices <- setdiff(seq_len(nrow(labeled)), test_indices)
  training <- labeled[train_indices, , drop = FALSE]
  testing <- labeled[test_indices, , drop = FALSE]
  if (!setequal(unique(training$human_topic_label),
                unique(labeled$human_topic_label))) {
    stop("At least one label is absent from the SVM training split.",
         call. = FALSE)
  }

  svm_config <- config
  svm_config$min_term_count <- 1L
  svm_config$svd_components <- config$classifier_svd_components
  features <- fit_text_features(training$text, svm_config)
  train_nonempty <- Matrix::rowSums(features$matrix) > 0
  if (any(!train_nonempty)) {
    training <- training[train_nonempty, , drop = FALSE]
    features$matrix <- features$matrix[train_nonempty, , drop = FALSE]
  }
  if (length(unique(training$human_topic_label)) < 2L) {
    stop("SVM training retained fewer than two human topic classes.",
         call. = FALSE)
  }
  set.seed(config$random_seed)
  lsa <- fit_lsa(features$matrix, svm_config)
  classifier <- e1071::svm(
    x = lsa$coordinates,
    y = factor(training$human_topic_label),
    kernel = "linear",
    cost = 1,
    scale = FALSE
  )

  test_iterator <- text2vec::itoken(
    testing$text,
    tokenizer = text2vec::word_tokenizer,
    progressbar = FALSE
  )
  test_dtm <- text2vec::create_dtm(test_iterator, features$vectorizer)
  test_tfidf <- features$tfidf$transform(test_dtm)
  test_coordinates <- as.matrix(test_tfidf %*% lsa$decomposition$v)
  test_norms <- sqrt(rowSums(test_coordinates^2))
  unprojectable <- test_norms == 0
  test_norms[unprojectable] <- 1
  test_coordinates <- test_coordinates / test_norms
  prediction <- as.character(stats::predict(classifier, test_coordinates))
  prediction[unprojectable] <- "Unclassifiable text"

  categories <- sort(unique(labeled$human_topic_label))
  confusion <- table(
    actual = factor(testing$human_topic_label, levels = categories),
    predicted = factor(
      prediction,
      levels = c(categories, "Unclassifiable text")
    )
  )
  per_class <- dplyr::bind_rows(lapply(categories, function(category) {
    true_positive <- confusion[category, category]
    predicted_count <- sum(confusion[, category])
    actual_count <- sum(confusion[category, ])
    precision <- if (predicted_count) true_positive / predicted_count else 0
    recall <- if (actual_count) true_positive / actual_count else 0
    tibble::tibble(
      human_topic_label = category,
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
      "Labeled examples",
      "Training examples",
      "Held-out test examples",
      "Human topic classes",
      "Test coverage",
      "Accuracy",
      "Balanced accuracy",
      "Macro precision",
      "Macro recall",
      "Macro F1"
    ),
    value = c(
      nrow(labeled),
      nrow(training),
      nrow(testing),
      length(categories),
      mean(prediction != "Unclassifiable text"),
      mean(prediction == testing$human_topic_label),
      mean(per_class$recall),
      mean(per_class$precision),
      mean(per_class$recall),
      mean(per_class$f1)
    )
  )
  prediction_output <- tibble::tibble(
    question_id = testing$question_id,
    source_url = testing$source_url,
    actual_topic = testing$human_topic_label,
    predicted_topic = prediction,
    correct = prediction == testing$human_topic_label
  )

  readr::write_csv(
    metrics,
    file.path(config$tables_dir, "09_svm_topic_metrics.csv")
  )
  readr::write_csv(
    per_class,
    file.path(config$tables_dir, "09_svm_per_class_metrics.csv")
  )
  readr::write_csv(
    prediction_output,
    file.path(config$tables_dir, "09_svm_test_predictions.csv")
  )
  readr::write_csv(
    as.data.frame.matrix(confusion) |>
      tibble::rownames_to_column("actual_topic"),
    file.path(config$tables_dir, "09_svm_confusion_matrix.csv")
  )
  saveRDS(
    list(
      classifier = classifier,
      vocabulary = features$vocabulary,
      vectorizer = features$vectorizer,
      tfidf = features$tfidf,
      svd_v = lsa$decomposition$v,
      categories = categories,
      training_question_ids = training$question_id,
      test_question_ids = testing$question_id,
      random_seed = config$random_seed,
      metrics = metrics
    ),
    file.path(config$models_dir, "09_svm_topic_classifier.rds")
  )

  message("Supervised linear SVM evaluation on human topic labels:")
  print(metrics)
  invisible(list(
    metrics = metrics,
    per_class = per_class,
    confusion = confusion,
    predictions = prediction_output,
    training_ids = training$question_id,
    test_ids = testing$question_id
  ))
}

evaluate_holdout_topic_model <- function(config) {
  review_path <- config$topic_review_file
  corpus_path <- file.path(config$data_dir, "02_cleaned_health_candidates.csv")
  for (path in c(review_path, corpus_path)) {
    if (!file.exists(path)) {
      stop("Required holdout-evaluation file is missing: ", path, call. = FALSE)
    }
  }

  review <- readr::read_csv(review_path, show_col_types = FALSE, progress = FALSE)
  corpus <- readr::read_csv(corpus_path, show_col_types = FALSE, progress = FALSE)
  if (!all(c("question_id", "human_topic_label", "review_status", "source_url") %in%
           names(review))) {
    stop("Reviewed topic file is missing required columns.", call. = FALSE)
  }
  if (!all(c("question_id", "text", "link") %in% names(corpus))) {
    stop("Cleaned question corpus is missing required columns.", call. = FALSE)
  }
  review <- review |>
    dplyr::mutate(
      question_id = as.character(question_id),
      human_topic_label = trimws(as.character(human_topic_label)),
      review_status = tolower(trimws(as.character(review_status))),
      source_url = trimws(as.character(source_url))
    )
  corpus <- corpus |>
    dplyr::mutate(
      question_id = as.character(question_id),
      link = trimws(as.character(link))
    )
  if (anyDuplicated(review$question_id) || anyDuplicated(corpus$question_id)) {
    stop("Question IDs must be unique in the review and corpus files.", call. = FALSE)
  }
  review <- review |>
    dplyr::filter(
      !is.na(human_topic_label),
      nzchar(human_topic_label),
      !is.na(review_status),
      review_status == "reviewed"
    )
  if (!nrow(review)) {
    stop("The topic-review file has no completed human labels.", call. = FALSE)
  }
  if (!all(review$question_id %in% corpus$question_id)) {
    stop(
      "Some reviewed question IDs are missing from the cleaned corpus.",
      call. = FALSE
    )
  }

  testing <- dplyr::inner_join(
    review,
    corpus |>
      dplyr::select(
        question_id,
        text,
        corpus_source_url = link
      ),
    by = "question_id"
  )
  if (any(testing$source_url != testing$corpus_source_url)) {
    stop("Reviewed source URLs do not match the cleaned corpus.", call. = FALSE)
  }
  training <- dplyr::filter(corpus, !question_id %in% testing$question_id)
  if (nrow(training) <= config$k_values[[length(config$k_values)]]) {
    stop("Not enough unlabeled training questions remain for clustering.", call. = FALSE)
  }

  message(sprintf(
    "Fitting text features and selecting k using %d non-reviewed questions.",
    nrow(training)
  ))
  features <- fit_text_features(training$text, config)
  train_nonempty <- Matrix::rowSums(features$matrix) > 0
  features$matrix <- features$matrix[train_nonempty, , drop = FALSE]
  training <- training[train_nonempty, , drop = FALSE]
  set.seed(config$random_seed)
  lsa <- fit_lsa(features$matrix, config)
  clustering <- fit_kmeans(lsa$coordinates, config)
  training_cluster_terms <- top_cluster_terms(
    features$matrix,
    features$vocabulary,
    clustering$fit$cluster,
    clustering$selected_k
  )
  cluster_topic_labels <- stats::setNames(
    vapply(seq_len(clustering$selected_k), function(cluster_id) {
      terms <- training_cluster_terms$term[
        training_cluster_terms$cluster == cluster_id
      ]
      suggest_cluster_topic(terms)
    }, character(1)),
    as.character(seq_len(clustering$selected_k))
  )

  test_iterator <- text2vec::itoken(
    testing$text,
    tokenizer = text2vec::word_tokenizer,
    progressbar = FALSE
  )
  test_dtm <- text2vec::create_dtm(test_iterator, features$vectorizer)
  test_tfidf <- features$tfidf$transform(test_dtm)
  test_coordinates <- as.matrix(test_tfidf %*% lsa$decomposition$v)
  test_norms <- sqrt(rowSums(test_coordinates^2))
  unprojectable <- test_norms == 0
  test_norms[unprojectable] <- 1
  test_coordinates <- test_coordinates / test_norms
  distances <- do.call(
    cbind,
    lapply(seq_len(nrow(clustering$fit$centers)), function(cluster_id) {
      residual <- sweep(
        test_coordinates,
        2,
        clustering$fit$centers[cluster_id, ],
        `-`
      )
      rowSums(residual^2)
    })
  )
  test_clusters <- max.col(-distances, ties.method = "first")
  test_clusters[unprojectable] <- NA_integer_
  joined <- testing |>
    dplyr::mutate(
      cluster_id = test_clusters,
      cluster_label = ifelse(
        is.na(test_clusters),
        "Unprojectable text",
        paste0(
          "Cluster ",
          test_clusters,
          ": ",
          unname(cluster_topic_labels[as.character(test_clusters)])
        )
      )
    )
  scored <- dplyr::filter(joined, !is.na(cluster_id))
  if (nrow(scored) < 2L) {
    stop("Fewer than two reviewed questions could be projected.", call. = FALSE)
  }

  posthoc_mapping <- scored |>
    dplyr::count(cluster_id, human_topic_label, name = "mapping_examples") |>
    dplyr::group_by(cluster_id) |>
    dplyr::mutate(cluster_total = sum(mapping_examples)) |>
    dplyr::arrange(
      dplyr::desc(mapping_examples),
      human_topic_label,
      .by_group = TRUE
    ) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::mutate(mapping_support = mapping_examples / cluster_total) |>
    dplyr::ungroup() |>
    dplyr::rename(posthoc_predicted_topic = human_topic_label)
  scored <- scored |>
    dplyr::left_join(
      posthoc_mapping |>
        dplyr::select(
          cluster_id,
          posthoc_predicted_topic,
          mapping_support
        ),
      by = "cluster_id"
    ) |>
    dplyr::mutate(
      posthoc_correct = posthoc_predicted_topic == human_topic_label
    )
  posthoc_accuracy <- mean(scored$posthoc_correct)
  topic_levels <- sort(unique(scored$human_topic_label))
  posthoc_confusion <- table(
    actual = factor(scored$human_topic_label, levels = topic_levels),
    predicted = factor(scored$posthoc_predicted_topic, levels = topic_levels)
  )
  posthoc_per_topic <- dplyr::bind_rows(lapply(topic_levels, function(topic) {
    true_positive <- posthoc_confusion[topic, topic]
    predicted_count <- sum(posthoc_confusion[, topic])
    actual_count <- sum(posthoc_confusion[topic, ])
    precision <- if (predicted_count) true_positive / predicted_count else 0
    recall <- if (actual_count) true_positive / actual_count else 0
    tibble::tibble(
      human_topic_label = topic,
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

  contingency <- table(
    cluster = factor(scored$cluster_id),
    human_topic = factor(scored$human_topic_label)
  )
  n <- sum(contingency)
  pair_total <- choose_two(n)
  cell_pairs <- sum(choose_two(contingency))
  cluster_pairs <- sum(choose_two(rowSums(contingency)))
  topic_pairs <- sum(choose_two(colSums(contingency)))
  expected_pairs <- cluster_pairs * topic_pairs / pair_total
  ari_denominator <- (cluster_pairs + topic_pairs) / 2 - expected_pairs
  ari <- if (abs(ari_denominator) < .Machine$double.eps) {
    if (cell_pairs == cluster_pairs && cell_pairs == topic_pairs) 1 else 0
  } else {
    (cell_pairs - expected_pairs) / ari_denominator
  }
  joint <- contingency / n
  cluster_probability <- rowSums(joint)
  topic_probability <- colSums(joint)
  nonzero <- joint > 0
  mutual_information <- sum(
    joint[nonzero] *
      log(joint[nonzero] / outer(cluster_probability, topic_probability)[nonzero])
  )
  cluster_entropy <- -sum(cluster_probability * log(cluster_probability))
  topic_entropy <- -sum(topic_probability * log(topic_probability))
  nmi <- if (cluster_entropy == 0 || topic_entropy == 0) {
    0
  } else {
    mutual_information / sqrt(cluster_entropy * topic_entropy)
  }
  purity <- sum(apply(contingency, 1, max)) / n

  metrics <- tibble::tibble(
    metric = c(
      "Unlabeled training questions",
      "Reviewed held-out questions",
      "Held-out questions projected",
      "Selected training k",
      "Human topic labels in test set",
      "Post-hoc mapped accuracy (uses the reviewed human labels)",
      "Post-hoc mapped balanced accuracy",
      "Post-hoc mapped macro F1",
      "Adjusted Rand index",
      "Normalized mutual information",
      "Cluster purity (descriptive)"
    ),
    value = c(
      nrow(training),
      nrow(testing),
      nrow(scored),
      clustering$selected_k,
      dplyr::n_distinct(scored$human_topic_label),
      posthoc_accuracy,
      mean(posthoc_per_topic$recall),
      mean(posthoc_per_topic$f1),
      ari,
      nmi,
      purity
    ),
    interpretation = c(
      "Questions used to fit vocabulary, TF-IDF, SVD, and k-means; reviewed rows were excluded.",
      "Independently human-labeled questions reserved for evaluation.",
      "Held-out rows with at least one usable term in the training vocabulary.",
      "Cluster count selected using training data only.",
      "Distinct human topic categories in the projected held-out set.",
      "Descriptive post-hoc score: each cluster is named using the majority human label among these same reviewed rows; optimistic and not independent test accuracy.",
      "Macro average recall for the same post-hoc mapping; uses test labels to name clusters.",
      "Macro F1 for the same post-hoc mapping; uses test labels to name clusters.",
      "Chance-adjusted agreement between holdout cluster membership and human topics; not classification accuracy.",
      "Normalized shared information between holdout clusters and human topics; not classification accuracy.",
      "Descriptive majority-topic share; not chance-adjusted and not classification accuracy."
    )
  )
  readr::write_csv(
    metrics,
    file.path(config$tables_dir, "08_holdout_topic_metrics.csv")
  )
  readr::write_csv(
    scored |>
      dplyr::transmute(
        question_id,
        source_url,
        human_topic_label,
        predicted_cluster = cluster_id,
        posthoc_predicted_topic,
        posthoc_correct,
        mapping_support,
        cluster_label,
        projected = !is.na(cluster_id)
      ),
    file.path(config$tables_dir, "08_holdout_topic_predictions.csv")
  )
  readr::write_csv(
    as.data.frame.matrix(contingency) |>
      tibble::rownames_to_column("cluster_id"),
    file.path(config$tables_dir, "08_holdout_topic_crosstab.csv")
  )
  readr::write_csv(
    posthoc_mapping,
    file.path(config$tables_dir, "08_posthoc_cluster_topic_mapping.csv")
  )
  readr::write_csv(
    posthoc_per_topic,
    file.path(config$tables_dir, "08_posthoc_per_topic_metrics.csv")
  )
  readr::write_csv(
    as.data.frame.matrix(posthoc_confusion) |>
      tibble::rownames_to_column("actual_topic"),
    file.path(config$tables_dir, "08_posthoc_confusion_matrix.csv")
  )
  message("Strict holdout topic evaluation (model fit without reviewed rows):")
  print(metrics)
  invisible(list(metrics = metrics, contingency = contingency, joined = joined))
}

evaluate_blind_topic_labels <- function(config) {
  review_path <- config$topic_review_file
  cluster_path <- config$cluster_review_sample_file
  for (path in c(review_path, cluster_path)) {
    if (!file.exists(path)) {
      stop("Required topic-evaluation file is missing: ", path, call. = FALSE)
    }
  }

  review <- readr::read_csv(review_path, show_col_types = FALSE, progress = FALSE)
  clusters <- readr::read_csv(cluster_path, show_col_types = FALSE, progress = FALSE)
  review_required <- c(
    "question_id", "human_topic_label", "review_status", "source_url"
  )
  cluster_required <- c("question_id", "cluster_id", "cluster_label")
  missing_review <- setdiff(review_required, names(review))
  missing_cluster <- setdiff(cluster_required, names(clusters))
  if (length(missing_review) || length(missing_cluster)) {
    stop(
      "Missing required columns. Review file: ",
      paste(missing_review, collapse = ", "),
      "; cluster file: ",
      paste(missing_cluster, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(review$question_id) || anyDuplicated(clusters$question_id)) {
    stop("Question IDs must be unique in both evaluation files.", call. = FALSE)
  }

  review <- review |>
    dplyr::mutate(
      question_id = as.character(question_id),
      human_topic_label = trimws(as.character(human_topic_label)),
      review_status = tolower(trimws(as.character(review_status))),
      source_url = trimws(as.character(source_url))
    )
  clusters <- clusters |>
    dplyr::mutate(
      question_id = as.character(question_id),
      source_url = trimws(as.character(source_url))
    )
  review <- review |>
    dplyr::filter(
      !is.na(human_topic_label),
      nzchar(human_topic_label),
      !is.na(review_status),
      review_status == "reviewed"
    ) |>
    dplyr::semi_join(clusters, by = "question_id")
  clusters <- clusters |>
    dplyr::semi_join(review, by = "question_id")
  if (nrow(review) < 2L) {
    stop(
      "At least two reviewed questions must also appear in the current cluster sample.",
      call. = FALSE
    )
  }
  joined <- dplyr::inner_join(
    review |>
      dplyr::select(
        question_id,
        human_topic_label,
        human_topic_notes = human_label_notes,
        source_url
      ),
    clusters |>
      dplyr::select(
        question_id,
        cluster_id,
        cluster_label,
        cluster_source_url = source_url
      ),
    by = "question_id"
  )
  if (any(joined$source_url != joined$cluster_source_url)) {
    stop("Source URLs do not match for all reviewed questions.", call. = FALSE)
  }

  contingency <- table(
    cluster = factor(joined$cluster_id),
    human_topic = factor(joined$human_topic_label)
  )
  n <- sum(contingency)
  pair_total <- choose_two(n)
  cell_pairs <- sum(choose_two(contingency))
  cluster_pairs <- sum(choose_two(rowSums(contingency)))
  topic_pairs <- sum(choose_two(colSums(contingency)))
  expected_pairs <- if (pair_total > 0) {
    cluster_pairs * topic_pairs / pair_total
  } else {
    0
  }
  ari_denominator <- (cluster_pairs + topic_pairs) / 2 - expected_pairs
  ari <- if (abs(ari_denominator) < .Machine$double.eps) {
    if (cell_pairs == cluster_pairs && cell_pairs == topic_pairs) 1 else 0
  } else {
    (cell_pairs - expected_pairs) / ari_denominator
  }

  joint <- contingency / n
  cluster_probability <- rowSums(joint)
  topic_probability <- colSums(joint)
  nonzero <- joint > 0
  mutual_information <- sum(
    joint[nonzero] *
      log(joint[nonzero] / outer(cluster_probability, topic_probability)[nonzero])
  )
  cluster_entropy <- -sum(cluster_probability[cluster_probability > 0] *
                            log(cluster_probability[cluster_probability > 0]))
  topic_entropy <- -sum(topic_probability[topic_probability > 0] *
                          log(topic_probability[topic_probability > 0]))
  nmi <- if (cluster_entropy == 0 && topic_entropy == 0) {
    1
  } else if (cluster_entropy == 0 || topic_entropy == 0) {
    0
  } else {
    mutual_information / sqrt(cluster_entropy * topic_entropy)
  }
  purity <- sum(apply(contingency, 1, max)) / n

  metrics <- tibble::tibble(
    metric = c(
      "Reviewed questions",
      "K-means clusters",
      "Human topic labels",
      "Adjusted Rand index",
      "Normalized mutual information",
      "Cluster purity (descriptive)"
    ),
    value = c(
      n,
      nrow(contingency),
      ncol(contingency),
      ari,
      nmi,
      purity
    ),
    interpretation = c(
      "Questions with completed independent human topic labels.",
      "Number of distinct unsupervised k-means groups represented.",
      "Number of distinct human-assigned topic categories.",
      "Chance-adjusted agreement between cluster memberships and human topics; 1 is perfect, 0 is chance-level.",
      "Shared information between cluster membership and human topic labels, normalized to 0-1.",
      "Share of questions matching each cluster's most common human topic; descriptive and not chance-adjusted."
    )
  )
  readr::write_csv(
    metrics,
    file.path(config$tables_dir, "07_topic_agreement_metrics.csv")
  )
  readr::write_csv(
    as.data.frame.matrix(contingency) |>
      tibble::rownames_to_column("cluster_id"),
    file.path(config$tables_dir, "07_cluster_human_topic_crosstab.csv")
  )
  per_cluster <- joined |>
    dplyr::count(cluster_id, cluster_label, human_topic_label, name = "questions") |>
    dplyr::group_by(cluster_id, cluster_label) |>
    dplyr::mutate(
      cluster_total = sum(questions),
      topic_share = questions / cluster_total,
      majority_topic = human_topic_label[
        which.max(questions)
      ],
      cluster_purity = max(questions) / cluster_total
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(cluster_id, dplyr::desc(questions), human_topic_label)
  readr::write_csv(
    per_cluster,
    file.path(config$tables_dir, "07_cluster_topic_breakdown.csv")
  )

  heatmap <- ggplot2::ggplot(
    per_cluster,
    ggplot2::aes(
      x = stringr::str_wrap(human_topic_label, 20),
      y = factor(cluster_id),
      fill = topic_share
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_gradient(
      low = "#f3f6f4",
      high = "#327a65",
      name = "Share within\ncluster",
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      title = "Human topics within k-means clusters",
      subtitle = sprintf(
        "Adjusted Rand index = %.3f; normalized mutual information = %.3f",
        ari,
        nmi
      ),
      x = "Independently assigned human topic",
      y = "K-means cluster"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )
  ggplot2::ggsave(
    file.path(config$plots_dir, "07_cluster_human_topic_heatmap.png"),
    heatmap,
    width = 14,
    height = 8,
    dpi = 160
  )

  message("Blind human-topic evaluation:")
  print(metrics)
  invisible(list(
    metrics = metrics,
    crosstab = contingency,
    per_cluster = per_cluster,
    joined = joined
  ))
}
