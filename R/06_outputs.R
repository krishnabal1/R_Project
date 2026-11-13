save_cluster_outputs <- function(questions, features, lsa, clustering, config) {
  top_terms <- top_cluster_terms(
    features$matrix,
    features$vocabulary,
    clustering$kmeans$fit$cluster,
    clustering$kmeans$selected_k
  )
  write_cluster_summary(questions, top_terms, config)
  topic_labels <- make_cluster_topic_labels(questions, top_terms, config)

  readr::write_csv(
    clustering$kmeans$candidates,
    file.path(config$tables_dir, "04_kmeans_k_comparison.csv")
  )
  readr::write_csv(
    clustering$dbscan$tuning,
    file.path(config$tables_dir, "04_dbscan_parameter_search.csv")
  )
  readr::write_csv(
    tibble::tibble(
      question_id = questions$question_id[clustering$hierarchical$sample_indices],
      hierarchical_cluster = clustering$hierarchical$labels
    ),
    file.path(config$tables_dir, "04_hierarchical_sample_assignments.csv")
  )

  comparison <- dplyr::bind_rows(
    data.frame(algorithm = "K-means", clustering$kmeans$metrics),
    data.frame(algorithm = "DBSCAN", clustering$dbscan$metrics),
    data.frame(algorithm = "Hierarchical Ward (sample)", clustering$hierarchical$metrics)
  )
  comparison$stability_mean_ari <- NA_real_
  comparison$stability_mean_ari[comparison$algorithm == "K-means"] <-
    clustering$stability$mean_ari
  readr::write_csv(
    comparison,
    file.path(config$tables_dir, "04_algorithm_comparison.csv")
  )

  saveRDS(
    list(
      vocabulary = features$vocabulary,
      vectorizer = features$vectorizer,
      tfidf = features$tfidf,
      svd = lsa$decomposition,
      component_count = lsa$component_count,
      config = config
    ),
    file.path(config$models_dir, "03_unsupervised_text_feature_model.rds")
  )
  saveRDS(
    clustering$kmeans$fit,
    file.path(config$models_dir, "04_kmeans_model.rds")
  )
  saveRDS(
    list(
      fit = clustering$dbscan$fit,
      sample_indices = clustering$dbscan$sample_indices,
      epsilon = clustering$dbscan$epsilon,
      min_points = clustering$dbscan$min_points
    ),
    file.path(config$models_dir, "04_dbscan_model.rds")
  )
  saveRDS(
    list(
      fit = clustering$hierarchical$fit,
      sample_indices = clustering$hierarchical$sample_indices,
      labels = clustering$hierarchical$labels
    ),
    file.path(config$models_dir, "04_hierarchical_model_sample.rds")
  )

  selection_plot <- ggplot2::ggplot(
    clustering$kmeans$candidates,
    ggplot2::aes(x = k, y = silhouette)
  ) +
    ggplot2::geom_line(color = "#327a65") +
    ggplot2::geom_point(color = "#327a65", size = 2) +
    ggplot2::geom_vline(
      xintercept = clustering$kmeans$selected_k,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = "K-means model selection",
      subtitle = paste(
        sprintf(
          "Highest sample silhouette among k >= %d candidates meeting the minimum-size check;",
          config$minimum_k
        ),
        "inspect stability and topic usefulness too"
      ),
      x = "Number of clusters (k)",
      y = "Sample silhouette"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(
    file.path(config$plots_dir, "04_kmeans_silhouette.png"),
    selection_plot,
    width = 10,
    height = 6,
    dpi = 150
  )

  elbow_plot <- ggplot2::ggplot(
    clustering$kmeans$candidates,
    ggplot2::aes(x = k, y = within_cluster_sum_squares)
  ) +
    ggplot2::geom_line(color = "#327a65") +
    ggplot2::geom_point(color = "#327a65", size = 2) +
    ggplot2::geom_vline(
      xintercept = clustering$kmeans$selected_k,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = "K-means elbow diagnostic",
      subtitle = "Use the bend as a diagnostic alongside silhouette and review",
      x = "Number of clusters (k)",
      y = "Within-cluster sum of squares"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(
    file.path(config$plots_dir, "04_kmeans_elbow.png"),
    elbow_plot,
    width = 10,
    height = 6,
    dpi = 150
  )

  sizes <- as.data.frame(table(clustering$questions$kmeans_cluster))
  names(sizes) <- c("cluster", "questions")
  size_plot <- ggplot2::ggplot(
    sizes,
    ggplot2::aes(x = factor(cluster), y = questions)
  ) +
    ggplot2::geom_col(fill = "#327a65") +
    ggplot2::labs(title = "Questions per k-means cluster", x = "Cluster", y = "Questions") +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(
    file.path(config$plots_dir, "04_kmeans_cluster_sizes.png"),
    size_plot,
    width = 8,
    height = 5,
    dpi = 150
  )

  selection_indices <- clustering$kmeans$selection_indices
  selected_silhouette <- cluster::silhouette(
    clustering$kmeans$fit$cluster[selection_indices],
    stats::dist(clustering$coordinates[selection_indices, , drop = FALSE])
  )
  silhouette_data <- tibble::tibble(
    cluster = factor(selected_silhouette[, "cluster"]),
    silhouette_width = selected_silhouette[, "sil_width"]
  )
  cluster_order <- silhouette_data |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      median_width = stats::median(silhouette_width),
      .groups = "drop"
    ) |>
    dplyr::arrange(median_width) |>
    dplyr::pull(cluster) |>
    as.character()
  silhouette_data$cluster <- factor(
    silhouette_data$cluster,
    levels = cluster_order
  )
  silhouette_width_plot <- ggplot2::ggplot(
    silhouette_data,
    ggplot2::aes(x = silhouette_width, y = cluster)
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#a23b3b") +
    ggplot2::geom_boxplot(
      fill = "#78a995",
      outlier.shape = NA,
      linewidth = 0.25
    ) +
    ggplot2::coord_cartesian(xlim = c(-1, 1)) +
    ggplot2::labs(
      title = sprintf(
        "Silhouette widths by cluster (k=%d)",
        clustering$kmeans$selected_k
      ),
      subtitle = "Negative or near-zero widths indicate overlapping or poorly assigned points",
      x = "Silhouette width",
      y = "Cluster (ordered by median width)"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  ggplot2::ggsave(
    file.path(config$plots_dir, "04_kmeans_silhouette_by_cluster.png"),
    silhouette_width_plot,
    width = 9,
    height = max(7, 0.22 * clustering$kmeans$selected_k),
    dpi = 160
  )

  scatter <- tibble::tibble(
    lsa_1 = clustering$coordinates[, 1],
    lsa_2 = clustering$coordinates[, 2],
    cluster = factor(clustering$questions$kmeans_cluster)
  )
  cluster_levels <- levels(scatter$cluster)
  distinct_cluster_colors <- c(
    "#0072B2",
    "#E69F00",
    "#009E73",
    "#CC79A7",
    "#D55E00",
    "#56B4E9",
    "#F0E442",
    "#000000"
  )
  cluster_colors <- if (length(cluster_levels) <= length(distinct_cluster_colors)) {
    distinct_cluster_colors[seq_along(cluster_levels)]
  } else {
    c(
      distinct_cluster_colors,
      grDevices::hcl.colors(
        length(cluster_levels) - length(distinct_cluster_colors),
        palette = "Dark 3"
      )
    )
  }
  names(cluster_colors) <- cluster_levels
  centroids <- scatter |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      lsa_1 = mean(lsa_1),
      lsa_2 = mean(lsa_2),
      .groups = "drop"
    )
  scatter_plot <- ggplot2::ggplot(
    scatter,
    ggplot2::aes(x = lsa_1, y = lsa_2, color = cluster)
  ) +
    ggplot2::geom_point(alpha = 0.7, size = 0.8) +
    ggplot2::scale_color_manual(values = cluster_colors, drop = FALSE) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(alpha = 1, size = 3)
      )
    ) +
    ggplot2::geom_text(
      data = centroids,
      ggplot2::aes(x = lsa_1, y = lsa_2, label = cluster),
      inherit.aes = FALSE,
      color = "#202020",
      size = 2.5
    ) +
    ggplot2::labs(
      title = sprintf(
        "K-means clusters in a 2-D LSA projection (k=%d)",
        clustering$kmeans$selected_k
      ),
      subtitle = sprintf(
        "Distinct colors identify clusters; numbers mark centroids. Clustering uses all %d LSA dimensions.",
        lsa$component_count
      ),
      x = "LSA component 1",
      y = "LSA component 2",
      color = "Cluster",
      caption = "Centroid numbers may overlap in 2-D; inspect silhouette-by-cluster plot for separation quality."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold")
    )
  ggplot2::ggsave(
    file.path(config$plots_dir, "04_kmeans_lsa_scatter.png"),
    scatter_plot,
    width = 10,
    height = 7,
    dpi = 150
  )

  dbscan_indices <- clustering$dbscan$sample_indices
  dbscan_labels <- clustering$dbscan$fit$cluster
  dbscan_scatter <- tibble::tibble(
    lsa_1 = clustering$coordinates[dbscan_indices, 1],
    lsa_2 = clustering$coordinates[dbscan_indices, 2],
    cluster = factor(dbscan_labels)
  )
  dbscan_levels <- levels(dbscan_scatter$cluster)
  noise_color <- if ("0" %in% dbscan_levels) {
    c("0" = "#B8B8B8")
  } else {
    character()
  }
  dbscan_cluster_levels <- setdiff(dbscan_levels, "0")
  dbscan_palette <- c(
    "#0072B2",
    "#E69F00",
    "#009E73",
    "#CC79A7",
    "#D55E00",
    "#56B4E9",
    "#F0E442",
    "#000000"
  )
  dbscan_cluster_colors <- if (length(dbscan_cluster_levels) <= length(dbscan_palette)) {
    dbscan_palette[seq_along(dbscan_cluster_levels)]
  } else {
    c(
      dbscan_palette,
      grDevices::hcl.colors(
        length(dbscan_cluster_levels) - length(dbscan_palette),
        palette = "Dark 3"
      )
    )
  }
  names(dbscan_cluster_colors) <- dbscan_cluster_levels
  dbscan_colors <- c(noise_color, dbscan_cluster_colors)
  dbscan_scatter_plot <- ggplot2::ggplot(
    dbscan_scatter,
    ggplot2::aes(x = lsa_1, y = lsa_2, color = cluster)
  ) +
    ggplot2::geom_point(alpha = 0.75, size = 1) +
    ggplot2::scale_color_manual(
      values = dbscan_colors,
      breaks = dbscan_levels,
      labels = ifelse(dbscan_levels == "0", "Noise (0)", dbscan_levels),
      drop = FALSE
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(alpha = 1, size = 3)
      )
    ) +
    ggplot2::labs(
      title = sprintf(
        "DBSCAN clusters in a 2-D LSA projection (%d clusters)",
        clustering$dbscan$metrics$cluster_count
      ),
      subtitle = sprintf(
        "Sampled questions; minPts=%d, epsilon=%.3f. Gray points are noise.",
        clustering$dbscan$min_points,
        clustering$dbscan$epsilon
      ),
      x = "LSA component 1",
      y = "LSA component 2",
      color = "DBSCAN label",
      caption = sprintf(
        "%.1f%% of sampled points are noise; DBSCAN was fit using all %d LSA dimensions.",
        100 * (1 - clustering$dbscan$metrics$assigned_fraction),
        lsa$component_count
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold")
    )
  ggplot2::ggsave(
    file.path(config$plots_dir, "04_dbscan_lsa_scatter.png"),
    dbscan_scatter_plot,
    width = 10,
    height = 7,
    dpi = 150
  )

  invisible(list(
    top_terms = top_terms,
    topic_labels = topic_labels,
    comparison = comparison
  ))
}

write_analysis_report <- function(
  original_rows,
  questions,
  lsa,
  clustering,
  config,
  classification = NULL
) {
  dbscan_metrics <- clustering$dbscan$metrics
  quality_warnings <- character()
  if (clustering$kmeans$metrics$silhouette < 0.5) {
    quality_warnings <- c(
      quality_warnings,
      "K-means silhouette is below 0.5; separation is not strong and clusters need careful human review."
    )
  }
  if (lsa$component_count <= 2L) {
    quality_warnings <- c(
      quality_warnings,
      "The 2-D embedding forms a curved continuum; its high silhouette may reward arc segmentation rather than distinct semantic topics. Treat k and labels as provisional."
    )
  }
  if (clustering$kmeans$selected_k == max(config$k_values)) {
    quality_warnings <- c(
      quality_warnings,
      "Selected k is at the maximum tested value; extend the search and inspect the elbow before finalizing cluster count."
    )
  }
  if (clustering$stability$mean_ari < 0.6) {
    quality_warnings <- c(
      quality_warnings,
      "K-means assignments vary across random starts; cluster stability is limited."
    )
  }
  if (dbscan_metrics$cluster_count < 2L) {
    quality_warnings <- c(
      quality_warnings,
      "DBSCAN found fewer than two clusters under the tested settings."
    )
  }
  if (dbscan_metrics$assigned_fraction < 0.8) {
    quality_warnings <- c(
      quality_warnings,
      "DBSCAN leaves more than 20% of sampled questions as noise; its silhouette applies only to assigned points."
    )
  }
  if (!is.null(classification) &&
      classification$metrics$value[
        classification$metrics$metric == "Macro F1"
      ] < 0.5) {
    quality_warnings <- c(
      quality_warnings,
      "Classification macro F1 is low; performance varies across categories and needs improvement."
    )
  }
  classification_lines <- if (is.null(classification)) {
    result_lines <- c(
      "CLUSTER-BASED CONDITION EVALUATION",
      paste(
        "Not evaluated: add source-linked verified cases to",
        "data/verified/verified_plant_cases.csv."
      )
    )
  } else {
    metric_values <- stats::setNames(
      classification$metrics$value,
      classification$metrics$metric
    )
    heldout_count <- metric_values[["Test examples"]]
    classification_limitation <- paste(
      "Calibration cases alone map clusters to conditions; test labels are not",
      "used to name clusters. Scores measure agreement with source labels,",
      "not clinical diagnostic accuracy."
    )
    if (heldout_count < 30L) {
      classification_limitation <- paste(
        classification_limitation,
        "The held-out sample is very small."
      )
    }
    result_lines <- c(
      "CLUSTER-BASED CONDITION EVALUATION",
      paste("Evidence type:", classification$evidence_type),
      paste(
        if (classification$evidence_type == "fact_sheet_paraphrase") {
          "Source-derived benchmark examples used:"
        } else {
          "Source-linked reference examples used:"
        },
        classification$verified_count
      ),
      paste("Unverified or synthetic rows excluded:", classification$excluded_count),
      paste("Categories used:", paste(classification$categories, collapse = ", ")),
      paste("Calibration examples:", classification$calibration_count),
      paste("Held-out examples:", heldout_count),
      sprintf(
        "Held-out cases assigned to a mapped cluster: %.1f%%",
        100 * classification$mapped_coverage
      ),
      sprintf(
        "%s: %.1f%%",
        if (classification$evidence_type == "fact_sheet_paraphrase") {
          "Source-derived benchmark accuracy"
        } else {
          "Held-out accuracy"
        },
        100 * metric_values[["Accuracy"]]
      ),
      sprintf(
        "Held-out balanced accuracy: %.1f%%",
        100 * metric_values[["Balanced accuracy"]]
      ),
      sprintf(
        "Held-out macro precision / recall: %.3f / %.3f",
        metric_values[["Macro precision"]],
        metric_values[["Macro recall"]]
      ),
      sprintf("Held-out macro F1: %.3f", metric_values[["Macro F1"]]),
      sprintf(
        "All 13 rows apparent accuracy (includes calibration cases): %.1f%%",
        100 * classification$all_case_metrics$value[
          classification$all_case_metrics$metric ==
            "Apparent accuracy (includes calibration cases)"
        ]
      ),
      "The all-row apparent score is descriptive only; calibration cases helped create the mapping.",
      paste("Important limitation:", classification_limitation)
    )
    if (classification$evidence_type == "fact_sheet_paraphrase") {
      result_lines <- c(
        result_lines,
        paste(
          "This benchmark uses paraphrases authored from fact sheets,",
          "not independent plant observations; do not report its scores",
          "as real-world prediction accuracy. The tiny number of source pages",
          "also makes calibration and held-out results highly preliminary."
        )
      )
    }
    result_lines
  }
  lines <- c(
    "PLANT HEALTH QUESTION ANALYSIS",
    "==============================",
    "",
    paste("Source:", basename(config$data_file)),
    paste("Original data rows:", original_rows),
    paste("Health candidate questions:", nrow(questions)),
    paste(
      "Balanced real-question cluster sample:",
      config$cluster_review_sample_size,
      "rows at data/processed/06_cluster_labeled_300_with_sources.csv"
    ),
    paste("K-means selected k:", clustering$kmeans$selected_k),
    paste(
      "K-means search range:",
      min(config$k_values),
      "to",
      max(config$k_values)
    ),
    paste("Minimum k eligible for selection:", config$minimum_k),
    paste("LSA dimensions used for clustering:", lsa$component_count),
    sprintf(
      "K-means sample silhouette: %.4f",
      clustering$kmeans$metrics$silhouette
    ),
    sprintf(
      "K-means stability mean ARI: %.4f",
      clustering$stability$mean_ari
    ),
    paste("DBSCAN sampled clusters:", dbscan_metrics$cluster_count),
    sprintf("DBSCAN sample coverage: %.1f%%", 100 * dbscan_metrics$assigned_fraction),
    sprintf("DBSCAN sample silhouette: %.4f", dbscan_metrics$silhouette),
    "",
    "Stack Exchange questions are used only as unlabeled text for clustering.",
    "Cluster names are human topic summaries, not disease diagnoses or training labels.",
    "The 300-row cluster sample contains real questions with one unsupervised cluster ID; it is separate from the source-derived disease benchmark.",
    "A source-grouped calibration subset maps clusters to conditions; disjoint source pages are held out for evaluation.",
    "The health-keyword filter selects candidates and may include false positives or miss cases.",
    "DBSCAN and hierarchical results are sample-based for bounded memory and runtime.",
    "",
    classification_lines,
    "",
    "QUALITY WARNINGS",
    if (length(quality_warnings)) {
      paste("-", quality_warnings)
    } else {
      "Review clusters and examples before drawing conclusions."
    }
  )
  writeLines(lines, file.path(config$project_dir, "analysis_report.txt"))
}

write_readme <- function(config) {
  lines <- c(
    "# Plant health text exploration and cluster-based evaluation",
    "",
    "Unlabeled gardening Stack Exchange questions are clustered for topic exploration.",
    "K-means is fit only on unlabeled Stack Exchange questions. A separate,",
    "source-linked calibration subset maps clusters to conditions, and disjoint",
    "source pages are held out for evaluation. Test labels never name clusters.",
    "",
    "## Run",
    "",
    "Open this folder in RStudio and source `main.R`, or run `Rscript main.R`.",
    "The original CSV is read from the project root. Set `PLANT_DATA_FILE` to",
    "override its location. Install required R packages once with:",
    "",
    "```r",
    "install.packages(c(\"readr\", \"dplyr\", \"stringr\", \"ggplot2\", \"xml2\",",
    "                   \"text2vec\", \"stopwords\", \"Matrix\", \"irlba\",",
    "                   \"cluster\", \"dbscan\"))",
    "```",
    "",
    "## Workflow",
    "",
    "1. Run `main.R` to clean the local Stack Exchange CSV and compare k-means, DBSCAN, and hierarchical clusters.",
    "2. Review `outputs/tables/04_cluster_summary_top_terms.csv` and representative examples.",
    "3. Review question topics and current model labels in the single `data/review/06_topic_review.csv` sheet; the labels are exploratory, not diagnoses.",
    "4. Add short paraphrased symptom cases with a supported condition and source URL to `data/verified/verified_plant_cases.csv`.",
    "5. Set `verified` to `yes` only after checking a reliable diagnostic source; enter verifier and date.",
    "6. Rerun `main.R`; calibration cases map k-means clusters to conditions, while disjoint source pages are held out for evaluation.",
    "7. Use `predict_new_question.R` to assign a new question to a mapped cluster. Unmapped clusters return an explicit message.",
    "",
    "Unsupervised scores (silhouette, Davies-Bouldin, Calinski-Harabasz) describe",
    "cluster structure, not disease accuracy. K-means candidates are searched from 2 through 60;",
    "the unconstrained silhouette maximum was k=2, but two buckets are too broad for topic discovery.",
    "Selection therefore requires k >= 10 to preserve more topic granularity.",
    "Run `Rscript evaluate_lsa_k_sensitivity.R` to compare dimensions and k.",
    "Run `Rscript validate_lsa_k_configs.R` to compare leading candidates on a separate point sample.",
    "The current 2-dimension, k>=10 selection is a high-silhouette topic-granularity tradeoff;",
    "inspect sensitivity results and cluster terms before interpreting it.",
    "Cluster-mapping metrics are computed",
    "only from a held-out split grouped by source page. Never use held-out",
    "labels to assign a condition to a cluster. Stack Exchange questions and",
    "the old manual annotation sheet are not used as diagnosis labels.",
    "Prefer university extension, plant diagnostic clinic, or government sources.",
    "Store paraphrased symptom text and a source link, not copied articles.",
    "Model outputs are possible causes for triage, not a confirmed plant diagnosis.",
    "Existing Stack Exchange annotation files are preserved but are not used to train the model.",
    "",
    "See `WORKFLOW.md` for data quality and evaluation requirements."
  )
  writeLines(lines, file.path(config$project_dir, "README.md"))
}
