script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1]])
} else {
  "evaluate_lsa_k_sensitivity.R"
}
project_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
setwd(project_dir)

source(file.path(project_dir, "R", "00_config.R"))
source(file.path(project_dir, "R", "01_data.R"))
source(file.path(project_dir, "R", "02_features.R"))
source(file.path(project_dir, "R", "03_clustering.R"))

config <- make_config(project_dir)
make_directories(config)
dimensions_to_test <- c(2:20, 40L, 60L, 80L)

message("Loading local data for LSA-dimension and k sensitivity testing.")
questions <- load_questions(config)
features <- fit_text_features(questions$text, config)
nonempty <- Matrix::rowSums(features$matrix) > 0
features$matrix <- features$matrix[nonempty, , drop = FALSE]
questions <- questions[nonempty, , drop = FALSE]

selection_indices <- sample_indices(
  nrow(features$matrix),
  config$selection_sample_size,
  config$random_seed
)
minimum_cluster_size <- max(
  5L,
  ceiling(0.005 * length(selection_indices))
)

candidate_rows <- list()
row_index <- 0L
for (dimension in dimensions_to_test) {
  message(sprintf("Evaluating %d LSA dimensions.", dimension))
  dimension_config <- config
  dimension_config$svd_components <- dimension
  set.seed(config$random_seed)
  lsa <- fit_lsa(features$matrix, dimension_config)
  sample_features <- lsa$coordinates[selection_indices, , drop = FALSE]
  sample_distances <- stats::dist(sample_features)

  dimension_rows <- lapply(config$k_values, function(k) {
    set.seed(config$random_seed + k)
    fit <- stats::kmeans(
      sample_features,
      centers = k,
      nstart = config$kmeans_restarts,
      iter.max = 1000L,
      algorithm = "Lloyd"
    )
    silhouette <- cluster::silhouette(fit$cluster, sample_distances)
    cluster_sizes <- table(fit$cluster)
    data.frame(
      lsa_dimensions = dimension,
      k = k,
      within_cluster_sum_squares = fit$tot.withinss,
      minimum_cluster_size = min(cluster_sizes),
      silhouette = mean(silhouette[, "sil_width"]),
      eligible = min(cluster_sizes) >= minimum_cluster_size,
      stringsAsFactors = FALSE
    )
  })
  dimension_results <- dplyr::bind_rows(dimension_rows)
  dimension_results$size_eligible <- dimension_results$eligible
  unconstrained_eligible <- which(
    dimension_results$size_eligible &
      is.finite(dimension_results$silhouette)
  )
  if (length(unconstrained_eligible) == 0L) {
    stop(
      "No candidate met the minimum-cluster-size rule for ",
      dimension,
      " LSA dimensions.",
      call. = FALSE
    )
  }
  unconstrained_selected <- unconstrained_eligible[
    which.max(dimension_results$silhouette[unconstrained_eligible])
  ]
  dimension_results$selected_unconstrained <-
    seq_len(nrow(dimension_results)) == unconstrained_selected
  dimension_results$eligible <- (
    dimension_results$size_eligible &
      dimension_results$k >= config$minimum_k
  )
  eligible <- which(
    dimension_results$eligible &
      is.finite(dimension_results$silhouette)
  )
  if (length(eligible) == 0L) {
    stop(
      "No candidate with k >= minimum_k met the minimum-cluster-size rule for ",
      dimension,
      " LSA dimensions.",
      call. = FALSE
    )
  }
  selected <- eligible[which.max(dimension_results$silhouette[eligible])]
  dimension_results$selected <- seq_len(nrow(dimension_results)) == selected
  row_index <- row_index + 1L
  candidate_rows[[row_index]] <- dimension_results
}

results <- dplyr::bind_rows(candidate_rows)
results <- results |>
  dplyr::select(
    lsa_dimensions,
    k,
    within_cluster_sum_squares,
    minimum_cluster_size,
    silhouette,
    size_eligible,
    eligible,
    selected,
    selected_unconstrained
  )
readr::write_csv(
  results,
  file.path(config$tables_dir, "04_lsa_k_sensitivity.csv")
)

best_by_dimension <- results |>
  dplyr::filter(selected) |>
  dplyr::arrange(dplyr::desc(silhouette))
readr::write_csv(
  best_by_dimension,
  file.path(config$tables_dir, "04_lsa_k_sensitivity_summary.csv")
)
best_unconstrained_by_dimension <- results |>
  dplyr::filter(selected_unconstrained) |>
  dplyr::arrange(dplyr::desc(silhouette))
readr::write_csv(
  best_unconstrained_by_dimension,
  file.path(config$tables_dir, "04_lsa_k_sensitivity_unconstrained.csv")
)

sensitivity_plot <- ggplot2::ggplot(
  results,
  ggplot2::aes(
    x = k,
    y = silhouette,
    color = factor(lsa_dimensions),
    group = factor(lsa_dimensions)
  )
) +
  ggplot2::geom_line(alpha = 0.75) +
  ggplot2::geom_point(
    data = dplyr::filter(results, selected),
    size = 2.5
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(results, selected_unconstrained),
    shape = 4,
    color = "#202020",
    size = 2.5
  ) +
  ggplot2::labs(
    title = "K-means silhouette across LSA dimensions and k",
    subtitle = paste(
      sprintf(
        "Circles mark the best k >= %d; x marks the unconstrained best k per dimension.",
        config$minimum_k
      ),
      "All scores use the same sample and k range."
    ),
    x = "Number of clusters (k)",
    y = "Mean sample silhouette",
    color = "LSA dimensions",
    caption = sprintf(
      "Eligibility requires at least %d questions in the smallest cluster.",
      minimum_cluster_size
    )
  ) +
  ggplot2::theme_minimal(base_size = 11)
ggplot2::ggsave(
  file.path(config$plots_dir, "04_lsa_k_sensitivity.png"),
  sensitivity_plot,
  width = 11,
  height = 7,
  dpi = 160
)

message("Best eligible candidate per LSA dimension:")
print(
  best_by_dimension |>
    dplyr::select(
      lsa_dimensions,
      k,
      silhouette,
      minimum_cluster_size,
      within_cluster_sum_squares
    )
)
message("Unconstrained silhouette maximum per LSA dimension:")
print(
  best_unconstrained_by_dimension |>
    dplyr::select(
      lsa_dimensions,
      k,
      silhouette,
      minimum_cluster_size,
      within_cluster_sum_squares
    )
)
