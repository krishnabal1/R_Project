script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1]])
} else {
  "validate_lsa_k_configs.R"
}
project_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
setwd(project_dir)

source(file.path(project_dir, "R", "00_config.R"))
source(file.path(project_dir, "R", "01_data.R"))
source(file.path(project_dir, "R", "02_features.R"))
source(file.path(project_dir, "R", "03_clustering.R"))

config <- make_config(project_dir)
make_directories(config)
candidate_configs <- data.frame(
  lsa_dimensions = c(2L, 2L, 3L, 4L, 5L, 5L),
  k = c(18L, 2L, 11L, 10L, 4L, 12L)
)

questions <- load_questions(config)
features <- fit_text_features(questions$text, config)
nonempty <- Matrix::rowSums(features$matrix) > 0
features$matrix <- features$matrix[nonempty, , drop = FALSE]
questions <- questions[nonempty, , drop = FALSE]

training_indices <- sample_indices(
  nrow(features$matrix),
  config$selection_sample_size,
  config$random_seed
)
remaining_indices <- setdiff(seq_len(nrow(features$matrix)), training_indices)
set.seed(config$random_seed + 9876L)
validation_indices <- sort(sample(
  remaining_indices,
  min(config$selection_sample_size, length(remaining_indices))
))

validation_rows <- lapply(seq_len(nrow(candidate_configs)), function(i) {
  dimensions <- candidate_configs$lsa_dimensions[[i]]
  k <- candidate_configs$k[[i]]
  dimension_config <- config
  dimension_config$svd_components <- dimensions
  set.seed(config$random_seed)
  lsa <- fit_lsa(features$matrix, dimension_config)
  training <- lsa$coordinates[training_indices, , drop = FALSE]
  validation <- lsa$coordinates[validation_indices, , drop = FALSE]

  set.seed(config$random_seed + k)
  fit <- stats::kmeans(
    training,
    centers = k,
    nstart = config$kmeans_restarts,
    iter.max = 1000L,
    algorithm = "Lloyd"
  )
  distances <- vapply(seq_len(k), function(cluster_id) {
    residual <- sweep(validation, 2, fit$centers[cluster_id, ], `-`)
    rowSums(residual^2)
  }, numeric(nrow(validation)))
  labels <- max.col(-distances, ties.method = "first")
  sizes <- table(labels)
  silhouette <- cluster::silhouette(labels, stats::dist(validation))
  data.frame(
    lsa_dimensions = dimensions,
    k = k,
    training_sample_size = nrow(training),
    validation_sample_size = nrow(validation),
    validation_cluster_count = length(sizes),
    minimum_validation_cluster_size = min(sizes),
    validation_silhouette = mean(silhouette[, "sil_width"])
  )
})

readr::write_csv(
  dplyr::bind_rows(validation_rows),
  file.path(config$tables_dir, "04_lsa_k_sensitivity_validation.csv")
)
