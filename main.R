script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1]])
} else {
  "main.R"
}
project_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
setwd(project_dir)

required_packages <- c(
  "readr", "dplyr", "stringr", "ggplot2", "xml2", "text2vec",
  "stopwords", "Matrix", "irlba", "cluster", "dbscan", "tibble"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install required packages before running: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

source_files <- c(
  "R/00_config.R",
  "R/01_data.R",
  "R/02_features.R",
  "R/03_clustering.R",
  "R/04_review.R",
  "R/05_classification.R",
  "R/06_outputs.R"
)
for (source_file in source_files) {
  source(file.path(project_dir, source_file), local = .GlobalEnv)
}

set.seed(123)
config <- make_config(project_dir)
make_directories(config)
write_readme(config)
message("Loading and auditing the local Stack Exchange CSV.")
original_rows <- nrow(readr::read_csv(
  config$data_file,
  show_col_types = FALSE,
  progress = FALSE
))
questions <- load_questions(config)
write_data_audit(questions, config)
write_eda_outputs(questions, config)
message(sprintf(
  "Prepared %s candidate questions from %s original rows.",
  format(nrow(questions), big.mark = ","),
  format(original_rows, big.mark = ",")
))

message("Building TF-IDF and truncated SVD/LSA features.")
features <- fit_text_features(questions$text, config)
nonempty <- Matrix::rowSums(features$matrix) > 0
questions <- questions[nonempty, , drop = FALSE]
features$matrix <- features$matrix[nonempty, , drop = FALSE]
lsa <- fit_lsa(features$matrix, config)
readr::write_csv(
  tibble::tibble(
    component = seq_len(lsa$component_count),
    singular_value = lsa$decomposition$d,
    relative_variance = lsa$explained_variance
  ),
  file.path(config$tables_dir, "03_lsa_feature_dimensions.csv")
)

message("Comparing k-means, DBSCAN, and hierarchical clustering.")
clustering <- fit_all_clusters(questions, features, lsa, config)
saved <- save_cluster_outputs(
  clustering$questions,
  features,
  lsa,
  clustering,
  config
)
message(sprintf(
  "Selected k=%d; sample silhouette=%.4f; stability mean ARI=%.4f.",
  clustering$kmeans$selected_k,
  clustering$kmeans$metrics$silhouette,
  clustering$stability$mean_ari
))
message("Algorithm comparison:")
print(saved$comparison)
message(
  "Cluster topic labels: data/review/cluster_topic_labels.csv"
)
cluster_review_sample <- write_cluster_review_sample(
  clustering$questions,
  saved$topic_labels,
  config
)
message(sprintf(
  "Saved %d real questions from %d clusters to %s.",
  nrow(cluster_review_sample),
  dplyr::n_distinct(cluster_review_sample$cluster_id),
  "data/processed/06_cluster_labeled_300_with_sources.csv"
))
classification <- evaluate_verified_cluster_mapping(config)
write_analysis_report(
  original_rows,
  clustering$questions,
  clustering,
  config,
  classification
)
if (is.null(classification)) {
  message(
    "Predictive evaluation is unavailable until enough source-linked cases ",
    "are available for calibration and a source-isolated test split."
  )
} else {
  message(sprintf(
    "%s: %.1f%% on %d test example(s) (%d total; %d calibration).",
    if (classification$evidence_type == "fact_sheet_paraphrase") {
      "Source-derived benchmark accuracy (not real-world accuracy)"
    } else {
      "Trusted-case held-out accuracy"
    },
    100 * classification$metrics$value[
      classification$metrics$metric == "Accuracy"
    ],
    classification$metrics$value[
      classification$metrics$metric == "Test examples"
    ],
    classification$verified_count,
    classification$calibration_count
  ))
}
