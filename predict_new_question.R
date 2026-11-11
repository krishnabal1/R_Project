arguments <- commandArgs(trailingOnly = TRUE)
question <- if (length(arguments)) {
  paste(arguments, collapse = " ")
} else {
  cat("Enter the plant question: ")
  flush.console()
  answer <- readLines(file("stdin"), n = 1L, warn = FALSE)
  if (length(answer)) answer[[1]] else ""
}
if (!nzchar(trimws(question))) {
  stop("Enter a non-empty question.", call. = FALSE)
}

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1]])
} else {
  "predict_new_question.R"
}
project_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
setwd(project_dir)
source(file.path(project_dir, "R", "01_data.R"))
source(file.path(project_dir, "R", "02_features.R"))
source(file.path(project_dir, "R", "00_config.R"))
source(file.path(project_dir, "R", "05_classification.R"))

mapping_file <- file.path(
  project_dir, "outputs", "models", "06_verified_cluster_mapping.rds"
)
feature_model_file <- file.path(
  project_dir, "outputs", "models", "03_unsupervised_text_feature_model.rds"
)
cluster_model_file <- file.path(
  project_dir, "outputs", "models", "04_kmeans_model.rds"
)
required_files <- c(mapping_file, feature_model_file, cluster_model_file)
if (!all(file.exists(required_files))) {
  verified_cases_file <- file.path(
    project_dir, "data", "verified", "verified_plant_cases.csv"
  )
  cat(
    "No cluster-to-condition mapping is available. Add source-linked cases to:\n",
    verified_cases_file,
    "\nThen run Rscript main.R.\n",
    sep = ""
  )
  quit(save = "no", status = 1L)
}

mapping <- readRDS(mapping_file)
feature_model <- readRDS(feature_model_file)
kmeans_model <- readRDS(cluster_model_file)
text <- normalize_text(paste(question))
query_terms <- text2vec::word_tokenizer(text)[[1]]
if (!any(query_terms %in% feature_model$vocabulary$term)) {
  stop(
    "The question has no words in the clustering vocabulary. Try including ",
    "more symptom or plant terms, or add representative reviewed examples.",
    call. = FALSE
  )
}
cluster_id <- assign_to_kmeans_cluster(text, feature_model, kmeans_model)
cluster_index <- match(cluster_id, mapping$cluster_mapping$cluster_id)
if (is.na(cluster_index)) {
  cat("Unknown (cluster not mapped)\n")
} else {
  cat(mapping$cluster_mapping$predicted_condition[[cluster_index]], "\n")
}
