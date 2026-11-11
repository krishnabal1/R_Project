script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1]])
} else {
  "evaluate_topic_labels.R"
}
project_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
setwd(project_dir)

required_packages <- c(
  "readr", "dplyr", "tibble", "ggplot2", "stringr", "text2vec",
  "stopwords", "Matrix", "irlba", "cluster", "e1071"
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

source(file.path(project_dir, "R", "00_config.R"))
source(file.path(project_dir, "R", "02_features.R"))
source(file.path(project_dir, "R", "03_clustering.R"))
source(file.path(project_dir, "R", "07_topic_evaluation.R"))
config <- make_config(project_dir)
make_directories(config)
evaluate_holdout_topic_model(config)
evaluate_topic_svm(config)
