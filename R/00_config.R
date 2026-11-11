make_config <- function(project_dir) {
  data_dir <- file.path(project_dir, "data", "processed")
  review_dir <- file.path(project_dir, "data", "review")
  verified_data_dir <- file.path(project_dir, "data", "verified")
  tables_dir <- file.path(project_dir, "outputs", "tables")
  plots_dir <- file.path(project_dir, "outputs", "plots")
  models_dir <- file.path(project_dir, "outputs", "models")

  list(
    project_dir = project_dir,
    data_file = Sys.getenv(
      "PLANT_DATA_FILE",
      unset = file.path(project_dir, "stackexchange_gardening_raw_25k.csv")
    ),
    data_dir = data_dir,
    review_dir = review_dir,
    topic_review_file = file.path(
      review_dir,
      "06_topic_blind_review.csv"
    ),
    cluster_review_sample_file = file.path(
      data_dir,
      "06_cluster_labeled_300_with_sources.csv"
    ),
    verified_data_dir = verified_data_dir,
    verified_cases_file = file.path(
      verified_data_dir,
      "verified_plant_cases.csv"
    ),
    tables_dir = tables_dir,
    plots_dir = plots_dir,
    models_dir = models_dir,
    health_only = TRUE,
    health_regex = paste(
      c(
        "symptom", "disease", "infection", "infected", "pest", "aphid",
        "mite", "insect", "bug", "fung", "mildew", "mold", "rot",
        "blight", "canker", "rust", "spot", "lesion", "wilt", "wilting",
        "yellow", "chloros", "necrosis", "deficien", "nutrient", "discolor",
        "curl", "dying", "dead", "decay", "infestation", "damage", "stunt",
        "deform", "brown", "black", "white", "scale", "gall", "mosaic",
        "virus", "bacterial", "fungal", "droop", "chewed", "hole", "borer",
        "webbing", "sticky", "honeydew", "patch", "root rot"
      ),
      collapse = "|"
    ),
    min_term_count = 5L,
    max_document_proportion = 0.8,
    max_vocabulary = 12000L,
    svd_components = 80L,
    k_values = 2:20,
    kmeans_restarts = 10L,
    selection_sample_size = 2500L,
    dbscan_sample_size = 1500L,
    dbscan_min_points = c(5L, 10L, 15L),
    dbscan_epsilon_quantiles = c(0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95),
    dbscan_min_coverage = 0.4,
    hierarchical_sample_size = 800L,
    annotation_rows_per_cluster = 30L,
    cluster_review_sample_size = 300L,
    min_verified_examples_per_class = 5L,
    test_fraction = 0.2,
    random_seed = 123L
  )
}

make_directories <- function(config) {
  directories <- c(
    config$data_dir,
    config$review_dir,
    config$verified_data_dir,
    config$tables_dir,
    config$plots_dir,
    config$models_dir
  )
  for (directory in directories) {
    if (!dir.exists(directory) &&
        !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
      stop("Could not create project directory: ", directory, call. = FALSE)
    }
  }
}
