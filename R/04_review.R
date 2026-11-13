top_cluster_terms <- function(tfidf_matrix, vocabulary, assignments, k) {
  dplyr::bind_rows(lapply(seq_len(k), function(cluster_id) {
    rows <- which(assignments == cluster_id)
    means <- Matrix::colMeans(tfidf_matrix[rows, , drop = FALSE])
    selected <- order(means, decreasing = TRUE)[seq_len(min(12L, length(means)))]
    tibble::tibble(
      cluster = cluster_id,
      rank = seq_along(selected),
      term = vocabulary$term[selected],
      mean_tfidf = means[selected],
      document_count = length(rows)
    )
  }))
}

suggest_cluster_topic <- function(terms) {
  terms <- utils::head(terms, 5L)
  has <- function(pattern) {
    any(grepl(paste0("^(", pattern, ")"), terms, ignore.case = TRUE))
  }
  if (has("tomato")) return("Tomato plant questions")
  if (has("palm")) return("Palm plant questions")
  if (has("tree|trees|bark|branch|branches")) {
    return("Tree health, structure, and fruiting questions")
  }
  if (has("grass|lawn") && has("soil|seed|seeds|grow")) {
    return("Soil, lawn, and growing conditions")
  }
  if (has("soil") && has("plant|plants|seed|seeds")) {
    return("Soil and plant-growing questions")
  }
  if (has("flower|flowers|petals") && has("identify|name")) {
    return("Flower identification and flowering questions")
  }
  if (has("leaves|leaf") &&
      has("brown|yellow|white|black|spots|spot|dying|wilting")) {
    return("Leaf symptoms and plant-care questions")
  }
  if (has("lemon|citrus|orange")) return("Citrus plant questions")
  if (has("aloe")) return("Aloe plant questions")
  if (has("cactus|cacti")) return("Cactus and succulent questions")
  if (has("compost|bokashi|worms")) return("Composting questions")
  if (has("grass|lawn")) return("Lawn and weed questions")
  if (has("seed|seedling|germinat|sprout")) return("Seed and seedling questions")
  if (has("flower|flowers|petals")) return("Flowering plant questions")
  if (has("garden|bed|yard|landscape")) return("Garden and landscape questions")
  if (has("weed")) return("Lawn and weed questions")
  if (has("root|roots|cuttings|rot")) {
    return("Root, propagation, and soil questions")
  }
  if (has("fertiliz|nutrient|ph|potting")) {
    return("Soil, fertilizer, and plant nutrition questions")
  }
  if (has("water|watering|sun|dry|pot")) {
    return("Watering and general plant-care questions")
  }
  if (has("insect|bugs|pest") && has("disease|fungus|fungal")) {
    return("Mixed pest and disease identification questions")
  }
  if (has("spot|spots|disease|fungus|fungal")) {
    return("Leaf spots and possible disease questions")
  }
  if (has("winter|season|year|growing")) {
    return("Seasonal growth and plant-care questions")
  }
  if (has("dying|dead|save|recovery")) {
    return("General plant recovery and decline questions")
  }
  if (has("leaves|leaf|brown|yellow|discolor")) {
    return("Leaf discoloration and symptom questions")
  }
  if (has("insect|bugs|pest|white|black")) {
    return("Mixed pest and plant-symptom questions")
  }
  "General plant-health and gardening questions"
}

make_cluster_topic_labels <- function(questions, top_terms, config) {
  cluster_ids <- sort(unique(questions$kmeans_cluster))
  current <- dplyr::bind_rows(lapply(cluster_ids, function(cluster_id) {
    members <- questions[questions$kmeans_cluster == cluster_id, , drop = FALSE]
    representative <- members[1, , drop = FALSE]
    terms <- top_terms$term[top_terms$cluster == cluster_id]
    examples <- utils::head(members$title, 5L)
    tibble::tibble(
      cluster_key = paste(utils::head(terms, 5L), collapse = "|"),
      cluster_id = cluster_id,
      question_count = nrow(members),
      top_terms = paste(terms, collapse = ", "),
      example_titles = paste(examples, collapse = " | "),
      question_id = as.character(representative$question_id),
      symptom_text = as.character(representative$body_text),
      topic_label = suggest_cluster_topic(terms),
      reviewed_by = "",
      reviewed_on = "",
      review_notes = "",
      review_status = "suggested - review",
      current_run = TRUE
    )
  }))

  label_file <- config$cluster_topic_labels_file
  if (file.exists(label_file)) {
    previous <- readr::read_csv(label_file, show_col_types = FALSE)
    if (!"question_id" %in% names(previous) &&
        "example_1_question_id" %in% names(previous)) {
      previous$question_id <- previous$example_1_question_id
    }
    if (!"symptom_text" %in% names(previous) &&
        "example_1_symptom_text" %in% names(previous)) {
      previous$symptom_text <- previous$example_1_symptom_text
    }
    required <- c(
      "cluster_key", "cluster_id", "question_count", "top_terms",
      "example_titles", "question_id", "symptom_text", "topic_label",
      "reviewed_by", "reviewed_on", "review_notes", "review_status",
      "current_run"
    )
    if (!all(required %in% names(previous))) {
      stop("Cluster topic-label file is missing required review columns.",
           call. = FALSE)
    }
    if (anyDuplicated(previous$cluster_key)) {
      stop("Cluster topic-label file has duplicate cluster keys.", call. = FALSE)
    }
    previous <- previous |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c(
            "cluster_key", "top_terms", "example_titles", "question_id",
            "symptom_text", "topic_label", "reviewed_by", "reviewed_on",
            "review_notes", "review_status"
          )),
          as.character
        ),
        cluster_id = as.integer(cluster_id),
        question_count = as.integer(question_count),
        current_run = as.logical(current_run)
      )
    previous <- previous |>
      dplyr::select(dplyr::all_of(required))
    current <- current |>
      dplyr::left_join(
        previous |>
          dplyr::select(
            cluster_key,
            saved_question_id = question_id,
            saved_symptom_text = symptom_text,
            saved_topic_label = topic_label,
            saved_reviewed_by = reviewed_by,
            saved_reviewed_on = reviewed_on,
            saved_review_notes = review_notes,
            saved_review_status = review_status
          ),
        by = "cluster_key"
      ) |>
      dplyr::mutate(
        question_id = dplyr::coalesce(saved_question_id, question_id),
        symptom_text = dplyr::coalesce(saved_symptom_text, symptom_text),
        topic_label = dplyr::if_else(
          !is.na(saved_review_status) &
            saved_review_status == "reviewed" &
            !is.na(saved_topic_label) &
            nzchar(trimws(saved_topic_label)),
          saved_topic_label,
          topic_label
        ),
        reviewed_by = dplyr::coalesce(saved_reviewed_by, reviewed_by),
        reviewed_on = dplyr::coalesce(saved_reviewed_on, reviewed_on),
        review_notes = dplyr::coalesce(saved_review_notes, review_notes),
        review_status = dplyr::if_else(
          !is.na(saved_review_status) &
            saved_review_status == "reviewed",
          saved_review_status,
          review_status
        )
      ) |>
      dplyr::select(-dplyr::starts_with("saved_"))
    historical <- previous |>
      dplyr::filter(!cluster_key %in% current$cluster_key) |>
      dplyr::mutate(current_run = FALSE)
    current <- dplyr::bind_rows(current, historical)
  }

  readr::write_csv(current, label_file, quote = "all")
  current
}

write_cluster_review_sample <- function(questions, topic_labels, config) {
  cluster_ids <- sort(unique(questions$kmeans_cluster))
  base_size <- config$cluster_review_sample_size %/% length(cluster_ids)
  remainder <- config$cluster_review_sample_size %% length(cluster_ids)
  set.seed(config$random_seed)
  allocation <- stats::setNames(
    rep(base_size, length(cluster_ids)) +
      as.integer(seq_along(cluster_ids) <= remainder),
    cluster_ids
  )

  selected <- dplyr::bind_rows(lapply(cluster_ids, function(cluster_id) {
    members <- questions |>
      dplyr::filter(kmeans_cluster == cluster_id)
    sample_count <- min(allocation[[as.character(cluster_id)]], nrow(members))
    if (sample_count < allocation[[as.character(cluster_id)]]) {
      warning(
        "Cluster ", cluster_id, " has fewer questions than the requested ",
        "balanced sample allocation.",
        call. = FALSE
      )
    }
    dplyr::slice_sample(members, n = sample_count)
  }))

  selected <- selected |>
    dplyr::left_join(
      topic_labels |>
        dplyr::filter(current_run) |>
        dplyr::select(
          cluster_id,
          cluster_topic_label = topic_label
        ),
      by = c("kmeans_cluster" = "cluster_id")
    ) |>
    dplyr::transmute(
      question_id,
      source_site = dplyr::coalesce(as.character(site), "gardening"),
      source_url = as.character(link),
      source_dataset = basename(config$data_file),
      question_created_at = as.character(creation_date),
      cluster_id = kmeans_cluster,
      cluster_label = cluster_topic_label,
      cluster_algorithm = "k-means",
      cluster_label_type = "provisional topic suggestion",
      title,
      symptom_text = body_text,
      label_type = "unsupervised_cluster_assignment",
      diagnosis_verified = FALSE
    ) |>
    dplyr::arrange(cluster_id, question_id)

  if (nrow(selected) != config$cluster_review_sample_size) {
    stop(
      "Could not create the requested ", config$cluster_review_sample_size,
      "-row cluster sample; only ", nrow(selected), " rows were selected.",
      call. = FALSE
    )
  }
  if (anyNA(selected$cluster_label) || any(!nzchar(selected$cluster_label))) {
    stop("A sampled cluster is missing its topic label.", call. = FALSE)
  }
  counts <- table(selected$cluster_id)
  if (max(counts) - min(counts) > 1L) {
    stop("The cluster review sample is not balanced.", call. = FALSE)
  }

  current_assignments <- questions |>
    dplyr::transmute(
      question_id = as.character(question_id),
      cluster_id = as.character(kmeans_cluster)
    ) |>
    dplyr::left_join(
      topic_labels |>
        dplyr::filter(current_run) |>
        dplyr::transmute(
          cluster_id = as.character(cluster_id),
          topic_label = as.character(topic_label)
        ),
      by = "cluster_id"
    )
  if (anyNA(current_assignments$topic_label) ||
      any(!nzchar(current_assignments$topic_label))) {
    stop("A current cluster is missing its topic label.", call. = FALSE)
  }
  current_assignments <- current_assignments |>
    dplyr::transmute(
      question_id,
      cluster_id,
      cluster_label = paste0("Cluster ", cluster_id, ": ", topic_label),
      cluster_label_source = "current full-corpus k-means model"
    )

  write_topic_review_sheet(selected, config, current_assignments)

  output_path <- config$cluster_review_sample_file
  temporary_path <- tempfile(
    pattern = "cluster-sample-",
    tmpdir = dirname(output_path),
    fileext = ".csv"
  )
  readr::write_csv(selected, temporary_path, quote = "all")
  if (file.exists(output_path) && !file.remove(output_path)) {
    file.remove(temporary_path)
    stop("Could not replace the previous cluster sample: ", output_path,
         call. = FALSE)
  }
  if (!file.rename(temporary_path, output_path)) {
    stop("Could not move the generated cluster sample into place: ",
         output_path, call. = FALSE)
  }
  selected
}

write_topic_review_sheet <- function(
  selected,
  config,
  current_assignments = NULL
) {
  review_columns <- c(
    "human_topic_label",
    "human_label_notes",
    "reviewer",
    "reviewed_on",
    "review_status"
  )
  review <- selected |>
    dplyr::transmute(
      question_id = as.character(question_id),
      source_url = as.character(source_url),
      title = as.character(title),
      symptom_text = as.character(symptom_text),
      cluster_id = as.character(cluster_id),
      cluster_label = paste0(
        "Cluster ", cluster_id, ": ", as.character(cluster_label)
      ),
      cluster_label_source = "current full-corpus k-means model",
      human_topic_label = "",
      human_label_notes = "",
      reviewer = "",
      reviewed_on = "",
      review_status = "pending"
    )

  if (file.exists(config$topic_review_file)) {
    previous <- readr::read_csv(
      config$topic_review_file,
      show_col_types = FALSE,
      progress = FALSE
    )
    required <- c("question_id", review_columns)
    missing <- setdiff(required, names(previous))
    if (length(missing)) {
      stop(
        "Topic-review file is missing columns: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    if (anyDuplicated(previous$question_id)) {
      stop("Topic-review file has duplicate question IDs.", call. = FALSE)
    }
    for (column in c(
      "source_url", "title", "symptom_text", "cluster_id",
      "cluster_label", "cluster_label_source"
    )) {
      if (!column %in% names(previous)) previous[[column]] <- ""
    }
    previous <- previous |>
      dplyr::mutate(
        question_id = as.character(question_id),
        dplyr::across(
          dplyr::all_of(c(
            "source_url", "title", "symptom_text", "cluster_id",
            "cluster_label", "cluster_label_source", review_columns
          )),
          as.character
        )
      ) |>
      dplyr::select(
        question_id,
        "source_url", "title", "symptom_text", "cluster_id",
        "cluster_label", "cluster_label_source",
        dplyr::all_of(review_columns)
      )
    review <- dplyr::full_join(
      previous,
      review,
      by = "question_id",
      suffix = c("_previous", "_current")
    ) |>
      dplyr::transmute(
        question_id,
        source_url = dplyr::coalesce(
          dplyr::na_if(source_url_current, ""),
          source_url_previous,
          ""
        ),
        title = dplyr::coalesce(
          dplyr::na_if(title_current, ""),
          title_previous,
          ""
        ),
        symptom_text = dplyr::coalesce(
          dplyr::na_if(symptom_text_current, ""),
          symptom_text_previous,
          ""
        ),
        cluster_id = dplyr::coalesce(
          dplyr::na_if(cluster_id_current, ""),
          cluster_id_previous,
          ""
        ),
        cluster_label = dplyr::coalesce(
          dplyr::na_if(cluster_label_current, ""),
          cluster_label_previous,
          ""
        ),
        cluster_label_source = dplyr::coalesce(
          dplyr::na_if(cluster_label_source_current, ""),
          cluster_label_source_previous,
          ""
        ),
        human_topic_label = dplyr::coalesce(
          dplyr::na_if(human_topic_label_previous, ""),
          human_topic_label_current,
          ""
        ),
        human_label_notes = dplyr::coalesce(
          dplyr::na_if(human_label_notes_previous, ""),
          human_label_notes_current,
          ""
        ),
        reviewer = dplyr::coalesce(
          dplyr::na_if(reviewer_previous, ""),
          reviewer_current,
          ""
        ),
        reviewed_on = dplyr::coalesce(
          dplyr::na_if(reviewed_on_previous, ""),
          reviewed_on_current,
          ""
        ),
        review_status = dplyr::coalesce(
          dplyr::na_if(review_status_previous, ""),
          review_status_current,
          "pending"
        )
      ) |>
      dplyr::mutate(
        review_status = dplyr::if_else(
          nzchar(trimws(human_topic_label)) &
            tolower(trimws(review_status)) %in% c("", "pending"),
          "reviewed",
          dplyr::if_else(
            nzchar(review_status),
            review_status,
            "pending"
          )
        )
      )
  }

  review <- review |>
    dplyr::arrange(question_id)
  if (!is.null(current_assignments)) {
    required_assignments <- c(
      "question_id", "cluster_id", "cluster_label", "cluster_label_source"
    )
    missing_assignments <- setdiff(
      required_assignments,
      names(current_assignments)
    )
    if (length(missing_assignments)) {
      stop(
        "Current cluster assignments are missing columns: ",
        paste(missing_assignments, collapse = ", "),
        call. = FALSE
      )
    }
    if (anyDuplicated(current_assignments$question_id)) {
      stop("Current cluster assignments have duplicate question IDs.",
           call. = FALSE)
    }
    review <- review |>
      dplyr::left_join(
        current_assignments |>
          dplyr::mutate(question_id = as.character(question_id)) |>
          dplyr::rename(
            latest_cluster_id = cluster_id,
            latest_cluster_label = cluster_label,
            latest_cluster_label_source = cluster_label_source
          ),
        by = "question_id"
      ) |>
      dplyr::mutate(
        cluster_id = dplyr::coalesce(
          dplyr::na_if(latest_cluster_id, ""),
          cluster_id
        ),
        cluster_label = dplyr::coalesce(
          dplyr::na_if(latest_cluster_label, ""),
          cluster_label
        ),
        cluster_label_source = dplyr::coalesce(
          dplyr::na_if(latest_cluster_label_source, ""),
          cluster_label_source
        )
      ) |>
      dplyr::select(-dplyr::starts_with("latest_cluster_"))
  }
  readr::write_csv(review, config$topic_review_file, quote = "all")
  invisible(review)
}

write_cluster_summary <- function(questions, top_terms, config) {
  readr::write_csv(
    top_terms,
    file.path(config$tables_dir, "04_cluster_summary_top_terms.csv")
  )
  readr::write_csv(
    questions,
    file.path(config$data_dir, "04_questions_with_clusters.csv")
  )
}
