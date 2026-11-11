required_columns <- c("question_id", "title", "body")

clean_html <- function(text) {
  text[is.na(text)] <- ""
  vapply(text, function(value) {
    if (!nzchar(value)) {
      return("")
    }
    document <- xml2::read_html(paste0("<html><body>", value, "</body></html>"))
    xml2::xml_text(document, trim = TRUE) |>
      paste(collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

normalize_text <- function(text) {
  text <- tolower(text)
  text <- gsub("https?://\\S+|www\\.\\S+", " ", text, perl = TRUE)
  text <- gsub("[^a-z\\s]", " ", text, perl = TRUE)
  text <- gsub("\\b[a-z]\\b", " ", text, perl = TRUE)
  gsub("\\s+", " ", trimws(text), perl = TRUE)
}

load_questions <- function(config) {
  if (!file.exists(config$data_file)) {
    stop(
      "The original data CSV was not found: ", config$data_file,
      ". Set PLANT_DATA_FILE to its location.",
      call. = FALSE
    )
  }

  questions <- readr::read_csv(
    config$data_file,
    show_col_types = FALSE,
    progress = FALSE
  )
  missing_columns <- setdiff(required_columns, names(questions))
  if (length(missing_columns) > 0) {
    stop(
      "The input CSV is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(questions$question_id)) {
    stop("The input CSV contains duplicate question IDs.", call. = FALSE)
  }

  questions <- questions |>
    dplyr::mutate(
      question_id = as.character(question_id),
      title = dplyr::coalesce(as.character(title), ""),
      body = dplyr::coalesce(as.character(body), ""),
      body_text = clean_html(body),
      raw_text = paste(title, body_text),
      text = normalize_text(raw_text),
      health_candidate = grepl(
        config$health_regex,
        paste(title, body_text),
        ignore.case = TRUE,
        perl = TRUE
      )
    ) |>
    dplyr::filter(nzchar(text), !is.na(question_id))

  if (config$health_only) {
    questions <- dplyr::filter(questions, health_candidate)
  }
  if (nrow(questions) < 10L) {
    stop("Fewer than 10 usable questions remain after text filtering.", call. = FALSE)
  }
  questions
}

write_data_audit <- function(questions, config) {
  original <- readr::read_csv(
    config$data_file,
    show_col_types = FALSE,
    progress = FALSE
  )
  profile <- tibble::tibble(
    column = names(original),
    type = vapply(original, function(column) paste(class(column), collapse = "/"), character(1)),
    missing_values = vapply(original, function(column) sum(is.na(column)), integer(1)),
    distinct_values = vapply(original, dplyr::n_distinct, integer(1))
  )
  overview <- tibble::tibble(
    metric = c(
      "Input rows", "Input columns", "Unique question IDs",
      "Rows retained after text cleaning", "Plant-health candidates",
      "Share retained as health candidates"
    ),
    value = c(
      nrow(original), ncol(original), dplyr::n_distinct(original$question_id),
      nrow(questions), sum(questions$health_candidate),
      sprintf("%.1f%%", 100 * mean(questions$health_candidate))
    )
  )

  readr::write_csv(profile, file.path(config$tables_dir, "01_column_profile.csv"))
  readr::write_csv(overview, file.path(config$tables_dir, "01_dataset_overview.csv"))
  preprocessing <- tibble::tibble(
    metric = c("Clean questions", "Health candidates", "Health candidate share"),
    value = c(
      nrow(questions),
      sum(questions$health_candidate),
      sprintf("%.1f%%", 100 * mean(questions$health_candidate))
    ),
    interpretation = c(
      "Questions with non-empty normalized title/body text.",
      "Rows matching the broad plant-health candidate keywords.",
      "A candidate filter rate, not measured health-topic precision or recall."
    )
  )
  readr::write_csv(
    preprocessing,
    file.path(config$tables_dir, "02_preprocessing_overview.csv")
  )
  readr::write_csv(
    questions,
    file.path(config$data_dir, "02_cleaned_health_candidates.csv")
  )
  invisible(list(profile = profile, overview = overview))
}

write_eda_outputs <- function(questions, config) {
  scores <- if ("score" %in% names(questions)) {
    suppressWarnings(as.numeric(questions$score))
  } else {
    rep(NA_real_, nrow(questions))
  }
  score_plot <- ggplot2::ggplot(
    tibble::tibble(score = scores[is.finite(scores)]),
    ggplot2::aes(x = score)
  ) +
    ggplot2::geom_histogram(bins = 30, fill = "#327a65", color = "white") +
    ggplot2::labs(title = "Question score distribution", x = "Score", y = "Questions") +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(
    file.path(config$plots_dir, "02_question_score_distribution.png"),
    score_plot,
    width = 8,
    height = 5,
    dpi = 150
  )

  if ("creation_date" %in% names(questions)) {
    dates <- suppressWarnings(as.Date(questions$creation_date))
    year_counts <- tibble::tibble(year = format(dates, "%Y")) |>
      dplyr::filter(!is.na(year), year != "NA") |>
      dplyr::count(year)
    year_plot <- ggplot2::ggplot(
      year_counts,
      ggplot2::aes(x = year, y = n, group = 1)
    ) +
      ggplot2::geom_line(color = "#327a65", linewidth = 1) +
      ggplot2::geom_point(color = "#327a65", size = 2) +
      ggplot2::labs(title = "Questions by year", x = "Year", y = "Questions") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    ggplot2::ggsave(
      file.path(config$plots_dir, "02_questions_by_year.png"),
      year_plot,
      width = 9,
      height = 5,
      dpi = 150
    )
  }
}
