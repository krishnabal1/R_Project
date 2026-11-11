fit_text_features <- function(text, config) {
  iterator <- text2vec::itoken(
    text,
    tokenizer = text2vec::word_tokenizer,
    progressbar = FALSE
  )
  vocabulary <- text2vec::create_vocabulary(
    iterator,
    stopwords = stopwords::stopwords("en")
  )
  vocabulary <- text2vec::prune_vocabulary(
    vocabulary,
    term_count_min = config$min_term_count,
    doc_proportion_max = config$max_document_proportion,
    vocab_term_max = config$max_vocabulary
  )
  if (nrow(vocabulary) < 10L) {
    stop("Text preprocessing retained fewer than 10 vocabulary terms.", call. = FALSE)
  }

  vectorizer <- text2vec::vocab_vectorizer(vocabulary)
  dtm <- text2vec::create_dtm(iterator, vectorizer)
  tfidf <- text2vec::TfIdf$new()
  matrix <- tfidf$fit_transform(dtm)
  list(
    vocabulary = vocabulary,
    vectorizer = vectorizer,
    tfidf = tfidf,
    matrix = matrix
  )
}

fit_lsa <- function(tfidf_matrix, config) {
  component_count <- min(
    config$svd_components,
    nrow(tfidf_matrix) - 1L,
    ncol(tfidf_matrix) - 1L
  )
  if (component_count < 2L) {
    stop("Not enough documents or terms for SVD.", call. = FALSE)
  }

  decomposition <- if (component_count >=
                       0.4 * min(nrow(tfidf_matrix), ncol(tfidf_matrix))) {
    base::svd(
      as.matrix(tfidf_matrix),
      nu = component_count,
      nv = component_count
    )
  } else {
    irlba::irlba(
      tfidf_matrix,
      nv = component_count,
      nu = component_count,
      maxit = 1000
    )
  }
  decomposition$u <- decomposition$u[, seq_len(component_count), drop = FALSE]
  decomposition$v <- decomposition$v[, seq_len(component_count), drop = FALSE]
  decomposition$d <- decomposition$d[seq_len(component_count)]
  coordinates <- sweep(decomposition$u, 2, decomposition$d, `*`)
  row_norm <- sqrt(rowSums(coordinates^2))
  row_norm[row_norm == 0] <- 1
  coordinates <- coordinates / row_norm
  list(
    coordinates = coordinates,
    decomposition = decomposition,
    component_count = component_count,
    explained_variance = decomposition$d^2 / sum(decomposition$d^2)
  )
}
