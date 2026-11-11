sample_indices <- function(size, maximum, seed) {
  set.seed(seed)
  if (size <= maximum) return(seq_len(size))
  sort(sample.int(size, maximum))
}

cluster_metrics <- function(features, labels) {
  labels <- as.integer(labels)
  valid <- !is.na(labels) & labels > 0L
  features <- features[valid, , drop = FALSE]
  labels <- labels[valid]
  cluster_sizes <- table(labels)
  cluster_count <- length(cluster_sizes)

  result <- list(
    cluster_count = cluster_count,
    assigned_fraction = mean(valid),
    minimum_cluster_size = if (cluster_count) min(cluster_sizes) else 0L,
    silhouette = NA_real_,
    davies_bouldin = NA_real_,
    calinski_harabasz = NA_real_
  )
  if (cluster_count < 2L || nrow(features) <= cluster_count) return(result)

  silhouette <- cluster::silhouette(labels, stats::dist(features))
  result$silhouette <- mean(silhouette[, "sil_width"])
  global_center <- colMeans(features)
  within <- 0
  between <- 0
  scatter <- numeric(cluster_count)
  names(scatter) <- names(cluster_sizes)
  centers <- matrix(NA_real_, nrow = cluster_count, ncol = ncol(features))
  for (position in seq_along(cluster_sizes)) {
    group <- names(cluster_sizes)[position]
    rows <- features[labels == as.integer(group), , drop = FALSE]
    center <- colMeans(rows)
    centers[position, ] <- center
    residual <- sweep(rows, 2, center, `-`)
    within <- within + sum(residual^2)
    between <- between + nrow(rows) * sum((center - global_center)^2)
    scatter[position] <- mean(sqrt(rowSums(residual^2)))
  }
  result$calinski_harabasz <- if (within > 0) {
    (between / (cluster_count - 1)) /
      (within / (nrow(features) - cluster_count))
  } else {
    NA_real_
  }

  center_distances <- as.matrix(stats::dist(centers))
  ratios <- matrix(0, nrow = cluster_count, ncol = cluster_count)
  for (i in seq_len(cluster_count)) {
    for (j in seq_len(cluster_count)) {
      if (i != j && center_distances[i, j] > 0) {
        ratios[i, j] <- (scatter[i] + scatter[j]) / center_distances[i, j]
      } else if (i != j) {
        ratios[i, j] <- Inf
      }
    }
  }
  result$davies_bouldin <- mean(apply(ratios, 1, max))
  result
}

fit_kmeans <- function(coordinates, config) {
  indices <- sample_indices(
    nrow(coordinates),
    config$selection_sample_size,
    config$random_seed
  )
  sample_features <- coordinates[indices, , drop = FALSE]
  candidate_rows <- lapply(config$k_values, function(k) {
    set.seed(config$random_seed + k)
    fit <- stats::kmeans(
      sample_features,
      centers = k,
      nstart = config$kmeans_restarts,
      iter.max = 1000L,
      algorithm = "Lloyd"
    )
    metrics <- cluster_metrics(sample_features, fit$cluster)
    data.frame(
      k = k,
      within_cluster_sum_squares = fit$tot.withinss,
      metrics,
      row.names = NULL
    )
  })
  candidates <- dplyr::bind_rows(candidate_rows)
  valid <- which(
    is.finite(candidates$silhouette) &
      candidates$minimum_cluster_size >= max(5L, ceiling(0.005 * length(indices)))
  )
  if (length(valid) == 0L) {
    stop("No k-means candidate met the minimum cluster-size check.", call. = FALSE)
  }
  selected_row <- valid[which.max(candidates$silhouette[valid])]
  selected_k <- candidates$k[[selected_row]]
  candidates$selected <- seq_len(nrow(candidates)) == selected_row

  set.seed(config$random_seed)
  fit <- stats::kmeans(
    coordinates,
    centers = selected_k,
    nstart = max(25L, config$kmeans_restarts),
    iter.max = 1000L,
    algorithm = "Lloyd"
  )
  evaluation_metrics <- cluster_metrics(
    coordinates[indices, , drop = FALSE],
    fit$cluster[indices]
  )
  list(
    fit = fit,
    selected_k = selected_k,
    selection_indices = indices,
    candidates = candidates,
    metrics = evaluation_metrics
  )
}

fit_dbscan <- function(coordinates, config) {
  indices <- sample_indices(
    nrow(coordinates),
    config$dbscan_sample_size,
    config$random_seed + 1L
  )
  features <- coordinates[indices, , drop = FALSE]
  fits <- list()
  rows <- list()
  position <- 0L
  for (min_points in config$dbscan_min_points) {
    neighbor_distances <- sort(dbscan::kNNdist(features, k = min_points))
    for (epsilon_quantile in config$dbscan_epsilon_quantiles) {
      epsilon <- unname(stats::quantile(neighbor_distances, epsilon_quantile))
      fit <- dbscan::dbscan(features, eps = epsilon, minPts = min_points)
      metrics <- cluster_metrics(features, fit$cluster)
      eligible <- metrics$cluster_count >= 2L &&
        metrics$assigned_fraction >= config$dbscan_min_coverage &&
        metrics$minimum_cluster_size >= min_points &&
        is.finite(metrics$silhouette)
      position <- position + 1L
      fits[[position]] <- list(fit = fit, min_points = min_points, epsilon = epsilon)
      rows[[position]] <- data.frame(
        min_points = min_points,
        epsilon_quantile = epsilon_quantile,
        epsilon = epsilon,
        eligible = eligible,
        metrics,
        row.names = NULL
      )
    }
  }

  tuning <- dplyr::bind_rows(rows)
  eligible <- which(tuning$eligible)
  if (length(eligible)) {
    selected <- eligible[which.max(tuning$silhouette[eligible])]
  } else {
    fallback <- which(tuning$epsilon_quantile == max(config$dbscan_epsilon_quantiles))
    selected <- fallback[[1]]
    warning(
      "No DBSCAN candidate met the cluster-count, coverage, and size checks; ",
      "the largest tested epsilon is reported as a diagnostic fallback.",
      call. = FALSE
    )
  }
  tuning$selected <- seq_len(nrow(tuning)) == selected
  list(
    fit = fits[[selected]]$fit,
    sample_indices = indices,
    min_points = fits[[selected]]$min_points,
    epsilon = fits[[selected]]$epsilon,
    metrics = tuning[selected, , drop = FALSE],
    tuning = tuning
  )
}

fit_hierarchical_sample <- function(coordinates, selected_k, config) {
  indices <- sample_indices(
    nrow(coordinates),
    config$hierarchical_sample_size,
    config$random_seed + 2L
  )
  features <- coordinates[indices, , drop = FALSE]
  tree <- stats::hclust(stats::dist(features), method = "ward.D2")
  labels <- stats::cutree(tree, k = selected_k)
  list(
    fit = tree,
    sample_indices = indices,
    labels = labels,
    metrics = cluster_metrics(features, labels)
  )
}

adjusted_rand_index <- function(first, second) {
  contingency <- table(first, second)
  choose_two <- function(x) x * (x - 1) / 2
  n <- sum(contingency)
  if (n < 2) return(NA_real_)
  sum_cells <- sum(choose_two(contingency))
  sum_rows <- sum(choose_two(rowSums(contingency)))
  sum_columns <- sum(choose_two(colSums(contingency)))
  total <- choose_two(n)
  expected <- sum_rows * sum_columns / total
  maximum <- (sum_rows + sum_columns) / 2
  if (maximum == expected) return(1)
  (sum_cells - expected) / (maximum - expected)
}

fit_stability <- function(coordinates, selected_k, sample_size, seed) {
  indices <- sample_indices(nrow(coordinates), sample_size, seed)
  features <- coordinates[indices, , drop = FALSE]
  assignments <- lapply(seq_len(5L), function(repetition) {
    set.seed(seed + repetition * 17L)
    stats::kmeans(
      features,
      centers = selected_k,
      nstart = 10L,
      iter.max = 1000L,
      algorithm = "Lloyd"
    )$cluster
  })
  pair_scores <- combn(seq_along(assignments), 2L, function(pair) {
    adjusted_rand_index(assignments[[pair[[1]]]], assignments[[pair[[2]]]])
  })
  list(
    mean_ari = mean(pair_scores, na.rm = TRUE),
    scores = pair_scores
  )
}

fit_all_clusters <- function(questions, features, lsa, config) {
  coordinates <- lsa$coordinates
  kmeans <- fit_kmeans(coordinates, config)
  dbscan <- fit_dbscan(coordinates, config)
  hierarchical <- fit_hierarchical_sample(coordinates, kmeans$selected_k, config)
  stability <- fit_stability(
    coordinates,
    kmeans$selected_k,
    config$selection_sample_size,
    config$random_seed + 3L
  )
  questions$kmeans_cluster <- kmeans$fit$cluster
  questions$dbscan_cluster <- NA_integer_
  questions$dbscan_cluster[dbscan$sample_indices] <- dbscan$fit$cluster
  list(
    questions = questions,
    coordinates = coordinates,
    kmeans = kmeans,
    dbscan = dbscan,
    hierarchical = hierarchical,
    stability = stability
  )
}
