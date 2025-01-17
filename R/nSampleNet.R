nSampleNet <- function(nsamples, samplesize, data, method) {
  calculate_ci <- function(x) {
    se <- sd(x) / sqrt(length(x))  # Standard Error
    mean_x <- mean(x)
    lower <- mean_x - 1.96 * se
    upper <- mean_x + 1.96 * se
    c(mean = mean_x, lower = lower, upper = upper)
  }

  get_global_properties <- function(global_results, metrics) {
    result_list <- lapply(metrics, function(metric) {
      sapply(global_results, function(res) res[["Global Properties"]][[metric]])
    })
    names(result_list) <- metrics
    result_list
  }

  calculate_metrics <- function(global_properties) {
    lapply(global_properties, calculate_ci)
  }

  summarize_results <- function(ci_results, metric_labels) {
    data.frame(
      Metric = metric_labels,
      Mean = sapply(ci_results, `[[`, "mean"),
      Lower_CI = sapply(ci_results, `[[`, "lower"),
      Upper_CI = sapply(ci_results, `[[`, "upper")
    )
  }

  if (!method %in% c("ggmModSelect", "BGGM1")) {
    stop("Invalid method. Choose either 'ggmModSelect' or 'BGGM1'.")
  }

  global_results <- vector("list", nsamples)

  for (i in seq_len(nsamples)) {
    sampled_data <- dplyr::sample_n(data, samplesize, replace = (method == "BGGM1"))

    if (method == "ggmModSelect") {
      gRand <- estimateNetwork(sampled_data, default = "ggmModSelect")
      graph <- gRand$graph
    } else if (method == "BGGM1") {
      gRand <- BGGM::estimate(sampled_data, type = "mixed")
      gRand <- BGGM::select(gRand)
      graph <- gRand$pcor_adj
    }

    qRand <- qgraph::qgraph(graph, layout = "spring", directed = FALSE, theme = "TeamFortress", DoNotPlot = TRUE)
    iRand <- igraph::as.igraph(qRand, attributes = TRUE)
    global_results[[i]] <- netDesc(iRand)
  }

  metrics <- c("Nodes", "Edges", "Negative.Edges", "Isolated.Nodes", "Components",
               "Ave.Path", "Density", "Global.Transitivity", "Connectivity", "Average.Degree")

  metric_labels <- c("nodes", "edges", "negative edges", "isolated nodes", "components",
                     "average path length", "density", "global transitivity", "connectivity", "average degree")

  global_properties <- get_global_properties(global_results, metrics)
  ci_results <- calculate_metrics(global_properties)
  summary_df <- summarize_results(ci_results, metric_labels)

  list("Mean Values and CI" = summary_df, "Complete Results" = global_results)
}
