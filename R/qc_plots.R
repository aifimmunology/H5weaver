#' Generate a QC Histogram Plot for a single metric
#'
#' @param meta A data.frame containing metadata
#' @param column A character object specifying the metadata to display
#' @param name_x A character object specifying a name to display on the x-axis
#' @param log_x A logical indicating whether or not to log10-scale the x-axis. Default is TRUE.
#' @param fill A character object specifying the color to use for for the histogram. Default is "dodgerblue".
#' @param target A numeric value for a target line to display on the x-axis
#'
#' @return a ggplot2 plot object
#' @export
qc_hist_plot <- function(meta,
                         column = "n_reads",
                         name_x = "N Reads per Cell",
                         log_x = TRUE,
                         fill = "dodgerblue",
                         target = 2e4) {

  if(log_x) {
    binwidth <- 0.02
  } else {
    binwidth <- 1e3
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = meta[[column]]),
                            binwidth = binwidth,
                            fill = fill) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = median(meta[[column]])),
                        linetype = "dashed",
                        color = "#000000") +
    ggplot2::geom_text(ggplot2::aes(x = median(meta[[column]]) * .95,
                                    y = 1450,
                                    label = paste0("median: ", median(meta[[column]]))),
                       color = "#000000",
                       hjust = 1,
                       vjust = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = target),
                        linetype = "dashed",
                        color = "#808080") +
    ggplot2::geom_text(ggplot2::aes(x = target * 1.05,
                                    y = 1450,
                                    label = paste0(target / 1e3, "k")),
                       color = "#808080",
                       hjust = 0,
                       vjust = 1) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous("N Cells", limits = c(0, 1500)) +
    ggplot2::theme_bw()

  if(log_x) {
    p <- p +
      ggplot2::scale_x_log10(paste0("log10(",name_x,")"),
                             limits = c(1e2, 2.5e5),
                             breaks = c(1e2, 5e2, 1e3, 5e3, 1e4, 5e4, 1e5, 2.5e5))
  } else {
    p <- p +
      ggplot2::scale_x_continuous(name_x,
                                  limits = c(1e2, 2.5e5),
                                  breaks = c(1e2, 5e2, 1e3, 5e3, 1e4, 5e4, 1e5, 2.5e5))
  }

  p
}

#' Generate a QC Scatter Plot for a pair of metrics
#'
#' @param meta A data.frame containing metadata
#' @param column_x A character object specifying the metadata to display on the x-axis
#' @param name_x A character object specifying a name to display on the x-axis
#' @param column_y A character object specifying the metadata to display on the y-axis
#' @param name_y A character object specifying a name to display on the y-axis
#' @param log_x A logical indicating whether or not to log10-scale the x-axis. Default is TRUE.
#' @param log_y A logical indicating whether or not to log10-scale the y-axis. Default is TRUE.
#' @param show_targets A logical indicating whether or not to plot lines displaying ratios of values. Default is TRUE.
#' @param color A character object specifying the color to use for for the points. Default is "dodgerblue".
#'
#' @return a ggplot2 plot object
#' @export
qc_scatter_plot <- function(meta,
                            column_x = "n_reads",
                            name_x = "N Reads per Cell",
                            column_y = "n_umis",
                            name_y = "N UMIs per Cell",
                            log_x = TRUE,
                            log_y = TRUE,
                            show_targets = TRUE,
                            color = "dodgerblue") {

  target_lines <- data.frame(x = c(1e2, 2e2, 4e2, 8e2),
                             xend = c(2.5e5, 2.5e5, 2.5e5, 2.5e5),
                             y = c(1e2, 1e2, 1e2, 1e2),
                             yend = c(2.5e5, 1.25e5, 6.25e4, 3.125e4),
                             group = c("1:1","1:2","1:4", "1:8"))

  p <- ggplot2::ggplot()

  if(show_targets) {
    p <- p +
      ggplot2::geom_segment(data = target_lines,
                            ggplot2::aes(x = x, xend = xend,
                                         y = y, yend = yend,
                                         group = group),
                            linetype = "dashed")
  }

  p <- p +
    ggplot2::geom_text(data = target_lines,
                       ggplot2::aes(x = xend * 0.9,
                                    y = yend,
                                    label = group),
                       angle = 45,
                       hjust = 1,
                       vjust = 0,
                       size = 3) +
    ggplot2::geom_point(ggplot2::aes(x = meta[[column_x]],
                                     y = meta[[column_y]]),
                        alpha = 0.2,
                        size = 0.2,
                        color = color) +
    ggplot2::scale_color_identity() +
    ggplot2::theme_bw()

  if(log_x) {
    p <- p +
      ggplot2::scale_x_log10(paste0("log10(",name_x,")"),
                             limits = c(1e2, 2.5e5),
                             breaks = c(1e2, 5e2, 1e3, 5e3, 1e4, 5e4, 1e5, 2.5e5),
                             labels = c("100", "500", "1k", "5k", "10k", "50k", "100k", "250k"))
  } else {
    p <- p +
      ggplot2::scale_x_continuous(name_x)
  }

  if(log_y) {
    p <- p +
      ggplot2::scale_y_log10(paste0("log10(",name_y,")"),
                             limits = c(1e2, 2.5e5),
                             breaks = c(1e2, 5e2, 1e3, 5e3, 1e4, 5e4, 1e5, 2.5e5),
                             labels = c("100", "500", "1k", "5k", "10k", "50k", "100k", "250k"))
  } else {
    p <- p +
      ggplot2::scale_y_continous(name_y)
  }

  p
}

#' Generate a QC Violin Plot for a metric, grouped by a categorical metadata column.
#'
#' @param meta A data.frame containing metadata
#' @param category_x A character object specifying the metadata to use for grouping on the x-axis
#' @param name_x A character object specifying a name to display on the x-axis
#' @param column_y A character object specifying the metadata to display on the y-axis
#' @param name_y A character object specifying a name to display on the y-axis
#' @param log_y A logical indicating whether or not to log10-scale the y-axis. Default is TRUE.
#' @param fill A character object specifying the fill color to use for for the violins. Default is "skyblue".
#'
#' @return a ggplot2 plot object
#' @export
qc_violin_plot <- function(meta,
                           category_x = "well_id",
                           name_x = "Well ID",
                           column_y = "n_reads",
                           name_y = "N Reads per Cell",
                           log_y = TRUE,
                           fill = "skyblue") {

  tidy_x <- rlang::parse_expr(category_x)
  tidy_y <- rlang::parse_expr(column_y)

  meta <- as.data.table(meta)
  q_table <- meta[, .(q_25 = quantile(get(column_y), 0.25),
                      q_50 = quantile(get(column_y), 0.50),
                      q_75 = quantile(get(column_y), 0.75)),
                  by = get(category_x)]
  names(q_table)[1] <- category_x

  global_median <- median(meta[[column_y]])

  p <- ggplot2::ggplot() +
    ggplot2::geom_violin(ggplot2::aes(x = as.factor(meta[[category_x]]),
                                      y = meta[[column_y]]),
                         fill = fill,
                         color = "#808080") +
    ggplot2::geom_errorbar(data = q_table,
                           ggplot2::aes(x = as.factor(!!tidy_x),
                                        ymin = q_25,
                                        ymax = q_75),
                           width = 0.25) +
    ggplot2::geom_point(data = q_table,
                        ggplot2::aes(x = as.factor(!!tidy_x),
                                     y = q_50)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = global_median),
                        linetype = "dashed") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_discrete(name_x) +
    ggplot2::theme_bw() +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 0,
                                     vjust = 0.3))


  if(log_y) {
    p <- p +
      ggplot2::scale_y_log10(paste0("log10(",name_y,")"),
                             limits = c(1e2, 2.5e5),
                             breaks = c(1e2, 5e2, 1e3, 5e3, 1e4, 5e4, 1e5, 2.5e5),
                             labels = c("100", "500", "1k", "5k", "10k", "50k", "100k", "250k"))
  } else {
    p <- p +
      ggplot2::scale_y_continous(name_y)
  }

  p

}

#' Generate a QC Barplot for a metric at multiple cutoffs
#'
#' @param meta A data.frame containing metadata
#' @param column_x A character object specifying the metadata to use plotting
#' @param name_x A character object specifying a name to display on the x-axis
#' @param cutoffs A numeric vector specifying one or more cutoffs to use. Default is c(500, 750, 1000).
#' @param fill A character object specifying the fill color to use for for the violins. Default is "purple".
#'
#' @return a ggplot2 plot object
#' @export
qc_cutoff_barplot <- function(meta,
                              column_x = "n_umis",
                              name_x = "N UMIs",
                              cutoffs = c(500, 750, 1000),
                              fill = "purple") {

  cutoff_counts <- data.frame(cutoff = cutoffs,
                              n_cells = sapply(cutoffs,
                                               function(x) {
                                                 sum(meta[[column_x]] > x)
                                               }))

  cutoff_counts <- cutoff_counts[order(cutoff_counts$cutoff),]

  ggplot2::ggplot() +
    ggplot2::geom_bar(data = cutoff_counts,
                      ggplot2::aes(x = as.factor(cutoff),
                                   y = n_cells),
                      stat = "identity",
                      fill = fill) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous("N Cells",
                                limits = c(0, 3e4),
                                expand = c(0, 0)) +
    ggplot2::scale_x_discrete(paste0(name_x, " Cutoff")) +
    ggplot2::theme_bw()

}
