#' Heuristics for sizing plots whose dimensions depend on data volume.
#'
#' Used from chunk options via `!expr`:
#'
#' ```yaml
#' #| fig-height: !expr fig_height_categories(n_distinct(podcasts$show))
#' ```
#'
#' Plots grow naturally as new shows/episodes are added; nothing to maintain
#' per chunk.

#' Inches of height for a categorical axis (post `coord_flip` bar/box/errorbar).
#'
#' @param n Number of category levels that will appear on the axis.
#' @param per_row Inches per category. Default tuned for ~12pt y-axis text.
#' @param padding Inches added for title, subtitle, axis title, legend, caption.
#' @param min Floor in inches.
#' @param max Ceiling — knitr/Quarto chokes on absurdly large figures.
fig_height_categories <- function(n, per_row = 0.18, padding = 2.5,
                                  min = 4, max = 40) {
  pmin(max, pmax(min, ceiling(n * per_row + padding)))
}

#' Inches of height for a faceted plot.
#'
#' Computes rows from `n` facets and `ncol`, then height per row + padding.
#'
#' @param n Number of facet panels.
#' @param ncol Columns in the facet grid.
#' @param per_facet Inches per facet panel.
#' @param padding Inches added for title, subtitle, x-axis text, caption.
#' @param min Floor in inches.
#' @param max Ceiling.
fig_height_facets <- function(n, ncol = 3, per_facet = 2.2, padding = 1.5,
                              min = 5, max = 40) {
  nrow <- ceiling(n / ncol)
  pmin(max, pmax(min, ceiling(nrow * per_facet + padding)))
}

#' Inches of width for a plot with long categorical labels on the y-axis.
#'
#' Estimates from the longest label; adds a base for the plotting area.
#'
#' @param labels Character vector — the actual labels (or a sample).
#' @param plot_area Inches reserved for the data area itself.
#' @param char_width Inches per character (rough; depends on font).
#' @param min Floor in inches.
#' @param max Ceiling.
fig_width_for_labels <- function(labels, plot_area = 7, char_width = 0.08,
                                 min = 8, max = 16) {
  max_chars <- max(nchar(labels, type = "width"), na.rm = TRUE)
  pmin(max, pmax(min, ceiling(plot_area + max_chars * char_width)))
}
