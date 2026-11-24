#' Prepare INLA Model Summaries for Effect Visualization
#'
#' @description
#' Filters the specified fixed effects from an INLA model summary table and 
#' appends an explicit reference level set to zero on the log-link scale.
#'
#' @param summary_df A \code{data.frame} or \code{tibble} containing INLA parameter 
#'   estimates (typically extracted via \code{summary(m)$fixed}), containing at least 
#'   the columns \code{effect}, \code{mean}, \code{0.5quant}, \code{0.025quant}, 
#'   and \code{0.975quant}.
#' @param effect_label A \code{character} string representing the prefix of the fixed 
#'   effect to extract (e.g., \code{"fjaar"}, \code{"boscluster"}, or \code{"telperiode"}).
#' @param ref_label A \code{character} string or numeric value denoting the reference 
#'   level label (e.g., \code{"2021"}, \code{"boscluster1"}, or \code{"T1"}).
#'
#' @return A named \code{list} with two components:
#' \describe{
#'   \item{plot_data}{A \code{tibble} of estimated factor levels and their posterior quantiles.}
#'   \item{ref_data}{A single-row \code{tibble} for the reference level with estimate = 0.}
#' }

prepare_plot_data <- function(summary_df, effect_label, ref_label) {
  # 1. Filter and clean estimated effect levels
  plot_data <- summary_df %>%
    dplyr::filter(grepl(paste0("^", effect_label), .data$param)) %>%
    dplyr::mutate(label = gsub(paste0("^", effect_label), "", .data$param))
  
  # 2. Construct explicit baseline row at log-effect = 0
  ref_data <- tibble::tibble(
    label = as.character(ref_label),
    mean  = 0
  )
  
  list(
    plot_data = plot_data,
    ref_data  = ref_data
  )
}


#' Prepare Consecutive Year Contrasts for Effect Visualization
#'
#' @description
#' Formats a data frame of consecutive year-over-year contrasts (produced by
#' \code{\link{refit_consecutive_years}}) into the list structure required by
#' \code{\link{plot_inla_effects}}. Because each contrast already compares
#' year \eqn{t} against year \eqn{t-1}, an empty reference table is returned so
#' that no baseline "R" marker is drawn on the x-axis.
#'
#' @param consecutive_df A \code{data.frame} or \code{tibble} containing consecutive
#'   year estimates, including the columns \code{contrast}, \code{mean},
#'   \code{0.5quant}, \code{0.025quant}, and \code{0.975quant}.
#'
#' @return A named \code{list} with two components:
#' \describe{
#'   \item{plot_data}{A \code{tibble} with \code{label} ordered chronologically as a factor.}
#'   \item{ref_data}{An empty zero-row \code{tibble} matching the structure expected by \code{\link{plot_inla_effects}}.}
#' }
prepare_consecutive_plot_data <- function(consecutive_df) {
  # Preserve chronological order of contrast labels on the x-axis
  plot_data <- consecutive_df %>%
    dplyr::mutate(
      label = factor(.data$contrast, levels = .data$contrast)
    )
  
  # Create an empty reference data frame so no 'R' marker is rendered
  ref_data <- tibble::tibble(
    label = factor(character(), levels = levels(plot_data$label)),
    mean  = numeric()
  )
  
  list(
    plot_data = plot_data,
    ref_data  = ref_data
  )
}


#' Prepare Annual Slope Summaries for Effect Visualization
#'
#' @description
#' Formats the combined full-model and cluster-level slope table (where
#' \code{boscluster = NA} represents the global model) into the structure
#' required by \code{\link{plot_inla_effects}}, adding a grouping column for
#' faceting.
#'
#' @param slope_df A \code{data.frame} or \code{tibble} containing annual slope
#'   estimates with a \code{boscluster} column (\code{NA} for the global model).
#' @param total_label Character string used to label the global model row where
#'   \code{boscluster} is \code{NA} (default \code{"Totaal"}).
#'
#' @return A named \code{list} with \code{plot_data} and an empty \code{ref_data}.
prepare_slope_plot_data <- function(slope_df, total_label = "") {
  plot_data <- slope_df %>%
    dplyr::mutate(
      facet_groep = factor(
        ifelse(is.na(.data$boscluster), "Volledig model", "Per boscluster"),
        levels = c("Volledig model", "Per boscluster")
      ),
      label = ifelse(is.na(.data$boscluster), total_label, as.character(.data$boscluster)),
      label = factor(.data$label, levels = unique(.data$label))
    )
  
  ref_data <- tibble::tibble(
    label = factor(character(), levels = levels(plot_data$label)),
    mean  = numeric()
  )
  
  list(
    plot_data = plot_data,
    ref_data  = ref_data
  )
}


#' Plot INLA Fixed Effects with Optional Horizontal Layout and Faceting
#'
#' @param results A \code{data.frame}/\code{tibble} of INLA estimates, consecutive
#'   contrasts, or annual slopes, or a pre-formatted list.
#' @param effect_label Character prefix of the target effect (for standard summaries).
#' @param ref_label Character or numeric reference level label (for standard summaries).
#' @param threshold Numeric vector of length 2 defining the lower and upper bounds
#'   for effect classification.
#' @param point_size Point size for estimates and reference marker (default \code{8}).
#' @param text_size Font size for the "R" glyph (default \code{4}).
#' @param y_limits Numeric vector of length 2 setting the effect-axis log-scale limits.
#' @param horizontal Logical; if \code{TRUE}, plots error bars horizontally using
#'   \code{coord_flip()} and reverses category order top-to-bottom (default \code{FALSE}).
#'
#' @return A \code{ggplot} object.
plot_inla_effects <- function(
    results,
    effect_label = NULL,
    ref_label    = NULL,
    threshold    = c(log(0.75), log(1.33)),
    point_size   = 8,
    text_size    = 4,
    y_limits     = c(NA, NA),
    horizontal   = FALSE
) {
  # 1. Normalize input structure
  if (is.data.frame(results)) {
    if ("boscluster" %in% names(results) && "term" %in% names(results)) {
      plot_data_results <- prepare_slope_plot_data(results)
    } else if ("contrast" %in% names(results)) {
      plot_data_results <- prepare_consecutive_plot_data(results)
    } else {
      if (is.null(effect_label) || is.null(ref_label)) {
        stop("When 'results' is a standard summary data.frame, both 'effect_label' and 'ref_label' must be supplied.")
      }
      plot_data_results <- prepare_plot_data(
        summary_df   = results,
        effect_label = effect_label,
        ref_label    = ref_label
      )
    }
  } else if (is.list(results) && all(c("plot_data", "ref_data") %in% names(results))) {
    plot_data_results <- results
  } else {
    stop("'results' must be either a data.frame or a list containing $plot_data and $ref_data.")
  }
  
  # Reverse factor order when horizontal so top row appears at the top
  if (horizontal) {
    lvls <- rev(levels(factor(plot_data_results$plot_data$label)))
    plot_data_results$plot_data <- plot_data_results$plot_data %>%
      dplyr::mutate(label = factor(.data$label, levels = lvls))
    if (nrow(plot_data_results$ref_data) > 0) {
      plot_data_results$ref_data <- plot_data_results$ref_data %>%
        dplyr::mutate(label = factor(.data$label, levels = lvls))
    }
  }
  
  # 2. Scale definitions
  index_breaks <- function() {
    neg_fractions   <- c(9/10, 4/5, 3/4, 2/3, 1/2, 1/3, 1/5)
    neg_breaks      <- log(sort(1 - neg_fractions))
    pos_multipliers <- c(1.25, 1.5, 2, 3, 5, 7.5, 10, 15, 20, 25)
    pos_breaks      <- log(pos_multipliers)
    sort(unique(c(neg_breaks, 0, pos_breaks)))
  }
  
  index_labels <- function(x) {
    sprintf("%+.0f%%", 100 * (exp(x) - 1))
  }
  
  minor_breaks <- function() {
    brks <- index_breaks()
    (brks[-length(brks)] + brks[-1]) / 2
  }
  
  index_breaks_rev <- function() {
    exp(index_breaks())
  }
  
  index_labels_rev <- function(digits = 2) {
    vals <- exp(index_breaks())
    ifelse(
      vals >= 10,
      sprintf("%.0f", vals),
      sprintf(paste0("%.", digits, "f"), vals)
    )
  }
  
  # 3. Base plot
  p <- ggplot2::ggplot(plot_data_results$plot_data, ggplot2::aes(x = .data$label)) +
    stat_effect(
      ggplot2::aes(
        y    = .data[["0.5quant"]],
        ymin = .data[["0.025quant"]],
        ymax = .data[["0.975quant"]]
      ),
      threshold = threshold,
      size      = point_size
    ) +
    ggplot2::geom_point(
      data        = plot_data_results$ref_data,
      ggplot2::aes(y = .data$mean),
      shape       = 21,
      size        = point_size,
      color       = "black",
      fill        = "white",
      stroke      = 1.1,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data        = plot_data_results$ref_data,
      ggplot2::aes(y = .data$mean),
      label       = "R",
      size        = text_size,
      fontface    = "bold",
      hjust       = 0.5,
      vjust       = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::labs(
      y      = "Procentuele verandering",
      x      = "",
      colour = "Effect classificatie"
    ) +
    ggplot2::scale_y_continuous(
      breaks       = index_breaks(),
      labels       = index_labels,
      minor_breaks = minor_breaks(),
      limits       = y_limits,
      sec.axis     = ggplot2::sec_axis(
        ~ exp(.),
        breaks = index_breaks_rev(),
        labels = index_labels_rev(),
        name   = "Proportionele verandering"
      )
    ) +
    ggplot2::theme_minimal()
  
  # 4. Orientation and faceting
  if (horizontal) {
    p <- p +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::coord_flip()
  }
  
  if ("facet_groep" %in% names(plot_data_results$plot_data)) {
    if (horizontal) {
      p <- p +
        ggplot2::facet_grid(
          facet_groep ~ .,
          scales = "free_y",
          space  = "free_y",
          switch = "y"
        ) +
        ggplot2::theme(
          strip.placement   = "outside",
          strip.text.y.left = ggplot2::element_text(face = "bold")
        )
    } else {
      p <- p +
        ggplot2::facet_grid(. ~ facet_groep, scales = "free_x", space = "free_x")
    }
  }
  
  p
}
