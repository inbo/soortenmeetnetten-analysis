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


#' Plot INLA Fixed Effects with a Reference Level Indicator
#'
#' @description
#' Generates an effect plot with posterior median points, credible intervals, and 
#' threshold classification via \code{stat_effect()}. The reference level is 
#' highlighted using an open circular marker enclosing the letter "R".
#'
#' @param results A \code{data.frame}/\code{tibble} of INLA estimates, or a pre-formatted 
#'   list returned by \code{\link{prepare_plot_data}}.
#' @param effect_label A \code{character} string prefix of the target effect (e.g., \code{"fjaar"}). 
#'   Required if \code{results} is a raw data frame.
#' @param ref_label A \code{character} string or numeric value defining the reference level (e.g., \code{"2021"}). 
#'   Required if \code{results} is a raw data frame.
#' @param threshold A numeric vector of length 2 defining the lower and upper bounds for 
#'   the effect classification (defaults to \code{c(log(0.75), log(1.33))}).
#' @param point_size Numeric value defining the point size for both effect estimates 
#'   and the reference marker. Default is \code{8}.
#' @param text_size Numeric value defining the font size for the "R" glyph. Default is \code{4}.
#' @param y_limits Numeric vector of length 2 setting the primary log-scale limits. 
#'   Default is limits based on the data.
#'
#' @return A \code{ggplot} object.

plot_inla_effects <- function(
  results,
  effect_label = NULL,
  ref_label    = NULL,
  threshold    = c(log(0.75), log(1.33)),
  point_size   = 8,
  text_size    = 4,
  y_limits     = c(NA, NA)
) {
  # Normalize input structure
  if (is.data.frame(results)) {
    if (is.null(effect_label) || is.null(ref_label)) {
      stop("When 'results' is a data.frame, both 'effect_label' and 'ref_label' must be supplied.")
    }
    plot_data_results <- prepare_plot_data(
      summary_df   = results,
      effect_label = effect_label,
      ref_label    = ref_label
    )
  } else if (is.list(results) && all(c("plot_data", "ref_data") %in% names(results))) {
    plot_data_results <- results
  } else {
    stop("'results' must be either a data.frame or a list containing $plot_data and $ref_data.")
  }
  
  # --- Dynamic/Extended Scale Definitions ---
  
  # 1. Extended ratios: generates multipliers up to 15x (+1400%) and 20x (+1900%)
  # Negative side retains standard INBO ratios: -80%, -75%, -67%, -50%, -33%, -20%
  index_breaks <- function() {
    neg_fractions <- c(4/5, 3/4, 2/3, 1/2, 1/3, 1/5)
    neg_breaks    <- log(sort(1 - neg_fractions))    # Down to log(0.20) = -80%
    
    # Explicit positive multipliers to handle large surges:
    # 1.25x (+25%), 1.5x (+50%), 2x (+100%), 3x (+200%), 5x (+400%), 7.5x (+650%), 10x (+900%), 15x (+1400%)
    pos_multipliers <- c(1.25, 1.5, 2, 3, 5, 7.5, 10, 15, 20)
    pos_breaks      <- log(pos_multipliers)
    
    sort(unique(c(neg_breaks, 0, pos_breaks)))
  }
  
  # 2. Percentage labels on primary y-axis
  index_labels <- function(x) {
    sprintf("%+.0f%%", 100 * (exp(x) - 1))
  }
  
  # 3. Intermediate minor grid lines between adjacent breaks
  minor_breaks <- function() {
    brks <- index_breaks()
    m_breaks <- (brks[-length(brks)] + brks[-1]) / 2
    return(m_breaks)
  }
  
  # 4. Secondary axis: exact ratio/multipliers
  index_breaks_rev <- function() {
    exp(index_breaks())
  }
  
  index_labels_rev <- function(digits = 1) {
    vals <- exp(index_breaks())
    # Format cleanly: omit decimal places for large numbers (>= 10)
    ifelse(vals >= 10, sprintf("%.0f", vals), format(round(vals, digits), nsmall = digits))
  }
  
  # Build ggplot visualization
  ggplot2::ggplot(plot_data_results$plot_data, ggplot2::aes(x = .data$label)) +
    # Estimated effect levels with credible intervals
    stat_effect(
      ggplot2::aes(
        y    = .data[["0.5quant"]],
        ymin = .data[["0.025quant"]],
        ymax = .data[["0.975quant"]]
      ),
      threshold = threshold,
      size      = point_size
    ) +
    # Reference level circle container
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
    # Reference level 'R' glyph
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
}
