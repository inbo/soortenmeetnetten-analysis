#' Plot Raw Observations, Annual INLA Estimates, and Continuous Linear Trend
#'
#' @description
#' Visualizes counts per transect per available nest box across years by
#' layering raw observation ratios (\code{aantal / n_nestbuizen}), discrete
#' annual posterior estimates from a categorical-year INLA model, and the
#' continuous exponential trend curve with its 95% credible ribbon.
#'
#' @param raw_df A \code{data.frame} containing raw observations with columns
#'   \code{fjaar} (or \code{year_val}), \code{aantal}, and \code{n_nestbuizen}
#'   (plus \code{facet_var} if faceting is used).
#' @param annual_df A \code{data.frame} returned by \code{\link{extract_annual_response}}
#'   containing \code{jaar}, \code{0.025quant}, \code{0.5quant}, and \code{0.975quant}.
#' @param continuous_df A \code{data.frame} returned by \code{\link{extract_continuous_response}}
#'   containing \code{jaar}, \code{trend_median}, \code{trend_lwr}, and \code{trend_upr}.
#' @param year_val Character name of the year column in \code{raw_df} (default \code{"fjaar"}).
#' @param facet_var Optional character name of a grouping column (e.g., \code{"boscluster"})
#'   to facet the plot into panels (default \code{NULL}).
#' @param facet_scales Character string passed to \code{facet_wrap(scales = ...)}
#'   when \code{facet_var} is supplied (default \code{"free_y"}).
#' @param y_transform Character string specifying the y-axis scale transformation:
#'   \code{"none"} (default linear scale), \code{"pseudo_log"} (preserves zeros while
#'   compressing large outliers), or \code{"sqrt"} (square-root scale).
#' @param pseudo_log_sigma Numeric threshold controlling where \code{"pseudo_log"}
#'   transitions from linear to logarithmic (default \code{0.1}). Smaller values
#'   compress moderate counts more strongly.
#' @param y_max Optional numeric upper limit for the y-axis applied via
#'   \code{coord_cartesian()} so ribbons and error bars are not clipped.
#' @param jitter_width Numeric horizontal jitter width for raw points (default \code{0.12}).
#' @param raw_alpha Numeric transparency for raw observation points (default \code{0.35}).
#'
#' @return A \code{ggplot} object.

plot_response_trends <- function(
  raw_df,
  annual_df,
  continuous_df,
  year_val         = "fjaar",
  facet_var        = NULL,
  facet_scales     = "free_y",
  y_transform      = c("none", "pseudo_log", "sqrt"),
  pseudo_log_sigma = 0.1,
  y_max            = NA,
  jitter_width     = 0.12,
  raw_alpha        = 0.35
) {
  require("dplyr")
  require("ggplot2")
  require("scales")
  
  y_transform <- match.arg(y_transform)
  
  # Prepare raw observations on the response scale
  raw_plot <- raw_df %>%
    dplyr::mutate(
      jaar         = as.numeric(as.character(.data[[year_val]])),
      obs_per_buis = .data$aantal / .data$n_nestbuizen
    )
  
  # Base ggplot layers
  p <- ggplot2::ggplot() +
    # 1. Continuous trend 95% credible ribbon
    ggplot2::geom_ribbon(
      data = continuous_df,
      ggplot2::aes(x = .data$jaar, ymin = .data$trend_lwr, ymax = .data$trend_upr, fill = "Lineaire trend (95% CI)"),
      alpha = 0.18
    ) +
    # 2. Continuous trend median line
    ggplot2::geom_line(
      data = continuous_df,
      ggplot2::aes(x = .data$jaar, y = .data$trend_median, colour = "Lineaire trend (INLA)"),
      linewidth = 1
    ) +
    # 3. Raw observations (horizontal jitter only so zeros remain exact)
    ggplot2::geom_jitter(
      data   = raw_plot,
      ggplot2::aes(x = .data$jaar, y = .data$obs_per_buis, colour = "Ruwe waarnemingen"),
      width  = jitter_width,
      height = 0,
      alpha  = raw_alpha,
      size   = 1.8
    ) +
    # 4. Discrete annual INLA estimates with 95% credible intervals
    ggplot2::geom_errorbar(
      data = annual_df,
      ggplot2::aes(x = .data$jaar, ymin = .data[["0.025quant"]], ymax = .data[["0.975quant"]], colour = "INLA jaarmodel"),
      width     = 0.15,
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = annual_df,
      ggplot2::aes(x = .data$jaar, y = .data[["0.5quant"]], colour = "INLA jaarmodel"),
      size = 3.5
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Ruwe waarnemingen"     = "grey45",
        "INLA jaarmodel"        = "#2b5c8f",
        "Lineaire trend (INLA)" = "#d95f02"
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c("Lineaire trend (95% CI)" = "#d95f02")
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(annual_df$jaar))) +
    ggplot2::labs(
      x      = "Jaar",
      y      = "Aantal per transect per vrije nestbuis",
      colour = "",
      fill   = ""
    ) +
    ggplot2::theme_minimal()
  
  # Apply y-axis transformation while keeping 0 intact
  if (y_transform == "pseudo_log") {
    p <- p + ggplot2::scale_y_continuous(
      trans  = scales::pseudo_log_trans(sigma = pseudo_log_sigma, base = 10),
      breaks = c(0, 0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10)
    )
  } else if (y_transform == "sqrt") {
    p <- p + ggplot2::scale_y_sqrt()
  }
  
  # Enforce non-negative y-axis and optional upper zoom without clipping ribbons
  p <- p + ggplot2::coord_cartesian(ylim = c(0, y_max))
  
  # Optional faceting (e.g., by boscluster)
  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_grid(
      stats::as.formula(paste(facet_var, "~", ".")),
      scales = facet_scales
    )
  }
  
  p
}
