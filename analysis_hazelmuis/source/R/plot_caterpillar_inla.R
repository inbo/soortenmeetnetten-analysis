#' Plot Caterpillar Plot for INLA Random Effects
#'
#' @description
#' Generates a caterpillar plot for the random intercepts of transects from an INLA model.
#' Transects are ranked by their posterior median effect. A zero line indicates the 
#' cluster-average baseline.
#'
#' @param inla_model An object of class \code{inla} containing fitted random effects.
#' @param random_var A \code{character} string specifying the random effect variable name 
#'   in the formula (default: \code{"transect"}).
#' @param point_size Numeric size for the posterior median point. Default is \code{0.7}.
#' @param cluster_lookup Optional data.frame or named vector to add and color by `boscluster`.
#'
#' @return A \code{ggplot} object.
#' @export
plot_caterpillar_inla <- function(
  inla_model,
  random_var = "transect",
  point_size = 0.7,
  cluster_lookup = NULL
) {
  
  if (!random_var %in% names(inla_model$summary.random)) {
    stop(sprintf("Random effect '%s' not found in model$summary.random", random_var))
  }
  
  # 1. Extract random effect estimates
  re_df <- inla_model$summary.random[[random_var]] %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      transect = ID,
      median   = `0.5quant`,
      lower    = `0.025quant`,
      upper    = `0.975quant`
    )
  
  # 2. Optionally join cluster information if provided
  if (!is.null(cluster_lookup)) {
    re_df <- re_df %>%
      dplyr::left_join(cluster_lookup, by = "transect")
  }
  
  # 3. Order transects by their posterior median estimate
  re_df <- re_df %>%
    dplyr::mutate(
      transect = stats::reorder(transect, median)
    )
  
  # 4. Visualization
  p <- ggplot2::ggplot(re_df, ggplot2::aes(x = median, y = transect)) +
    # Reference zero line (mean log-effect)
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    
    # Credible interval and point estimate
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = lower, xmax = upper),
      size = point_size
    ) +
    
    ggplot2::labs(
      x = "Random effect (log-schaal)",
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_blank()
    )
  
  return(p)
}
