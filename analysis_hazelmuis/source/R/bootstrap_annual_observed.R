#' Compute Non-Parametric Bootstrap Mean and 95% CI per Year
#'
#' @description
#' Resamples rows within each year (with replacement) and calculates the
#' weighted count per available nest box (`sum(aantal) / sum(n_nestbuizen)`).
#'
#' @param df A `data.frame` containing `fjaar`, `aantal`, and `n_nestbuizen`.
#' @param year_val Character name of the year column (default `"fjaar"`).
#' @param n_boot Integer number of bootstrap replicates (default `1000`).
#' @param seed Random seed for reproducibility (default `123`).
#'
#' @return A `tibble` with `jaar`, `obs_mean`, `boot_lower`, and `boot_upper`.

bootstrap_annual_observed <- function(
  df,
  year_val = "fjaar",
  n_boot = 1000,
  seed = 123
) {
  set.seed(seed)
  
  df %>%
    dplyr::group_by(jaar = as.numeric(as.character(.data[[year_val]]))) %>%
    dplyr::group_modify(function(sub_df, key) {
      n <- nrow(sub_df)
      # Direct observed weighted mean
      obs_val <- sum(sub_df$aantal, na.rm = TRUE) /
        sum(sub_df$n_nestbuizen, na.rm = TRUE)
      
      # Bootstrap replicates
      boot_vals <- replicate(n_boot, {
        idx <- sample.int(n, size = n, replace = TRUE)
        sum(sub_df$aantal[idx], na.rm = TRUE) /
          sum(sub_df$n_nestbuizen[idx], na.rm = TRUE)
      })
      
      tibble::tibble(
        n_obs      = n,
        obs_mean   = obs_val,
        boot_lower = stats::quantile(boot_vals, 0.025, na.rm = TRUE),
        boot_upper = stats::quantile(boot_vals, 0.975, na.rm = TRUE)
      )
    }) %>%
    dplyr::ungroup()
}
