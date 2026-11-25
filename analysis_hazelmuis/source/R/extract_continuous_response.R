#' Extract Continuous Annual Trend Curve and Credible Ribbon on the Response Scale
#'
#' @description
#' Refits the best INLA model with year as a centered continuous predictor,
#' draws posterior samples, and evaluates the expected count per transect per
#' available nest box (\code{n_nestbuizen = 1}) across a continuous sequence of
#' years on the original response scale.
#'
#' @param best_model A list element from `best_models` containing `$model`.
#' @param year_val Character name of the categorical year column in the original
#'   model data (default `"fjaar"`).
#' @param n_samples Integer number of posterior samples to draw (default `1000`).
#' @param n_grid Integer number of evenly spaced points along the year range
#'   (default `100`).
#' @param marginalize_others Logical; if `TRUE` (default), averages over the
#'   levels of other fixed factors in the model (e.g., `boscluster`, `telperiode`).
#' @param seed Optional integer random seed for reproducibility (default `NULL`).
#'
#' @return A `tibble` with `jaar`, `trend_median`, `trend_lwr`, and `trend_upr`.

extract_continuous_response <- function(
  best_model,
  year_val           = "fjaar",
  n_samples          = 1000,
  n_grid             = 100,
  marginalize_others = TRUE,
  seed               = NULL
) {
  require("dplyr")
  require("tibble")
  require("INLA")
  require("stats")
  
  m_orig  <- best_model$model
  fam     <- m_orig$.args$family
  form    <- m_orig$.args$formula
  df_orig <- m_orig$.args$data
  
  if (!year_val %in% names(df_orig)) {
    stop(sprintf("Column '%s' not found in model data.", year_val))
  }
  
  # 1. Create centered continuous year variable
  year_num <- as.numeric(as.character(df_orig[[year_val]]))
  min_yr   <- min(year_num, na.rm = TRUE)
  max_yr   <- max(year_num, na.rm = TRUE)
  
  cont_var_name <- paste0(year_val, "_cont")
  df_temp <- df_orig %>%
    dplyr::mutate(!!rlang::sym(cont_var_name) := year_num - min_yr)
  
  # 2. Replace factor year with continuous year in formula and refit with config = TRUE
  form_str  <- paste(deparse(form), collapse = " ")
  form_cont <- stats::as.formula(
    gsub(paste0("\\b", year_val, "\\b"), cont_var_name, form_str)
  )
  environment(form_cont) <- environment()
  
  m_cont <- INLA::inla(
    formula         = form_cont,
    family          = fam,
    data            = df_temp,
    control.compute = list(config = TRUE)
  )
  
  # 3. Draw posterior samples
  if (!is.null(seed)) set.seed(seed)
  samples <- INLA::inla.posterior.sample(n = n_samples, result = m_cont)
  
  fixed_names <- rownames(m_cont$summary.fixed)
  latent_rows <- paste0(fixed_names, ":1")
  
  beta_mat <- vapply(samples, function(s) {
    as.numeric(s$latent[latent_rows, 1])
  }, numeric(length(fixed_names)))
  rownames(beta_mat) <- fixed_names
  
  # 4. Marginalize over other fixed factors (identical to extract_annual_response)
  other_offset <- rep(0, n_samples)
  if (marginalize_others) {
    other_vars <- setdiff(
      all.vars(m_cont$.args$formula)[-1],
      c(cont_var_name, "transect", "n_nestbuizen")
    )
    for (v in other_vars) {
      if (v %in% names(df_temp) && is.factor(df_temp[[v]])) {
        lvls          <- levels(df_temp[[v]])
        lvl_terms     <- paste0(v, lvls[-1])
        present_terms <- intersect(lvl_terms, fixed_names)
        if (length(present_terms) > 0) {
          other_offset <- other_offset +
            colSums(beta_mat[present_terms, , drop = FALSE]) / length(lvls)
        }
      }
    }
  }
  
  # 5. Evaluate expected count across continuous year grid on response scale
  yr_grid <- seq(min_yr, max_yr, length.out = n_grid)
  dt_grid <- yr_grid - min_yr
  
  pred_mat <- vapply(dt_grid, function(dt) {
    eta <- beta_mat["(Intercept)", ] + dt * beta_mat[cont_var_name, ] + other_offset
    exp(eta)
  }, numeric(n_samples)) # rows = samples, cols = grid points
  
  tibble::tibble(
    jaar         = yr_grid,
    trend_median = apply(pred_mat, 2, stats::quantile, probs = 0.500),
    trend_lwr    = apply(pred_mat, 2, stats::quantile, probs = 0.025),
    trend_upr    = apply(pred_mat, 2, stats::quantile, probs = 0.975)
  )
}
