#' Extract Annual Posterior Estimates on the Original Response Scale
#'
#' @description
#' Draws posterior samples from a fitted INLA model and computes the expected
#' count per transect per available nest box (`n_nestbuizen = 1`, i.e., offset = 0)
#' on the original response scale (`exp(eta)`) for each year.
#'
#' @param best_model A list element from `best_models` containing `$name` and
#'   a fitted `inla` object in `$model`.
#' @param year_val Character string specifying the name of the year factor
#'   column in the model data and formula (default `"fjaar"`).
#' @param n_samples Integer number of posterior samples to draw (default `1000`).
#' @param marginalize_others Logical; if `TRUE` (default), averages over the
#'   levels of other fixed factors in the model (e.g. `boscluster`, `telperiode`)
#'   rather than conditioning solely on their reference levels.
#' @param seed Optional integer random seed for reproducibility (default `NULL`).
#'
#' @return A `tibble` with one row per year on the original response scale
#'   containing `jaar`, `mean`, `sd`, `0.025quant`, `0.5quant`, and `0.975quant`.

extract_annual_response <- function(
  best_model,
  year_val           = "fjaar",
  n_samples          = 1000,
  marginalize_others = TRUE,
  seed               = NULL
) {
  require("dplyr")
  require("tibble")
  require("INLA")
  require("stats")
  
  m_orig  <- best_model$model
  df_orig <- m_orig$.args$data
  
  if (!year_val %in% names(df_orig)) {
    stop(sprintf("Column '%s' not found in model data.", year_val))
  }
  
  # 1. Ensure posterior sampling configuration is enabled
  if (is.null(m_orig$misc$configs) || is.null(m_orig$misc$configs$config)) {
    form <- m_orig$.args$formula
    environment(form) <- environment()
    m_orig <- inla(
      formula         = form,
      family          = m_orig$.args$family,
      data            = df_orig,
      control.compute = list(config = TRUE)
    )
  }
  
  # 2. Draw posterior samples
  if (!is.null(seed)) set.seed(seed)
  samples <- inla.posterior.sample(n = n_samples, result = m_orig)
  
  # 3. Extract latent fixed-effect draws per sample
  fixed_names <- rownames(m_orig$summary.fixed)
  latent_rows <- paste0(fixed_names, ":1")
  
  beta_mat <- vapply(samples, function(s) {
    as.numeric(s$latent[latent_rows, 1])
  }, numeric(length(fixed_names)))
  rownames(beta_mat) <- fixed_names
  
  # 4. Optional adjustment for other fixed factors (average across all levels)
  other_offset <- rep(0, n_samples)
  if (marginalize_others) {
    other_vars <- setdiff(all.vars(m_orig$.args$formula)[-1], c(year_val, "transect", "n_nestbuizen"))
    for (v in other_vars) {
      if (v %in% names(df_orig) && is.factor(df_orig[[v]])) {
        lvls <- levels(df_orig[[v]])
        lvl_terms <- paste0(v, lvls[-1])
        present_terms <- intersect(lvl_terms, fixed_names)
        if (length(present_terms) > 0) {
          # Average contribution across all k levels (reference level contributes 0)
          other_offset <- other_offset + colSums(beta_mat[present_terms, , drop = FALSE]) / length(lvls)
        }
      }
    }
  }
  
  # 5. Compute expected count per available nest box on original scale: exp(eta)
  year_levels <- as.character(sort(unique(as.numeric(as.character(df_orig[[year_val]])))))
  ref_year    <- levels(factor(df_orig[[year_val]]))[1]
  
  resp_draws <- lapply(year_levels, function(yr) {
    param_name <- paste0(year_val, yr)
    eta <- if (yr == ref_year || !param_name %in% fixed_names) {
      beta_mat["(Intercept)", ] + other_offset
    } else {
      beta_mat["(Intercept)", ] + beta_mat[param_name, ] + other_offset
    }
    exp(eta)
  })
  names(resp_draws) <- year_levels
  draws_mat <- do.call(rbind, resp_draws)
  
  # 6. Summarize posterior distributions on the original scale
  tibble::tibble(
    jaar         = as.numeric(year_levels),
    mean         = apply(draws_mat, 1, mean),
    sd           = apply(draws_mat, 1, sd),
    `0.025quant` = apply(draws_mat, 1, quantile, probs = 0.025),
    `0.5quant`   = apply(draws_mat, 1, quantile, probs = 0.500),
    `0.975quant` = apply(draws_mat, 1, quantile, probs = 0.975)
  )
}
