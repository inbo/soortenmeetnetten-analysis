#' Refit the best model with year as a continuous predictor to estimate the linear trend (slope)
#'
#' Replaces the categorical year factor in the model formula and dataset with a
#' continuous numeric year variable (optionally centered on the first year for
#' numerical stability), refits the INLA model with the original distribution
#' family, and extracts the annual slope along with percentage change and effect
#' classification.
#'
#' @param best_model A list element from `best_models` containing `$name` and
#'   a fitted `inla` object in `$model`.
#' @param year_val Character string specifying the name of the year factor
#'   column in the original model data and formula (default `"fjaar"`).
#' @param center Logical indicating whether to center the continuous year
#'   variable on its minimum value (`year - min(year)`). Default is `TRUE`.
#'   Centering does not affect the slope estimate, only the intercept.
#' @param ... Additional arguments passed directly to
#'   [effectclass::add_effect_classification()] (e.g., `threshold`, `reference`,
#'   `coarse`).
#'
#' @return A single-row `data.frame` containing the linear year slope on the
#'   log-link scale (`mean`, `sd`, quantiles), the annual multiplicative factor
#'   (`factor_jaar`), annual percentage change (`pct_jaar`), and the `effect`
#'   classification column.

refit_year_slope <- function(best_model, year_val = "fjaar", center = TRUE, ...) {
  require("dplyr")
  require("tibble")
  require("INLA")
  require("stats")
  require("effectclass")
  
  m_orig  <- best_model$model
  fam     <- m_orig$.args$family
  form    <- m_orig$.args$formula
  df_orig <- m_orig$.args$data
  
  if (!year_val %in% names(df_orig)) {
    stop(sprintf("Column '%s' not found in model data.", year_val))
  }
  
  # 1. Convert factor year to numeric (optionally centered on start year)
  year_num <- as.numeric(as.character(df_orig[[year_val]]))
  if (center) {
    year_num <- year_num - min(year_num, na.rm = TRUE)
  }
  
  cont_var_name <- paste0(year_val, "_cont")
  df_temp <- df_orig %>%
    mutate(!!sym(cont_var_name) := year_num)
  
  # 2. Replace the categorical year term with the continuous term in the formula
  form_str  <- paste(deparse(form), collapse = " ")
  form_cont <- stats::as.formula(
    gsub(paste0("\\b", year_val, "\\b"), cont_var_name, form_str)
  )
  environment(form_cont) <- environment()
  
  # 3. Refit the model
  m_refit <- inla(
    formula = form_cont,
    family  = fam,
    data    = df_temp
  )
  
  # 4. Extract the slope row and add annual percentage change + effect classification
  res <- as.data.frame(summary(m_refit)$fixed) %>%
    rownames_to_column("param") %>%
    filter(param == cont_var_name) %>%
    mutate(
      term          = "jaar_continu",
      start_jaar    = min(as.numeric(as.character(df_orig[[year_val]])), na.rm = TRUE),
      gecentreerd   = center,
      factor_jaar   = exp(`0.5quant`),
      pct_jaar      = (exp(`0.5quant`) - 1) * 100,
      .before       = 1
    )
  
  if (length(list(...)) > 0) {
    res <- res %>%
      add_effect_classification(
        cl_columns = c("0.025quant", "0.975quant"),
        ...
      )
  }
  
  res %>%
    select(-any_of(c("param", "mode", "kld", "effect_code")))
}
