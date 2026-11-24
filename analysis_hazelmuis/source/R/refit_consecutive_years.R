#' Refit the best model for each consecutive reference year (year_t vs year_{t-1})
#'
#' Iteratively updates the reference level of the specified year factor variable
#' to each preceding year, refits the selected INLA model with the exact same
#' formula and distribution family, and extracts the consecutive year-over-year
#' contrasts along with their effect classifications.
#'
#' @param best_model A list element from `best_models` containing `$name` and
#'   a fitted `inla` object in `$model`.
#' @param year_val Character string specifying the name of the year factor
#'   column in the model data and formula (default `"fjaar"`).
#' @param ... Additional arguments passed directly to
#'   [effectclass::add_effect_classification()] (e.g., `threshold`, `reference`,
#'   `coarse`).
#'
#' @return A `data.frame` containing the consecutive year contrasts (`contrast`,
#'   `jaar_van`, `jaar_naar`), posterior summary statistics (`mean`, `sd`,
#'   quantiles), and the `effect` classification column.
refit_consecutive_years <- function(best_model, year_val = "fjaar", ...) {
  require("dplyr")
  require("purrr")
  require("tibble")
  require("INLA")
  require("stats")
  require("effectclass")
  
  m_orig  <- best_model$model
  fam     <- m_orig$.args$family
  form    <- m_orig$.args$formula
  df_orig <- m_orig$.args$data
  
  # Re-bind formula environment so stats::model.frame() is resolved inside inla()
  environment(form) <- environment()
  
  # Sort all unique years in chronological order as character strings
  all_years <- as.character(sort(unique(as.numeric(as.character(df_orig[[year_val]])))))
  
  # Iterate over each consecutive year pair (ref_yr -> target_yr)
  results <- map2_dfr(
    all_years[-length(all_years)],
    all_years[-1],
    function(ref_yr, target_yr) {
      # 1. Relevel the reference year using an explicit character string
      df_temp <- df_orig %>%
        mutate(!!sym(year_val) := relevel(factor(.data[[year_val]]), ref = as.character(ref_yr)))
      
      # 2. Refit the exact same model family and formula
      m_refit <- inla(
        formula = form,
        family  = fam,
        data    = df_temp
      )
      
      # 3. Extract the fixed-effect row for the target year
      target_param <- paste0(year_val, target_yr)
      
      as.data.frame(summary(m_refit)$fixed) %>%
        rownames_to_column("param") %>%
        filter(param == target_param) %>%
        mutate(
          contrast  = paste0(target_yr, " vs ", ref_yr),
          jaar_van  = ref_yr,
          jaar_naar = target_yr,
          .before   = 1
        )
    }
  )
  
  # Add effect classification forwarding additional arguments via ...
  results %>%
    add_effect_classification(
      cl_columns = c("0.025quant", "0.975quant"),
      ...
    ) %>%
    select(-any_of(c("param", "mode", "kld", "effect_code")))
}
