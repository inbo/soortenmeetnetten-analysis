library(n2khab)
library(tidyverse)
library(git2rdata)
library(sf)

get_characteristics_smp <- function(species_group = NULL,
                                    file = "metadata/meetnetkarakteristieken",
                                    path = fileman_up("soortenmeetnetten-data")) {
  
  characteristics_smp <- read_vc(file = file, 
                                root = path)
  
  if (!is.null(species_group)) {
    
    if (species_group %in% characteristics_smp$soortgroep) {
      
      characteristics_smp <- characteristics_smp %>%
        filter(.data$soortgroep == species_group)
      
    }
    
    else {
      
      warning(str_c("Selecteer een van volgende soortgroepen: ", 
                    str_c(unique(characteristics_smp$soortgroep), collapse = ", ")))
      
    }
  }
  
  return(characteristics_smp)
  
}

get_visits_smp <- function(species_group = NULL,
                                    file = "raw/bezoeken",
                                    path = fileman_up("soortenmeetnetten-data")) {
  
  visits_smp <- read_vc(file = file, 
                                 root = path)
  
  if (!is.null(species_group)) {
    
    if (species_group %in% visits_smp$soortgroep) {
      
      visits_smp <- visits_smp %>%
        filter(.data$soortgroep == species_group)
      
    }
    
    else {
      
      warning(str_c("Selecteer een van volgende soortgroepen: ", 
                    str_c(unique(visits_smp$soortgroep), collapse = ", ")))
      
    }
  }
  
  return(visits_smp)
  
}

get_counts_smp <- function(species_group = NULL,
                           count_aggregation = NULL,
                           file = "raw/aantallen",
                           path = fileman_up("soortenmeetnetten-data")) {
  
  counts_smp <- read_vc(file = file, 
                        root = path)
  
  if (!is.null(species_group)) {
    
    if (species_group %in% counts_smp$soortgroep) {
      
      counts_smp <- counts_smp %>%
        filter(.data$soortgroep == species_group)
      
    }
    
    else {
      
      warning(str_c("Selecteer een van volgende soortgroepen: ", 
                    str_c(unique(counts_smp$soortgroep), collapse = ", ")))
      
    }
  }
  
  if (!is.null(count_aggregation)) {
    
    if (count_aggregation == "individuals") {
      
      counts_smp <- counts_smp %>%
        mutate(multiply_count = ifelse(activiteit == "copula" | (activiteit == "eiafzettend" & geslacht == "U"), 2, 1)) %>%
        group_by(soortgroep, meetnet, protocol, jaar, datum, doy, sublocatie, visit_id, niet_geteld, checklist, species_id, soort_nl, soort_wet, primaire_soort, sample_id) %>%
        summarise(aantal = sum(aantal * multiply_count)) %>%
        ungroup()
      
    } else if (count_aggregation == "lifestage") {
      
      counts_smp <- counts_smp %>%
        mutate(multiply_count = ifelse(activiteit == "copula" | (activiteit == "eiafzettend" & geslacht == "U"), 2, 1)) %>%
        group_by(soortgroep, meetnet, protocol, jaar, datum, doy, sublocatie, visit_id, niet_geteld, checklist, species_id, soort_nl, soort_wet, primaire_soort, sample_id, levensstadium) %>%
        summarise(aantal = sum(aantal * multiply_count)) %>%
        ungroup()
      
    }
    
  }
  
  return(counts_smp)
  
}

get_observers_smp <- function(species_group = NULL,
                           file = "raw/tellers",
                           path = fileman_up("soortenmeetnetten-data")) {
  
  observers_smp <- read_vc(file = file, 
                        root = path)
  
  if (!is.null(species_group)) {
    
    if (species_group %in% observers_smp$soortgroep) {
      
      observers_smp <- observers_smp %>%
        filter(.data$soortgroep == species_group)
      
    }
    
    else {
      
      warning(str_c("Selecteer een van volgende soortgroepen: ", 
                    str_c(unique(observers_smp$soortgroep), collapse = ", ")))
      
    }
  }
  
  return(observers_smp)
  
}

get_covariates_smp <- function(species_group = NULL,
                              file = "raw/covariabele",
                              path = fileman_up("soortenmeetnetten-data")) {
  
  covariates_smp <- read_vc(file = file, 
                           root = path)
  
  if (!is.null(species_group)) {
    
    if (species_group %in% covariates_smp$soortgroep) {
      
      covariates_smp <- covariates_smp %>%
        filter(.data$soortgroep == species_group)
      
    }
    
    else {
      
      warning(str_c("Selecteer een van volgende soortgroepen: ", 
                    str_c(unique(covariates_smp$soortgroep), collapse = ", ")))
      
    }
  }
  
  return(covariates_smp)
  
}

get_locations_smp <- function(species_group = NULL,
                              only_active = TRUE,
                              only_sample = FALSE,
                                    file = "raw/meetnetten_locaties.gpkg",
                                    path = fileman_up("soortenmeetnetten-data")) {
  
  locations_smp <- st_read(dsn = file.path(path, file),
                           layer = "locaties", quiet = TRUE)
  
  if (!is.null(species_group)) {
    
    if (species_group %in% locations_smp$soortgroep) {
      locations_smp <- locations_smp %>%
        filter(.data$soortgroep == species_group)
    }
    
    else {
      warning(str_c("Selecteer een van volgende soortgroepen: ", 
                    str_c(unique(locations_smp$soortgroep), collapse = ", ")))
    }
  }
  
  if (only_sample) {
    locations_smp <- locations_smp %>%
      filter(.data$is_sample)
  }
  
  if (only_active) {
    locations_smp <- locations_smp %>%
      filter(.data$is_active)
  }
  
  return(locations_smp)
  
}

add_tag_utm5 <- function(locations_smp,
                     method = "largest_overlap",
                     file = "gis/utm5",
                     path = fileman_up("soortenmeetnetten-data")) {
  
  utm5 <- st_read(dsn = file.path(path, file),
                           layer = "utm5", quiet = TRUE) %>%
    select(tag_utm5 = TAG) %>%
    st_transform(crs = st_crs(locations_smp))
  
  if (method == "centroid") {

      suppressMessages(
        suppressWarnings(
          centroid_smp_utm5 <- st_centroid(locations_smp) %>%
            st_join(utm5)  %>%
            st_drop_geometry() %>%
            select(meetnet, locatie, tag_utm5)
          )
        )
      
      locations_smp_utm5 <- locations_smp %>%
        left_join(centroid_smp_utm5, by = c("meetnet", "locatie"))

  } else if (method == "largest_overlap") {
    
    suppressMessages(
      suppressWarnings(
        locations_smp_utm5 <- locations_smp %>%
        st_join(utm5, largest = TRUE)
      )
    )
    
  }
  
  return(locations_smp_utm5)
}

read_utm5 <- function(file = "gis/utm5",
                     path = fileman_up("soortenmeetnetten-data")) {
  
  utm5 <- st_read(dsn = file.path(path, file),
                  layer = "utm5", quiet = TRUE) %>%
    select(tag_utm5 = TAG) 
  
  return(utm5)
}
 
calculate_monitoring_effort <- function(species_group = NULL, aggregation_level = "meetnet"){
  
  visits <- get_visits_smp(species_group)
  observers <- get_observers_smp(species_group)
  
  if (aggregation_level == "meetnet") {
    
    visits_summary <- visits %>%
      group_by(soortgroep, meetnet, jaar) %>%
      summarise(n_tellingen = n_distinct(visit_id)) %>%
      ungroup()
    
    locations_summary <- visits %>%
      group_by(soortgroep, meetnet, jaar) %>%
      summarise(n_telgebieden = n_distinct(locatie)) %>%
      ungroup()
    
    observers_summary <- observers %>%
      group_by(soortgroep, meetnet, jaar) %>%
      summarise(n_tellers = n_distinct(naam_teller)) %>%
      ungroup()
    
  } else if (aggregation_level %in% c("species_group", "soortgroep")) {
    visits_summary <- visits %>%
      group_by(soortgroep, jaar) %>%
      summarise(n_tellingen = n_distinct(visit_id)) %>%
      ungroup()
    
    locations_summary <- visits %>%
      group_by(soortgroep, jaar) %>%
      summarise(n_telgebieden = n_distinct(locatie)) %>%
      ungroup()
    
    observers_summary <- observers %>%
      group_by(soortgroep, jaar) %>%
      summarise(n_tellers = n_distinct(naam_teller)) %>%
      ungroup()
  }
  
  monitoring_effort_long <- bind_rows(
    visits_summary %>%
      rename(aantal = n_tellingen) %>%
      mutate(monitoringsinspanning = "aantal tellingen"),
    locations_summary %>%
      rename(aantal = n_telgebieden) %>%
      mutate(monitoringsinspanning = "aantal tellocaties"),
    observers_summary %>%
      rename(aantal = n_tellers) %>%
      mutate(monitoringsinspanning = "aantal tellers")
  ) %>%
    arrange(soortgroep) %>%
    select(soortgroep, monitoringsinspanning, everything())
  
  return(monitoring_effort_long)
  
}

get_summary_counts <- function(species_group = NULL) {
  
  counts_visit <- get_counts_smp(species_group = species_group) %>%
    group_by(soortgroep, meetnet, protocol, locatie, visit_id, jaar, datum, soort_nl, soort_wet, primaire_soort, geslacht, activiteit, levensstadium, checklist) %>%
    summarise(aantal = sum(aantal, na.rm = TRUE)) %>%
    ungroup()
  
  summary_counts_details <- counts_visit %>%
    group_by(primaire_soort, soortgroep, jaar, soort_nl, soort_wet, geslacht, activiteit, levensstadium) %>%
    summarise(aantal_totaal = sum(aantal),
              aantal_gemiddeld = round(mean(aantal),1),
              aantal_maximum = max(aantal),
              bezoeken = n_distinct(visit_id)) %>%
    ungroup()
  
  summary_counts_lifestage <- get_counts_smp(species_group = species_group, count_aggregation = "lifestage") %>%
    group_by(primaire_soort, soortgroep, jaar, soort_nl, soort_wet, levensstadium) %>%
    summarise(aantal_totaal = sum(aantal),
              aantal_gemiddeld = round(mean(aantal),1),
              aantal_maximum = max(aantal),
              bezoeken = dplyr::n()) %>%
    ungroup()
  
  summary_counts_individuals <- get_counts_smp(species_group = species_group, count_aggregation = "lifestage") %>%
    group_by(primaire_soort, soortgroep, jaar, soort_nl, soort_wet) %>%
    summarise(aantal_totaal = sum(aantal),
              aantal_gemiddeld = round(mean(aantal),1),
              aantal_maximum = max(aantal),
              bezoeken = dplyr::n()) %>%
    ungroup()
  
  summary_counts <- list(details = summary_counts_details,
                         lifestage = summary_counts_lifestage,
                         individuals = summary_counts_individuals
                         )
  
  return(summary_counts)
  
}

get_summary_distribution <- function(species_group = NULL) {
  
  counts <- get_counts_smp(species_group = species_group)
  
  summary_distribution_individuals <- counts %>%
    group_by(primaire_soort, soortgroep, jaar, protocol, soort_nl, soort_wet, locatie) %>%
    summarise(voorkomen = sum(aantal) > 0,
              checklist = any(checklist)) %>%
    ungroup() %>%
    group_by(soortgroep, jaar) %>%
    mutate(locaties_geteld_overige = n_distinct(locatie)) %>%
    ungroup() %>%
    group_by(primaire_soort, soortgroep, jaar, soort_nl, soort_wet, locaties_geteld_overige) %>%
    summarise(locaties_aanwezig = sum(voorkomen),
              locaties_geteld_primair = n_distinct(locatie)) %>%
    ungroup() %>%
    mutate(locaties_geteld = ifelse(primaire_soort, locaties_geteld_primair, locaties_geteld_overige),
           proportie_locaties_aanwezig  = round(locaties_aanwezig/locaties_geteld *100, 1)) %>%
    select(-locaties_geteld_primair, -locaties_geteld_overige)
  
  summary_distribution <- list(individuals = summary_distribution_individuals
  )
  
  return(summary_distribution)
  
}

fit_indexmodel_nbinom_inlabru <- function(analyseset_species) {
  
  model.matrix(~fjaar, analyseset_species) %>% # create dummy variable for year
    as.data.frame() %>%
    select(-1) %>% # drop intercept
    bind_cols(analyseset_species) -> analyseset_species_bru
  
  analyseset_species_bru <- analyseset_species_bru %>%
    mutate(loc_id = as.integer(factor(locatie)))
  
  n_loc <- n_distinct(analyseset_species_bru$locatie)
  
  fjaar_formula <- analyseset_species %>%
    distinct(jaar, fjaar) %>% 
    mutate(fjaar = str_c("fjaar", fjaar)) %>%
    filter(jaar != min(jaar)) 
             
  #inlabru
  comp_inlabru <- as.formula(str_c("aantal ~ doy_scaled + doy_scaled_2", 
                                   str_c(fjaar_formula$fjaar, collapse = " + "), 
                                   "site(map = loc_id, model = \"iid\", n = n_loc)",
                                   sep = " + "))
  
  indexmodel_nbinom_inlabru <- bru(comp_inlabru, data = analyseset_species_bru, family = "nbinomial")
  
  return(indexmodel_nbinom_inlabru)
  
}  

fit_indexmodel_nbinom_inla <- function(analyseset_species) {
  
  model_nbinom_doy_iid <- inla(aantal ~ fjaar + doy_scaled + doy_scaled_2 + f(locatie, model = "iid"),
                               family = "nbinomial",
                               data = analyseset_species,
                               control.compute = list(config = TRUE),
                               control.predictor = list(compute = TRUE)
  )
  
  return(model_nbinom_doy_iid)
  
}

derive_index_inlabru <- function(analyseset_species, indexmodel_nbinom_inlabru, ref_method = "min") {

  model.matrix(~fjaar, analyseset_species) %>% # create dummy variable for year
    as.data.frame() %>%
    select(-1) %>% # drop intercept
    bind_cols(analyseset_species) -> analyseset_species_bru
  
  analyseset_species_bru <- analyseset_species_bru %>%
    mutate(loc_id = as.integer(factor(locatie)),
           ref_jaar = min(jaar))

  fjaar_formula <- analyseset_species %>%
    distinct(jaar, fjaar) %>% 
    mutate(fjaar = str_c("fjaar", fjaar)) %>%
    filter(jaar != min(jaar)) 
  
  year_simdata <- analyseset_species_bru %>%
    filter(jaar != ref_jaar) %>%
    select(soort_nl, soort_wet, jaar, ref_jaar, starts_with("fjaar")) %>%
    unique()
  
  formula_index <- as.formula(str_c("~ exp(", str_c(fjaar_formula$fjaar, collapse = " + "), ")"))
  
  index <- predict(indexmodel_nbinom_inlabru, 
                        data = year_simdata, 
                        formula = formula_index) %>%
    mutate(parameter = "index") %>%
    select(parameter, soort_nl, soort_wet, jaar, ref_jaar, mean, sd, lcl_0.95 = q0.025, ucl_0.95 = q0.975)
  
  return(index)
  
  }
  
derive_diff_years_inla <- function(analyseset_species) {
  
  jaar_min <- min(analyseset_species$jaar)
  jaar_max <- max(analyseset_species$jaar)
  
  contrast_base <-  expand.grid(from = jaar_min:jaar_max, 
                                to = jaar_min:jaar_max, 
                                ref = (jaar_min + 1):jaar_max) %>%
    filter(from != to) %>%
    mutate(
      contrast = ifelse(
        from == ref, 
        -1, 
        ifelse(to == ref, 1, 0)),
      ref = str_c("fjaar", ref)) %>%
    spread(key = "ref", value = "contrast")
    
  contrast_base %>%
    select(starts_with("fjaar")) %>%
    inla.make.lincombs() %>%
    `names<-`(paste(contrast_base$from, contrast_base$to, sep = "-")) -> lc
  
  model_nbinom_inla <- inla(aantal ~ fjaar + doy_scaled + doy_scaled_2 + f(locatie, model = "iid"),
                               family = "nbinomial",
                               data = analyseset_species,
                               control.compute = list(config = TRUE),
                               control.predictor = list(compute = TRUE),
                               lincomb = lc
  )
  
  compare_years <- model_nbinom_inla$summary.lincomb.derived %>%
    rownames_to_column("parameter") %>%
    mutate(parameter = "contrast_years",
           soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           year_from = contrast_base$from,
           year_to = contrast_base$to) %>%
    select(parameter, soort_nl, soort_wet, jaar = year_from, jaar_ref = year_to, mean, sd, lcl_0.95 = `0.025quant`, ucl_0.95 = `0.975quant`)
  
  return(compare_years)
  
}   
  
derive_max_count_inlabru <- function(analyseset_species, indexmodel_nbinom_inlabru){
  
  model.matrix(~fjaar, analyseset_species) %>% # create dummy variable for year
    as.data.frame() %>%
    select(-1) %>% # drop intercept
    bind_cols(analyseset_species) -> analyseset_species_bru
  
  analyseset_species_bru <- analyseset_species_bru %>%
    mutate(loc_id = as.integer(factor(locatie)))
  
  doy_range <- analyseset_species_bru %>%
    select(soort_nl, soort_wet, doy_min, doy_max, doy_mid) %>%
    unique()
  
  doy_simdata <- data.frame(
    soort_nl = doy_range$soort_nl,
    soort_wet = doy_range$soort_wet,
    doy_scaled = ((doy_range$doy_min - doy_range$doy_mid):(doy_range$doy_max - doy_range$doy_mid))/28) %>%
    mutate(doy_scaled_2 = doy_scaled^2,
           doy = doy_scaled * 28 + doy_range$doy_mid)
  
  doy_effect_nbinom <- predict(indexmodel_nbinom_inlabru, 
                               data = doy_simdata, 
                               formula = ~exp(doy_scaled + doy_scaled_2))
  
  doy_max_count <-  (doy_effect_nbinom %>%
                     top_n(1, mean))$doy 
  
  simulate_data_peak_nbinom <- analyseset_species_bru %>%
    select(soort_nl, soort_wet, starts_with("fjaar"), jaar) %>%
    unique() %>%
    mutate(doy_scaled = (doy_max_count - doy_range$doy_mid)/28,
           doy_scaled_2 = doy_scaled^2,
           doy_max_count = doy_max_count)
  
  fjaar_formula <- analyseset_species %>%
    distinct(jaar, fjaar) %>% 
    mutate(fjaar = str_c("fjaar", fjaar)) %>%
    filter(jaar != min(jaar)) 
  
  formula_max_count <- as.formula(str_c(
    "~ exp(Intercept + doy_scaled + doy_scaled_2 + ", 
    str_c(fjaar_formula$fjaar, collapse = " + "), 
    ")")
    )
  
  max_count <- predict(indexmodel_nbinom_inlabru, 
                       data = simulate_data_peak_nbinom, 
                       formula = formula_max_count) %>%
    mutate(parameter = "max_count") %>%
    select(parameter, soort_nl, soort_wet, jaar, mean, sd, lcl_0.95 = q0.025, ucl_0.95 = q0.975)
  
}  

fit_trendmodel_nbinom_inlabru <- function(analyseset_species) {
  
  analyseset_species_2 <- analyseset_species %>%
    mutate(loc_id = as.integer(factor(locatie)),
           year_scaled = jaar -2016)
  
  n_loc <- n_distinct(analyseset_species$locatie)
  
  formula <- aantal ~ doy_scaled + doy_scaled_2 + year_scaled +
    site(map = loc_id, model = "iid", n = n_loc)
  
  trendmodel_nbinom_inlabru <- bru(formula, data = analyseset_species_2, family = "nbinomial")
  
  return(trendmodel_nbinom_inlabru)
  
}


derive_trend_inlabru <- function(analyseset_species, trendmodel_nbinom_inlabru) {
  
  #gemiddelde jaarlijkse trend
  
  trend_average <- trendmodel_nbinom_inlabru$marginals.fixed$year_scaled %>%
    inla.tmarginal(fun = function(x) (exp(x) - 1) * 100) %>%
    inla.zmarginal(silent = TRUE) %>%
    data.frame() %>%
    mutate(parameter = "trend_average",
           soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           jaar_min = min(analyseset_species$jaar),
           jaar_max = max(analyseset_species$jaar)) %>%
    select(parameter, soort_nl, soort_wet, jaar_min, jaar_max, mean, sd, lcl_0.95 = quant0.025, ucl_0.95 = quant0.975)
  
  #totale trend over volledige periode 
  
  periode <- n_distinct(analyseset_species$jaar)

  trend_totaal <- trendmodel_nbinom_inlabru$marginals.fixed$year_scaled %>%
    inla.tmarginal(fun = function(x) (exp(x * (periode - 1)) - 1) * 100) %>%
    inla.zmarginal(silent = TRUE) %>%
    data.frame() %>%
    mutate(parameter = "trend_total",
           soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           jaar_min = min(analyseset_species$jaar),
           jaar_max = max(analyseset_species$jaar)) %>%
    select(parameter, soort_nl, soort_wet, jaar_min, jaar_max, mean, sd, lcl_0.95 = quant0.025, ucl_0.95 = quant0.975)
  
  trend <- bind_rows(trend_average,
                     trend_totaal)
  
  return(trend)
  
}  

get_waic_inlabru <- function(analyseset_species, model_inlabru) {
  
  result_waic <- data.frame(
    parameter = "waic",
    soort_nl = unique(analyseset_species$soort_nl),
    soort_wet = unique(analyseset_species$soort_wet),
    waic = model_inlabru$waic$waic)
  
  return(result_waic)
}

check_random_effect <- function(model_inla){
  
  precision_loc <- model_inla$summary.hyperpar$mean[1]
  
  x <- data.frame(sigma = 1/sqrt(precision_loc),
                  precision = precision_loc,
                  sim_iid = as.numeric(simulate_iid(tau = precision_loc)))
  
  return(x)
}



simulate_data_model_inlabru <- function(analyseset_species, model_inlabru) {
  
  model.matrix(~fjaar, analyseset_species) %>% # create dummy variable for year
    as.data.frame() %>%
    select(-1) %>% # drop intercept
    bind_cols(analyseset_species) -> analyseset_species_bru
  
  analyseset_species_bru <- analyseset_species_bru %>%
    mutate(loc_id = as.integer(factor(locatie)))
  
  doy_range <- analyseset_species_bru %>%
    group_by(doy_min, doy_max, doy_mid) %>%
    summarise(doy_min_obs = min(doy),
              doy_max_obs = max(doy)) %>%
    ungroup() %>%
    mutate(doy_min_show = min(doy_min, doy_min_obs),
           doy_max_show = max(doy_max, doy_max_obs))
  
  jaren_range <- analyseset_species_bru %>%
    select(soort_nl, jaar, starts_with("fjaar20")) %>%
    distinct()
  
  n_loc <- n_distinct(analyseset_species_bru$locatie)
  
  doy_year_loc_simdata <- expand.grid(
    soort_nl = unique(analyseset_species$soort_nl),
    loc_id = 1:n_loc,
    doy_scaled = ((doy_range$doy_min_show - doy_range$doy_mid):(doy_range$doy_max_show - doy_range$doy_mid))/28) %>%
    left_join(jaren_range, by = "soort_nl") %>%
    mutate(doy_scaled_2 = doy_scaled^2,
           doy = doy_scaled * 28 + doy_range$doy_mid)
  
  doy_year_simdata <- doy_year_loc_simdata %>%
    select(-loc_id) %>%
    unique()
  
  fjaar_formula <- analyseset_species_bru %>%
    select(starts_with("fjaar2")) %>%
    colnames() %>%
    str_c(collapse = " + ")
  
  doy_year_effect <- predict(model_inlabru, 
                             data = doy_year_simdata, 
                             formula = as.formula(
                               str_c(
                               "~ exp(Intercept + doy_scaled + doy_scaled_2 + ",
                               fjaar_formula,
                               ")"
                               )
                             )
                             )
  
  doy_year_loc_effect <- predict(model_inlabru, 
                                 data = doy_year_loc_simdata, 
                                 formula = as.formula(
                                   str_c(
                                   "~ exp(Intercept + doy_scaled + doy_scaled_2 + ",
                                   fjaar_formula,
                                   " + site)")
                                   )
                                 )
  
  observed_counts <- analyseset_species_bru %>%
    select(jaar, loc_id, y_obs = aantal, doy_scaled)
  
  doy_year_effect <- doy_year_effect %>%
    select(mean_year = mean,
           sd_year = sd,
           lci_0.95_year = q0.025,
           uci_0.95_year = q0.975,
           jaar, doy_scaled) 
  
  result <- doy_year_loc_effect %>%
    left_join(observed_counts, by = c("jaar", "loc_id", "doy_scaled")) %>%
    left_join(doy_year_effect, by = c("jaar", "doy_scaled"))
  
  return(result)
}

get_results_analysis <- function(path = "analysis_libellen", name_analysis) {
  
  if (dir.exists(here(path))) {
    
    if (dir.exists(here(path, "output", name_analysis))) {
      
      analyseset <- read_vc(file = "analyseset", root = here(path, "output", name_analysis))
      results_indexmodel <- read_vc(file = "results_indexmodel", root = here(path, "output", name_analysis))
      results_trendmodel <- read_vc(file = "results_trendmodel", root = here(path, "output", name_analysis))
      results_waic <- read_vc(file = "results_waic", root = here(path, "output", name_analysis))
      
      results_analysis <- list(
        analyseset = analyseset,
        indexmodel = results_indexmodel,
        trendmodel = results_trendmodel,
        waic = results_waic)
      
      return(results_analysis)
        
      } else {
      
      stop("analysis name does not exist")
      
      }
    
    } else {
      
      stop("path does not exist")
      
  }
    
}
    
     
  



