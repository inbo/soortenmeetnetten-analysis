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
    