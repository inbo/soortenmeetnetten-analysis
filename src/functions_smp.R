library(n2khab)
library(tidyverse)
library(git2rdata)
library(sf)

get_characteristics_smp <- function(species_group = NULL,
                                    file = "metadata/meetnetkarakteristieken",
                                    path = fileman_up("soortenmeetnetten-queries")) {
  
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

get_date_export_smp <- function(file = "raw/date_export",
                                path = fileman_up("soortenmeetnetten-queries")) {
  
  version <- read_vc(file = file, 
                     root = path)
  
  return(version$date_export)
}


get_taxonomy_smp <- function(file = "metadata/extern/checklist.csv",
                                    path = fileman_up("soortenmeetnetten-queries")) {
  
  taxonomy_smp_orig <- read.csv2(file.path(path, file))
  
  taxonomy_smp <- taxonomy_smp_orig %>%
    select(species_order = Nr, genus = GENUS, species = SPECIES, soort_wet = Species.Name, soort_nl = Nederlandse.Naam)
  

  return(taxonomy_smp)
  
}

get_visits_smp <- function(species_group = NULL,
                                    file = "raw/bezoeken",
                                    path = fileman_up("soortenmeetnetten-queries")) {
  
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
                           path = fileman_up("soortenmeetnetten-queries")) {
  
  if(is.null(species_group)) {
    
    counts_smp <- read_vc(file = file, 
                          root = path) 
    
  } else if (str_to_lower(species_group) %in% c("planten", "vaatplanten")) {
    
    counts_smp <- read_vc(file = "raw/aantallen_planten", 
                          root = path)
  } else {
    
    counts_smp <- read_vc(file = file, 
                          root = path)
    
    if (str_to_lower(species_group) %in% counts_smp$soortgroep) {
      
      counts_smp <- counts_smp %>%
        filter(.data$soortgroep == str_to_lower(species_group))
      
    } else {
      
      warning(str_c("Selecteer een van volgende soortgroepen: ", 
                    str_c(unique(counts_smp$soortgroep), collapse = ", ")))
      
    }
    
  }
    
  
  if (!is.null(count_aggregation)) {
    
    if (count_aggregation == "individuals") {
      
      counts_smp <- counts_smp %>%
        filter(type_aantal != "maximum samen gezien") %>%
        mutate(multiply_count = ifelse(activiteit == "copula" | (activiteit == "eiafzettend" & geslacht == "U"), 2, 1)) %>%
        group_by(soortgroep, meetnet, protocol, jaar, datum, doy, locatie, sublocatie, visit_id, niet_geteld, checklist, species_id, soort_nl, soort_wet, primaire_soort, sample_id) %>%
        summarise(aantal = sum(aantal * multiply_count)) %>%
        ungroup()
      
    } else if (count_aggregation == "lifestage") {
      
      counts_smp <- counts_smp %>%
        filter(type_aantal != "maximum samen gezien") %>%
        mutate(levensstadium = ifelse(levensstadium == "imago (niet uitgekleurd)", "imago", levensstadium),
          multiply_count = ifelse(activiteit == "copula" | (activiteit == "eiafzettend" & geslacht == "U"), 2, 1)) %>%
        group_by(soortgroep, meetnet, protocol, jaar, datum, doy, locatie,  sublocatie, visit_id, niet_geteld, checklist, species_id, soort_nl, soort_wet, primaire_soort, sample_id, levensstadium) %>%
        summarise(aantal = sum(aantal * multiply_count)) %>%
        ungroup()
      
    }
    
  }
  
  return(counts_smp)
  
}

get_observers_smp <- function(species_group = NULL,
                           file = "raw/tellers",
                           path = fileman_up("soortenmeetnetten-queries")) {
  
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
                               linked_to = "visit",
                              path = fileman_up("soortenmeetnetten-queries")) {
  
  if (linked_to == "visit") {
    
    file <- "raw/covariabele"
    
  } else if (linked_to == "sample") {
    
    file <- "raw/covariabele_sample"
    
  }
  
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


get_workpackages_smp <- function(file = "raw/werkpakketten",
                               path = fileman_up("soortenmeetnetten-queries")) {
  
  workpackages_smp <- read_vc(file = file, 
                            root = path)
  
  return(workpackages_smp)
  
}

get_tasks_smp <- function(file = "raw/taken",
                                 path = fileman_up("soortenmeetnetten-queries")) {
  
  tasks_smp <- read_vc(file = file, 
                              root = path)
  
  return(tasks_smp)
  
}

get_locations_notes_smp <- function(file = "raw/locatie_opm",
                          path = fileman_up("soortenmeetnetten-queries")) {
  
  locations <- get_locations_smp(only_active = FALSE, only_sample = FALSE) %>%
    st_drop_geometry()
  
  locations_parent_id <- locations %>%
    select(locatie_type, id, parent_id)
  
  locations_hoofdlocaties <- locations %>%
    filter(locatie_type == "locatie") %>%
    select(hoofdlocatie = locatie, id)
  
  locations_notes_smp <- read_vc(file = file, 
                                 root = path) %>%
    left_join(locations_parent_id, by = "id") %>%
    mutate(sublocatie = ifelse(locatie_type == "sublocatie", locatie, NA)) %>%
    left_join(locations_hoofdlocaties, by = c("parent_id" = "id")) %>%
    mutate(locatie = ifelse(locatie_type == "sublocatie", as.character(hoofdlocatie), as.character(locatie))) %>%
    select(-hoofdlocatie) %>%
    select(soortgroep, meetnet, locatie, id, sublocatie, is_sample, is_active, datum_opmerking, opmerking_locatie)
  
  
  return(locations_notes_smp)
  
}

get_monitoring_targets_smp <- function(file = "metadata/monitoring_targets",
                                    path = fileman_up("soortenmeetnetten-queries")) {
  
  monitoring_targets_smp <- read_vc(file = file, 
                                 root = path)
  
  return(monitoring_targets_smp)
  
}


get_locations_smp <- function(species_group = NULL,
                              only_active = TRUE,
                              only_sample = FALSE,
                                    file = "raw/meetnetten_locaties.gpkg",
                                    path = fileman_up("soortenmeetnetten-queries")) {
  
  locations_smp <- read_sf(dsn = file.path(path, file),
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

get_transects_smp <- function(species_group = NULL,
                              only_active = TRUE,
                              only_sample = FALSE,
                              only_lines = TRUE,
                              file = "raw/meetnetten_locaties.gpkg",
                              path = fileman_up("soortenmeetnetten-queries")) {
  
  transects_smp <- st_read(dsn = file.path(path, file),
                           layer = "transecten", quiet = TRUE)
  
  if (!is.null(species_group)) {
    
    if (species_group %in% transects_smp$soortgroep) {
      transects_smp <- transects_smp %>%
        filter(.data$soortgroep == species_group)
    }
    
    else {
      warning(str_c("Selecteer een van volgende soortgroepen: ", 
                    str_c(unique(transects_smp$soortgroep), collapse = ", ")))
    }
  }
  
  if (only_sample) {
    transects_smp <- transects_smp %>%
      filter(.data$is_sample)
  }
  
  if (only_active) {
    transects_smp <- transects_smp %>%
      filter(.data$is_active)
  }
  
  if (only_lines) {
    transects_smp <- transects_smp %>%
      filter(.data$sectie_lijn)
  }
  
  return(transects_smp)
  
}

add_tag_utm <- function(locations_smp,
                     size_km = 5,    
                     method = "largest_overlap",
                     file = "gis/utm",
                     path = fileman_up("soortenmeetnetten-queries")) {
  
  utm <- read_utm(size_km = size_km, file = file, path = path) %>%
    st_transform(crs = st_crs(locations_smp))
  
  if (method == "centroid") {

      suppressMessages(
        suppressWarnings(
          centroid_smp_utm <- st_centroid(locations_smp) %>%
            st_join(utm)  %>%
            st_drop_geometry() %>%
            select(meetnet, locatie, tag_utm, size_km)
          )
        )
      
      locations_smp_utm <- locations_smp %>%
        left_join(centroid_smp_utm, by = c("meetnet", "locatie"))

  } else if (method == "largest_overlap") {
    
    suppressMessages(
      suppressWarnings(
        locations_smp_utm <- locations_smp %>%
        st_join(utm, largest = TRUE)
      )
    )
    
  }
  
  return(locations_smp_utm)
}

read_utm <- function(size_km = 5,
                     file = "gis/utm",
                     path = fileman_up("soortenmeetnetten-queries")) {
  
  utm <- st_read(dsn = file.path(path, file),
                  layer = str_c("utm", size_km), crs = 31370, quiet = TRUE) %>%
    select(tag_utm = TAG) %>%
    mutate(size_km = size_km)
  
  return(utm)
}
 
read_provincies <- function(file = "gis/provincies",
                            path = fileman_up("soortenmeetnetten-queries")) {
  
  provincies <- st_read(dsn = file.path(path, file),
                 layer = "Provincies2015", crs = 31370, quiet = TRUE) %>%
    select(naam = NAAM)
  
  return(provincies)
}


calculate_monitoring_effort <- function(species_group = NULL, aggregation_level = "meetnet"){
  
  visits <- get_visits_smp(species_group) %>%
    filter(meetnet != "Algemene Vlindermonitoring")
  
  observers <- get_observers_smp(species_group) %>%
    filter(meetnet != "Algemene Vlindermonitoring")
  
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
  } else if (aggregation_level %in% c("overall")) {
    visits_summary <- visits %>%
      group_by(soortgroep) %>%
      summarise(n_tellingen = n_distinct(visit_id)) %>%
      ungroup()
    
    locations_summary <- visits %>%
      group_by(soortgroep) %>%
      summarise(n_telgebieden = n_distinct(locatie)) %>%
      ungroup()
    
    observers_summary <- observers %>%
      group_by(soortgroep) %>%
      summarise(n_tellers = n_distinct(naam_teller)) %>%
      ungroup()
  } else if (aggregation_level %in% c("all_species")) {
    
    visits_summary <- visits %>%
      group_by(jaar) %>%
      summarise(n_tellingen = n_distinct(visit_id)) %>%
      ungroup() %>%
      mutate(soortgroep = "alle soorten")
    
    locations_summary <- visits %>%
      group_by(jaar) %>%
      summarise(n_telgebieden = n_distinct(locatie)) %>%
      ungroup() %>%
      mutate(soortgroep = "alle soorten")
    
    observers_summary <- observers %>%
      group_by(jaar) %>%
      summarise(n_tellers = n_distinct(naam_teller)) %>%
      ungroup() %>%
      mutate(soortgroep = "alle soorten")
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

get_summary_counts <- function(species_group = NULL, aggregation = "meetnet", 
                               file = "raw/aantallen",
                               path = fileman_up("soortenmeetnetten-queries")) {
  
  counts_visit <- get_counts_smp(species_group = species_group, file = file, path = path) %>%
    mutate(protocol = str_remove(protocol, "\\(v1\\)")) %>%
    mutate(protocol = str_remove(protocol, "\\(mobiel\\)")) %>%
    mutate(protocol = str_trim(protocol)) %>%
    filter(type_aantal != "maximum samen gezien") %>%
    group_by(soortgroep, meetnet, protocol, locatie, visit_id, jaar, datum, soort_nl, soort_wet, primaire_soort, geslacht, activiteit, levensstadium, checklist) %>%
    summarise(aantal = sum(aantal, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(soort_nl)) %>%
    mutate(primaire_soort = ifelse(is.na(primaire_soort), FALSE, primaire_soort)) 
  
  if (aggregation == "meetnet") {

    summary_none_checklist_visits <- counts_visit %>% #soortsepecifiek
      distinct(meetnet, protocol, jaar, locatie, visit_id, checklist, soort_nl) %>%
      group_by(meetnet, protocol, jaar, soort_nl) %>%
      summarise(visits_none_checklist = sum(!checklist)) %>%
      ungroup()
    
    summary_effort <- counts_visit %>% #meetnetspecifiek
      distinct(meetnet, protocol, jaar, locatie, visit_id, checklist, primaire_soort) %>%
      group_by(meetnet, protocol, jaar, locatie, visit_id, checklist) %>%
      summarise(count_overige = sum(primaire_soort == FALSE) > 0) %>%
      ungroup() %>%
      group_by(meetnet, protocol, jaar) %>%
      summarise(visits_checklist = sum(checklist),
                visits = n_distinct(visit_id),
                visits_overige = sum(count_overige)) %>%
      ungroup()
    
    summary_counts_details <- counts_visit %>%
      group_by(primaire_soort, soortgroep, meetnet, protocol, jaar, soort_nl, soort_wet, geslacht, activiteit, levensstadium) %>%
      summarise(aantal_totaal = sum(aantal),
                #aantal_gemiddeld = round(mean(aantal),1),
                aantal_maximum = max(aantal),
                bezoeken = n_distinct(visit_id)) %>%
      ungroup() %>%
      left_join(summary_effort, by = c("meetnet", "protocol", "jaar")) %>%
      left_join(summary_none_checklist_visits, by = c("meetnet", "protocol", "jaar", "soort_nl" )) %>%
      mutate(visits_calc = ifelse(primaire_soort, visits, visits_checklist + visits_none_checklist),
             aantal_gemiddeld = round(aantal_totaal/visits_calc, 1))
    
    summary_counts_lifestage <- get_counts_smp(species_group = species_group, count_aggregation = "lifestage", file = file, path = path) %>%
      filter(!is.na(soort_nl)) %>%
      mutate(protocol = str_remove(protocol, "\\(v1\\)")) %>%
      mutate(protocol = str_remove(protocol, "\\(mobiel\\)")) %>%
      mutate(protocol = str_trim(protocol)) %>%
      mutate(primaire_soort = ifelse(is.na(primaire_soort), FALSE, primaire_soort)) %>%
      group_by(primaire_soort, soortgroep, meetnet, protocol, jaar, soort_nl, soort_wet, levensstadium) %>%
      summarise(aantal_totaal = sum(aantal),
                #aantal_gemiddeld = round(mean(aantal),1),
                aantal_maximum = max(aantal),
                bezoeken =  n_distinct(visit_id)) %>%
      ungroup() %>%
      left_join(summary_effort, by = c("meetnet", "protocol", "jaar")) %>%
      left_join(summary_none_checklist_visits, by = c("meetnet", "protocol", "jaar", "soort_nl" )) %>%
      mutate(visits_calc = ifelse(primaire_soort, visits, visits_checklist + visits_none_checklist),
             aantal_gemiddeld = round(aantal_totaal/visits_calc, 1))
    
    summary_counts_individuals <- get_counts_smp(species_group = species_group, count_aggregation = "lifestage", file = file, path = path) %>%
      filter(!is.na(soort_nl)) %>%
      mutate(protocol = str_remove(protocol, "\\(v1\\)")) %>%
      mutate(protocol = str_remove(protocol, "\\(mobiel\\)")) %>%
      mutate(protocol = str_trim(protocol)) %>%
      mutate(primaire_soort = ifelse(is.na(primaire_soort), FALSE, primaire_soort)) %>%
      group_by(soortgroep, meetnet, protocol, locatie, visit_id, jaar, datum, soort_nl, soort_wet, primaire_soort, levensstadium) %>%
      summarise(aantal = sum(aantal, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(primaire_soort, soortgroep, meetnet, protocol, jaar, soort_nl, soort_wet) %>%
      summarise(aantal_totaal = sum(aantal),
                #aantal_gemiddeld = round(mean(aantal),1),
                aantal_maximum = max(aantal),
                visits_species = n_distinct(visit_id),
                locaties_species_year = sum(aantal > 0)) %>%
      ungroup()
    
    meetnetten_species <- summary_counts_individuals %>%
      distinct(soortgroep, meetnet, protocol, primaire_soort, soort_nl, soort_wet)
    
    meetnetten_years <- summary_counts_individuals %>%
      distinct(meetnet, jaar)
    
    summary_counts_individuals <-  meetnetten_years %>%
      left_join(meetnetten_species, by = "meetnet") %>%
      left_join(summary_counts_individuals, by = c("soortgroep", "meetnet", "protocol", "primaire_soort", "soort_nl", "soort_wet", "jaar")) %>%
      left_join(summary_effort, by = c("meetnet", "protocol", "jaar")) %>%
      left_join(summary_none_checklist_visits, by = c("meetnet", "protocol", "jaar", "soort_nl" )) %>%
      mutate(aantal_totaal = ifelse(is.na(aantal_totaal), 0, aantal_totaal),
             aantal_maximum = ifelse(is.na(aantal_maximum), 0, aantal_maximum),
             visits_none_checklist = ifelse(is.na(visits_none_checklist), 0, visits_none_checklist),
             visits_species = ifelse(is.na(visits_species), 0, visits_species),
             locaties_species_year = ifelse(is.na(locaties_species_year), 0, locaties_species_year)) %>%
      mutate(visits_calc = ifelse(primaire_soort, visits, visits_checklist + visits_none_checklist),
             aantal_gemiddeld = round(aantal_totaal/visits_calc, 1),
             aantal_gemiddeld_overige = ifelse(primaire_soort, aantal_gemiddeld, round(aantal_totaal/visits_overige, 1)))
    
  }
  
  if (aggregation == "locatie") {
    
    summary_none_checklist_visits <- counts_visit %>% #soortsepecifiek
      distinct(meetnet, protocol, jaar, locatie, visit_id, checklist, soort_nl) %>%
      group_by(meetnet, protocol, jaar, locatie, soort_nl) %>%
      summarise(visits_none_checklist = sum(!checklist)) %>%
      ungroup()
    
    summary_effort <- counts_visit %>% #meetnetspecifiek
      distinct(meetnet, protocol, jaar, locatie, visit_id, checklist) %>%
      group_by(meetnet, protocol, jaar, locatie) %>%
      summarise(visits_checklist = sum(checklist),
                visits = n_distinct(visit_id)) %>%
      ungroup()
    
    summary_counts_details <- counts_visit %>%
      group_by(primaire_soort, soortgroep, meetnet, protocol, jaar, locatie, soort_nl, soort_wet, geslacht, activiteit, levensstadium) %>%
      summarise(aantal_totaal = sum(aantal),
                #aantal_gemiddeld = round(mean(aantal),1),
                aantal_maximum = max(aantal),
                bezoeken = n_distinct(visit_id)) %>%
      ungroup() %>%
      left_join(summary_effort, by = c("meetnet", "protocol", "jaar", "locatie")) %>%
      left_join(summary_none_checklist_visits, by = c("meetnet", "protocol", "jaar", "locatie", "soort_nl" )) %>%
      mutate(visits_calc = ifelse(primaire_soort, visits, visits_checklist + visits_none_checklist),
             aantal_gemiddeld = round(aantal_totaal/visits_calc, 1))
    
    summary_counts_lifestage <- get_counts_smp(species_group = species_group, count_aggregation = "lifestage", file = file, path = path) %>%
      filter(!is.na(soort_nl)) %>%
      mutate(protocol = str_remove(protocol, "\\(v1\\)")) %>%
      mutate(protocol = str_remove(protocol, "\\(mobiel\\)")) %>%
      mutate(protocol = str_trim(protocol)) %>%
      mutate(primaire_soort = ifelse(is.na(primaire_soort), FALSE, primaire_soort)) %>%
      group_by(primaire_soort, soortgroep, meetnet, protocol, jaar, locatie, soort_nl, soort_wet, levensstadium) %>%
      summarise(aantal_totaal = sum(aantal),
                #aantal_gemiddeld = round(mean(aantal),1),
                aantal_maximum = max(aantal),
                bezoeken =  n_distinct(visit_id)) %>%
      ungroup() %>%
      left_join(summary_effort, by = c("meetnet", "protocol", "jaar", "locatie")) %>%
      left_join(summary_none_checklist_visits, by = c("meetnet", "protocol", "jaar", "locatie", "soort_nl" )) %>%
      mutate(visits_calc = ifelse(primaire_soort, visits, visits_checklist + visits_none_checklist),
             aantal_gemiddeld = round(aantal_totaal/visits_calc, 1))
    
    summary_counts_individuals <- get_counts_smp(species_group = species_group, count_aggregation = "lifestage", file = file, path = path) %>%
      filter(!is.na(soort_nl)) %>%
      mutate(protocol = str_remove(protocol, "\\(v1\\)")) %>%
      mutate(protocol = str_remove(protocol, "\\(mobiel\\)")) %>%
      mutate(protocol = str_trim(protocol)) %>%
      mutate(primaire_soort = ifelse(is.na(primaire_soort), FALSE, primaire_soort)) %>%
      group_by(primaire_soort, soortgroep, meetnet, protocol, jaar, locatie, soort_nl, soort_wet) %>%
      summarise(aantal_totaal = sum(aantal),
                #aantal_gemiddeld = round(mean(aantal),1),
                aantal_maximum = max(aantal),
                bezoeken = n_distinct(visit_id)) %>%
      ungroup() %>%
      left_join(summary_effort, by = c("meetnet", "protocol", "jaar", "locatie")) %>%
      left_join(summary_none_checklist_visits, by = c("meetnet", "protocol", "jaar", "locatie", "soort_nl" )) %>%
      mutate(visits_calc = ifelse(primaire_soort, visits, visits_checklist + visits_none_checklist),
             aantal_gemiddeld = round(aantal_totaal/visits_calc, 1))
  }
  
  summary_counts <- list(details = summary_counts_details,
                         lifestage = summary_counts_lifestage,
                         individuals = summary_counts_individuals
  )
  
  
  return(summary_counts)
  
}

get_summary_distribution <- function(species_group = NULL, aggregation_periode = "year", 
                                     file = "raw/aantallen",
                                     path = fileman_up("soortenmeetnetten-queries")) {
  
  counts <- get_counts_smp(species_group = species_group, file = file, path = path) %>%    
    mutate(protocol = str_remove(protocol, "\\(v1\\)")) %>%
    mutate(protocol = str_remove(protocol, "\\(mobiel\\)")) %>%
    mutate(protocol = str_trim(protocol)) %>%
    filter(!is.na(soort_nl)) %>%
    filter(type_aantal != "maximum samen gezien") %>%
    mutate(primaire_soort = ifelse(is.na(primaire_soort), FALSE, primaire_soort))
  
  if (aggregation_periode == "year") {
    
    summary_effort <- counts %>%
      distinct(meetnet, protocol, jaar, locatie, visit_id, checklist) %>%
      group_by(meetnet, protocol, jaar, locatie) %>%
      summarise(checklist_locatie = any(checklist),
                visits_checklist = sum(checklist),
                visits = n_distinct(visit_id)) %>%
      ungroup() %>%
      group_by(meetnet, protocol, jaar) %>%
      summarise(geteld_locatie_primair = n_distinct(locatie),
                geteld_locatie_checklist = sum(checklist_locatie),
                visits_checklist = sum(visits_checklist),
                visits = sum(visits)) %>%
      ungroup()
    
    summary_distribution_lifestage <- counts %>%
      mutate(levensstadium = ifelse(levensstadium == "imago (niet uitgekleurd)", "imago", levensstadium)) %>%
      group_by(meetnet, protocol, jaar, locatie) %>%
      mutate(checklist_locatie = any(checklist) ) %>%
      ungroup() %>%
      group_by(primaire_soort, soortgroep, jaar, meetnet, protocol, soort_nl, soort_wet, levensstadium, locatie) %>%
      summarise(voorkomen = sum(aantal) > 0,
                none_checklist_locatie = !unique(checklist_locatie)) %>%
      ungroup() %>%
      group_by(primaire_soort, soortgroep, meetnet, protocol, jaar, soort_nl, soort_wet, levensstadium) %>%
      summarise(locaties_aanwezig = sum(voorkomen),
                geteld_locatie_none_checklist = sum(none_checklist_locatie)) %>%
      ungroup() %>%
      left_join(summary_effort, by = c("meetnet", "protocol", "jaar")) %>%
      mutate(locatie_geteld = ifelse(primaire_soort, 
                                     geteld_locatie_primair,
                                     geteld_locatie_checklist + geteld_locatie_none_checklist), 
             proportie_locaties_aanwezig  = round(locaties_aanwezig/locatie_geteld *100, 1)) %>%
      select(primaire_soort, soortgroep, meetnet, protocol, jaar, soort_nl, soort_wet, levensstadium, locatie_geteld, locaties_aanwezig, proportie_locaties_aanwezig)
    
    summary_distribution_individuals <- counts %>%
      group_by(meetnet, protocol, jaar, locatie) %>%
      mutate(checklist_locatie = any(checklist) ) %>%
      ungroup() %>%
      group_by(primaire_soort, soortgroep, jaar, meetnet, protocol, soort_nl, soort_wet, locatie) %>%
      summarise(voorkomen = sum(aantal) > 0,
                none_checklist_locatie = !unique(checklist_locatie)) %>%
      ungroup() %>%
      group_by(primaire_soort, soortgroep, meetnet, protocol, jaar, soort_nl, soort_wet) %>%
      summarise(locaties_aanwezig = sum(voorkomen),
                geteld_locatie_none_checklist = sum(none_checklist_locatie)) %>%
      ungroup() %>%
      left_join(summary_effort, by = c("meetnet", "protocol", "jaar")) %>%
      mutate(locatie_geteld = ifelse(primaire_soort, 
                                     geteld_locatie_primair,
                                     geteld_locatie_checklist + geteld_locatie_none_checklist), 
             proportie_locaties_aanwezig  = round(locaties_aanwezig/locatie_geteld *100, 1)) %>%
      select(primaire_soort, soortgroep, meetnet, protocol, jaar, soort_nl, soort_wet, locatie_geteld, locaties_aanwezig, proportie_locaties_aanwezig)
    
    summary_distribution <- list(lifestage = summary_distribution_lifestage,
                                 individuals = summary_distribution_individuals)
    } else if (aggregation_periode == "all_years") {
      
      summary_effort <- counts %>%
        distinct(meetnet, protocol, locatie, visit_id, checklist) %>%
        group_by(meetnet, protocol, locatie) %>%
        summarise(checklist_locatie = any(checklist),
                  visits_checklist = sum(checklist),
                  visits = n_distinct(visit_id)) %>%
        ungroup() %>%
        group_by(meetnet, protocol) %>%
        summarise(geteld_locatie_primair = n_distinct(locatie),
                  geteld_locatie_checklist = sum(checklist_locatie),
                  visits_checklist = sum(visits_checklist),
                  visits = sum(visits)) %>%
        ungroup()
      
      summary_distribution_lifestage <- counts %>%
        mutate(levensstadium = ifelse(levensstadium == "imago (niet uitgekleurd)", "imago", levensstadium)) %>%
        group_by(meetnet, protocol, locatie) %>%
        mutate(checklist_locatie = any(checklist) ) %>%
        ungroup() %>%
        group_by(primaire_soort, soortgroep, meetnet, protocol, soort_nl, soort_wet, levensstadium, locatie) %>%
        summarise(voorkomen = sum(aantal) > 0,
                  none_checklist_locatie = !unique(checklist_locatie)) %>%
        ungroup() %>%
        group_by(primaire_soort, soortgroep, meetnet, protocol, soort_nl, soort_wet, levensstadium) %>%
        summarise(locaties_aanwezig = sum(voorkomen),
                  geteld_locatie_none_checklist = sum(none_checklist_locatie)) %>%
        ungroup() %>%
        left_join(summary_effort, by = c("meetnet", "protocol")) %>%
        mutate(locatie_geteld = ifelse(primaire_soort, 
                                       geteld_locatie_primair,
                                       geteld_locatie_checklist + geteld_locatie_none_checklist), 
               proportie_locaties_aanwezig  = round(locaties_aanwezig/locatie_geteld *100, 1)) %>%
        select(primaire_soort, soortgroep, meetnet, protocol, soort_nl, soort_wet, levensstadium, locatie_geteld, locaties_aanwezig, proportie_locaties_aanwezig)
      
      summary_distribution_individuals <- counts %>%
        group_by(meetnet, protocol, locatie) %>%
        mutate(checklist_locatie = any(checklist) ) %>%
        ungroup() %>%
        group_by(primaire_soort, soortgroep, meetnet, protocol, soort_nl, soort_wet, locatie) %>%
        summarise(voorkomen = sum(aantal) > 0,
                  none_checklist_locatie = !unique(checklist_locatie)) %>%
        ungroup() %>%
        group_by(primaire_soort, soortgroep, meetnet, protocol, soort_nl, soort_wet) %>%
        summarise(locaties_aanwezig = sum(voorkomen),
                  geteld_locatie_none_checklist = sum(none_checklist_locatie)) %>%
        ungroup() %>%
        left_join(summary_effort, by = c("meetnet", "protocol")) %>%
        mutate(locatie_geteld = ifelse(primaire_soort, 
                                       geteld_locatie_primair,
                                       geteld_locatie_checklist + geteld_locatie_none_checklist), 
               proportie_locaties_aanwezig  = round(locaties_aanwezig/locatie_geteld *100, 1)) %>%
        select(primaire_soort, soortgroep, meetnet, protocol, soort_nl, soort_wet, locatie_geteld, locaties_aanwezig, proportie_locaties_aanwezig)
      
      summary_distribution <- list(lifestage = summary_distribution_lifestage,
                                   individuals = summary_distribution_individuals)
    }
  
  return(summary_distribution)
  
}

fit_indexmodel_nbinom_inlabru <- function(analyseset_species, offset_var = NULL, season_effect = TRUE) {
  
  analyseset_species <- analyseset_species %>%
    mutate(fjaar = factor(jaar),
           locatie = as.character(locatie),
           locatie = as.factor(locatie))
  
  model.matrix(~fjaar, analyseset_species) %>% # create dummy variable for year
    as.data.frame() %>%
    select(-1) %>% # drop intercept
    bind_cols(analyseset_species) -> analyseset_species_bru
  
  analyseset_species_bru <- analyseset_species_bru %>%
    mutate(loc_id = as.integer(factor(locatie)))
  
  n_loc <- n_distinct(analyseset_species_bru$locatie)
  
  fjaar_formula <- analyseset_species_bru %>%
    select(starts_with("fjaar"), -fjaar) %>% 
    unique() %>%
    colnames() %>%
    str_c(collapse = " + ")
  
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  
  comp_inlabru <- as.formula(str_c("aantal ~ doy_scaled + doy_scaled_2", 
                                   fjaar_formula, 
                                   "site(map = loc_id, model = \"iid\", n = n_loc, hyper = prec.prior)",
                                   sep = " + "))
  
  if(is.null(offset_var)) {
    
    indexmodel_nbinom_inlabru1 <- bru(comp_inlabru, 
                                     data = analyseset_species_bru, 
                                     family = "nbinomial")
    
  } else {
    
    analyseset_species_bru <- analyseset_species_bru %>%
      rename(offset = offset_var)
    
    indexmodel_nbinom_inlabru <- bru(comp_inlabru, 
                                     data = analyseset_species_bru, 
                                     family = "nbinomial",
                                     offset = offset)
    
  }
  
  return(indexmodel_nbinom_inlabru)
  
}  

fit_indexmodel_nbinom_inla <- function(analyseset_species, offset_var = NULL, season_effect = TRUE, generation_effect = FALSE) {
  
  analyseset_species <- analyseset_species %>%
    mutate(fjaar = factor(jaar),
           locatie = as.character(locatie),
           locatie = as.factor(locatie))
  
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  
  if (season_effect) {
    
    if (generation_effect) {
      
      formula_indexmodel <- as.formula("aantal ~ fjaar + generatie*doy_scaled + generatie*doy_scaled_2 + f(locatie, model = \"iid\", hyper = prec.prior)")
      
    } else {
      
      formula_indexmodel <- as.formula("aantal ~ fjaar + doy_scaled + doy_scaled_2 + f(locatie, model = \"iid\", hyper = prec.prior)")
      
    }
    
  } else {
    
    if (generation_effect) {
      
      formula_indexmodel <- as.formula("aantal ~ fjaar + generatie + f(locatie, model = \"iid\", hyper = prec.prior)")
      
    } else {
      
      formula_indexmodel <- as.formula("aantal ~ fjaar + f(locatie, model = \"iid\", hyper = prec.prior)")
      
    }
    
  } 
  
  if(is.null(offset_var)) {
    
  indexmodel_nbinom_doy_iid <- inla(formula_indexmodel,
                                      family = "nbinomial",
                                      data = analyseset_species,
                                      control.compute = list(config = TRUE, waic = TRUE),
                                      control.predictor = list(compute = TRUE))
  } else {
    
    analyseset_species2 <- analyseset_species %>%
      rename(offset = offset_var)
    
    indexmodel_nbinom_doy_iid <- inla(formula_indexmodel,
                                      family = "nbinomial",
                                      data = analyseset_species2,
                                      offset = offset,
                                      control.compute = list(config = TRUE, waic = TRUE),
                                      control.predictor = list(compute = TRUE))
    
  }
  
  
  return(indexmodel_nbinom_doy_iid)
  
}

derive_index_inlabru <- function(analyseset_species, indexmodel_nbinom_inlabru, ref_method = "min") {

  model.matrix(~fjaar, analyseset_species) %>% # create dummy variable for year
    as.data.frame() %>%
    select(-1) %>% # drop intercept
    bind_cols(analyseset_species) -> analyseset_species_bru
  
  analyseset_species_bru <- analyseset_species_bru %>%
    mutate(loc_id = as.integer(factor(locatie)),
           ref_jaar = min(jaar))

  fjaar_formula <- analyseset_species_bru %>%
    select(starts_with("fjaar"), -fjaar) %>% 
    unique() %>%
    colnames() %>%
    str_c(collapse = " + ") 
  
  year_simdata <- analyseset_species_bru %>%
   # filter(jaar != ref_jaar) %>%
    select(soort_nl, soort_wet, jaar, ref_jaar, starts_with("fjaar")) %>%
    unique()
  
  formula_index <- as.formula(str_c("~ exp(", fjaar_formula, ")"))
  
  index <- predict(indexmodel_nbinom_inlabru, 
                        data = year_simdata, 
                        formula = formula_index) %>%
    mutate(parameter = "index") %>%
    select(parameter, soort_nl, soort_wet, jaar, ref_jaar, mean, sd, lcl_0.95 = q0.025, ucl_0.95 = q0.975)
  
  formula_index_link <-  as.formula(str_c("~ (", fjaar_formula, ")"))
  
  sd_link_index <- predict(indexmodel_nbinom_inlabru, 
                               data = year_simdata, 
                               formula = formula_index_link) %>%
    mutate(lcl_0.90_link = qnorm(p = c(0.05), mean, sd),
           ucl_0.90_link = qnorm(p = c(0.95), mean , sd )) %>%
    select(soort_nl, jaar, mean_link = mean, sd_link = sd, lcl_0.90_link, ucl_0.90_link)
  
  index_result <- index %>%
    left_join(sd_link_index, by = c("soort_nl", "jaar")) %>%
    mutate(lcl_0.90 = exp(lcl_0.90_link),
           ucl_0.90 = exp(ucl_0.90_link)) %>%
    select(-lcl_0.90_link, -ucl_0.90_link)
    
  
  return(index_result)
  
}

derive_index_inla <- function(analyseset_species, indexmodel_nbinom_inla, ref_method = "min", fun = NULL) {
  
  if (is.null(fun)) {
  
  if (min(analyseset_species$jaar) == 2016) {
    
    fun = function(...) {
      c(exp(fjaar2017),  exp(fjaar2018), exp(fjaar2019), exp(fjaar2020), exp(fjaar2021), exp(fjaar2022), exp(fjaar2023))
    }
    
  } else if (min(analyseset_species$jaar) == 2017) {
    
    fun = function(...) {
      c(exp(fjaar2018), exp(fjaar2019), exp(fjaar2020), exp(fjaar2021), exp(fjaar2022), exp(fjaar2023))
    }
      
    } else if (min(analyseset_species$jaar) == 2018) {
      
      fun = function(...) {
        c(exp(fjaar2019), exp(fjaar2020), exp(fjaar2021), exp(fjaar2022), exp(fjaar2023))
      } 
        } else if (min(analyseset_species$jaar) == 2019) {
      
          fun = function(...) {
            c(exp(fjaar2020), exp(fjaar2021), exp(fjaar2022), exp(fjaar2023))
          }
    
        }
    
  }
  
  model_inla.samples <- inla.posterior.sample(1000, indexmodel_nbinom_inla)
  
  quantile_values <- c(0.025, 0.05, 0.20, 0.35,  0.65, 0.80, 0.95, 0.975)
  
  predict_year_inla <- inla.posterior.sample.eval(fun, model_inla.samples) %>%
    as.data.frame() %>%
    mutate(jaar = (min(analyseset_species$jaar) + 1):max(analyseset_species$jaar)) %>%
    gather(starts_with("sample"), key = "sample", value = "waarde") %>%
    group_by(jaar) %>%
    mutate(mean = mean(waarde),
           sd = sd(waarde)) %>%
    ungroup() %>%
    group_by(jaar, mean, sd) %>%
    summarise(qs = quantile(waarde, quantile_values), prob = quantile_values) %>%
    ungroup() %>%
    mutate(l_u = ifelse(prob < 0.5, "lcl", "ucl"),
           ci = ifelse(prob %in% c(0.025, 0.975), "0.95",
                       ifelse(prob %in% c(0.05, 0.95), "0.90",
                              ifelse(prob %in% c(0.20, 0.80), "0.60",
                                     ifelse(prob %in% c(0.35, 0.65), "0.30", NA)))),
           type = str_c(l_u, "_", ci)) %>%
    select(-prob, -l_u, -ci) %>%
    spread(key = type, value = qs) %>%
    mutate(soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           ref_jaar = min(jaar) - 1,
           parameter = "index") %>%
    select(parameter, soort_nl, soort_wet, jaar, ref_jaar, everything())
  
  return(predict_year_inla)
    
}

derive_index_meetcyclus_inla <- function(analyseset_species = NULL, indexmodel_nbinom_inla, function_eval = NULL, set_seed = 0) {
  
  
  var_fixed <- indexmodel_nbinom_inla$names.fixed[-1]
  
  analyseset_species <- indexmodel_nbinom_inla$.args$data
  
  meetcyclus_ref <- analyseset_species %>%
    distinct(meetcyclus, jaar) %>%
    slice_min(jaar)
  
  if (is.null(function_eval)) {
    
    function_eval <- function(...) {exp(meetcyclus2019_2021)}
    
  }
  
  model_inla.samples <- inla.posterior.sample(1000, indexmodel_nbinom_inla, seed = set_seed, num.threads = "1:1")
  
  quantile_values <- c(0.025, 0.05, 0.20, 0.35,  0.65, 0.80, 0.95, 0.975)
  
  predict_year_inla <- inla.posterior.sample.eval(function_eval, model_inla.samples) %>%
    as.data.frame() %>%
    mutate(meetcyclus = var_fixed) %>%
    gather(starts_with("sample"), key = "sample", value = "waarde") %>%
    group_by(meetcyclus) %>%
    mutate(mean = mean(waarde),
           sd = sd(waarde)) %>%
    ungroup() %>%
    group_by(meetcyclus, mean, sd) %>%
    summarise(qs = quantile(waarde, quantile_values), prob = quantile_values) %>%
    ungroup() %>%
    mutate(l_u = ifelse(prob < 0.5, "lcl", "ucl"),
           ci = ifelse(prob %in% c(0.025, 0.975), "0.95",
                       ifelse(prob %in% c(0.05, 0.95), "0.90",
                              ifelse(prob %in% c(0.20, 0.80), "0.60",
                                     ifelse(prob %in% c(0.35, 0.65), "0.30", NA)))),
           type = str_c(l_u, "_", ci)) %>%
    select(-prob, -l_u, -ci) %>%
    spread(key = type, value = qs) %>%
    mutate(soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           parameter = "index",
           meetcyclus_ref = meetcyclus_ref$meetcyclus) %>%
    select(parameter, soort_nl, soort_wet, meetcyclus, everything())
  
  return(predict_year_inla)
  
}
  

derive_index_rw_sample <- function(sample) {
  
  s <- sample$latent %>%
    as.data.frame() %>%
    rownames_to_column(var = "parameter") %>%
    filter(str_sub(parameter, 1, 4) == "jaar") %>%
    separate(col = parameter, into = c("var", "jaar_centered"), sep = ":") %>%
    mutate(jaar_centered = as.numeric(jaar_centered)) %>%
    select(-var)
  
  colnames(s)[2] <- "sample_value"
  
  ref_value <- s[s$jaar_centered == 1, "sample_value"]
  
  s <- s %>%
    mutate(index = ifelse(jaar_centered == 1, NA, exp(sample_value - ref_value)),
           diff_previous_year = exp(sample_value - lag(sample_value))) %>%
    pivot_longer(cols = -jaar_centered,
                 names_to = "parameter", values_to = "value" )
  
  return(s)
}



derive_index_rw_inla <- function(analyseset_species, inlamodel_rw, set_seed = 0) {
  
  set.seed(set_seed)
  
  samples <- inla.posterior.sample(1000, inlamodel_rw , seed = set_seed,num.threads = "1:1")
  
  quantile_values <- c(0.025, 0.05, 0.20, 0.35,  0.65, 0.80, 0.95, 0.975)
  
  estimates_jaar <- map_df(samples, derive_index_rw_sample, .id = "sample") %>%
    group_by(parameter, jaar_centered) %>%
    mutate(mean = mean(value),
           sd = sd(value)) %>%
    ungroup() %>%
    group_by(parameter, jaar_centered, mean, sd) %>%
    summarise(qs = quantile(value, quantile_values, na.rm =TRUE), prob = quantile_values) %>%
    ungroup() %>%
    mutate(l_u = ifelse(prob < 0.5, "lcl", "ucl"),
           ci = ifelse(prob %in% c(0.025, 0.975), "0.95",
                       ifelse(prob %in% c(0.05, 0.95), "0.90",
                              ifelse(prob %in% c(0.20, 0.80), "0.60",
                                     ifelse(prob %in% c(0.35, 0.65), "0.30", NA)))),
           type = str_c(l_u, "_", ci)) %>%
    select(-prob, -l_u, -ci) %>%
    spread(key = type, value = qs)  %>%
    mutate(soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           ref_jaar = min(analyseset_species$jaar),
           jaar = ref_jaar + jaar_centered -1) %>%
    select(parameter, soort_nl, soort_wet, jaar, ref_jaar, everything())
  
  
}

derive_prob_rw_inla <- function(analyseset_species, inlamodel_rw, set_seed = 0) {
  
  set.seed(set_seed)
  
  samples <- inla.posterior.sample(1000, inlamodel_rw , seed = set_seed,num.threads="1:1")
  
  quantile_values <- c(0.025, 0.05, 0.20, 0.35,  0.65, 0.80, 0.95, 0.975)
  
  estimates_jaar <- map_df(samples, derive_prob_rw_sample, .id = "sample") %>%
    group_by(parameter, jaar_centered) %>%
    mutate(mean = mean(value),
           sd = sd(value)) %>%
    ungroup() %>%
    group_by(parameter, jaar_centered, mean, sd) %>%
    summarise(qs = quantile(value, quantile_values, na.rm =TRUE), prob = quantile_values) %>%
    ungroup() %>%
    mutate(l_u = ifelse(prob < 0.5, "lcl", "ucl"),
           ci = ifelse(prob %in% c(0.025, 0.975), "0.95",
                       ifelse(prob %in% c(0.05, 0.95), "0.90",
                              ifelse(prob %in% c(0.20, 0.80), "0.60",
                                     ifelse(prob %in% c(0.35, 0.65), "0.30", NA)))),
           type = str_c(l_u, "_", ci)) %>%
    select(-prob, -l_u, -ci) %>%
    spread(key = type, value = qs)  %>%
    mutate(soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           ref_jaar = min(analyseset_species$jaar),
           jaar = ref_jaar + jaar_centered -1) %>%
    select(parameter, soort_nl, soort_wet, jaar, ref_jaar, everything())
  
  
}


derive_prob_rw_sample <- function(sample) {
  
  s <- sample$latent %>%
    as.data.frame() %>%
    rownames_to_column(var = "parameter") %>%
    filter(str_sub(parameter, 1, 4) == "jaar") %>%
    separate(col = parameter, into = c("var", "jaar_centered"), sep = ":") %>%
    mutate(jaar_centered = as.numeric(jaar_centered)) %>%
    select(-var)
  
  colnames(s)[2] <- "sample_value"
  
  ref_value <- s[s$jaar_centered == 1, "sample_value"]
  
  s <- s %>%
    mutate(prob = plogis(sample_value)) %>%
    pivot_longer(cols = -jaar_centered,
                 names_to = "parameter", values_to = "value" )
  
  return(s)
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
  
  # doy_range <- analyseset_species_bru %>%
  #   select(soort_nl, soort_wet, doy_min, doy_max, doy_mid) %>%
  #   unique()
  # 
  # doy_simdata <- data.frame(
  #   soort_nl = doy_range$soort_nl,
  #   soort_wet = doy_range$soort_wet,
  #   doy_scaled = ((doy_range$doy_min - doy_range$doy_mid):(doy_range$doy_max - doy_range$doy_mid))/28) %>%
  #   mutate(doy_scaled_2 = doy_scaled^2,
  #          doy = doy_scaled * 28 + doy_range$doy_mid)
  # 
  # doy_effect_nbinom <- predict(indexmodel_nbinom_inlabru, 
  #                              data = doy_simdata, 
  #                              formula = ~exp(doy_scaled + doy_scaled_2))
  # 
  # doy_max_count <-  (doy_effect_nbinom %>%
  #                    top_n(1, mean))$doy 
  
  ## alternatief via afgeleide: d(a*doy_scaled + b*doy_scaled^2) = 0 -> doy_scaled_max = -a/2b
  
  doy_scaled_max_count <- -indexmodel_nbinom_inlabru$summary.fixed["doy_scaled", "mean"]/(2*indexmodel_nbinom_inlabru$summary.fixed["doy_scaled_2", "mean"])
  doy_max_count <- round(doy_scaled_max_count * 28 + unique(analyseset_species$doy_mid))  
  
  
  simulate_data_peak_nbinom <- analyseset_species_bru %>%
    select(soort_nl, soort_wet, starts_with("fjaar"), jaar) %>%
    unique() %>%
    mutate(doy_scaled = (doy_max_count - doy_range$doy_mid)/28,
           doy_scaled_2 = doy_scaled^2,
           doy_max_count = doy_max_count)
  
  fjaar_formula <- analyseset_species_bru %>%
    select(starts_with("fjaar"), -fjaar) %>% 
    unique() %>%
    colnames() %>%
    str_c(collapse = " + ")  
  
  formula_max_count <- as.formula(str_c(
    "~ exp(Intercept + doy_scaled + doy_scaled_2 + ", 
    fjaar_formula, 
    ")")
    )
  
  max_count <- predict(indexmodel_nbinom_inlabru, 
                       data = simulate_data_peak_nbinom, 
                       formula = formula_max_count) %>%
    mutate(parameter = "max_count") %>%
    select(parameter, soort_nl, soort_wet, jaar, mean, sd, lcl_0.95 = q0.025, ucl_0.95 = q0.975)
  
  formula_max_count_link <- as.formula(str_c(
    "~ (Intercept + doy_scaled + doy_scaled_2 + ", 
    fjaar_formula, ")")
  )
  
  sd_link_max_count <- predict(indexmodel_nbinom_inlabru, 
                     data = simulate_data_peak_nbinom, 
                     formula = formula_max_count_link) %>%
    select(soort_nl, jaar, mean_link = mean, sd_link = sd)
  
  max_count <- max_count %>%
    left_join(sd_link_max_count, by = c("soort_nl", "jaar")) %>%
    mutate(lcl_0.90 = exp(qnorm(p = c(0.05), mean = mean_link, sd = sd_link)),
           ucl_0.90 = exp(qnorm(p = c(0.95), mean = mean_link, sd = sd_link))
             )
  
  return(max_count)
  
}  




derive_max_count_inla <- function(analyseset_species, indexmodel_nbinom_inla){
  
  
  doy_scaled_max <- -indexmodel_nbinom_inla$summary.fixed["doy_scaled", "mean"]/(2*indexmodel_nbinom_inla$summary.fixed["doy_scaled_2", "mean"]) #maximimum -> afgeleide(ax + bx2) = 0 -> x = -a/2b
  
  if (min(analyseset_species$jaar) == 2016) {
    
    fun = function(doy) {
      c(exp(Intercept + doy_scaled * doy + doy_scaled_2 * doy^2), 
        exp(Intercept + fjaar2017 + doy_scaled * doy + doy_scaled_2 * doy^2),  
        exp(Intercept + fjaar2018 + doy_scaled * doy + doy_scaled_2 * doy^2), 
        exp(Intercept + fjaar2019 + doy_scaled * doy + doy_scaled_2 * doy^2), 
        exp(Intercept + fjaar2020 + doy_scaled * doy + doy_scaled_2 * doy^2),
        exp(Intercept + fjaar2021 + doy_scaled * doy + doy_scaled_2 * doy^2))
    
    }
    
  } else if (min(analyseset_species$jaar) == 2017) {
    
    fun = function(doy = -indexmodel_nbinom_inla$summary.fixed["doy_scaled", "mean"]/(2*indexmodel_nbinom_inla$summary.fixed["doy_scaled_2", "mean"])) {
      c(exp(Intercept + doy_scaled * doy + doy_scaled_2 * doy^2),
        exp(Intercept + fjaar2018 + doy_scaled * doy + doy_scaled_2 * doy^2), 
        exp(Intercept + fjaar2019 + doy_scaled * doy + doy_scaled_2 * doy^2), 
        exp(Intercept + fjaar2020 + doy_scaled * doy + doy_scaled_2 * doy^2),
        exp(Intercept + fjaar2021 + doy_scaled * doy + doy_scaled_2 * doy^2))
    }
    
  } else if (min(analyseset_species$jaar) == 2018) {
    
    fun = function(doy = -indexmodel_nbinom_inla$summary.fixed["doy_scaled", "mean"]/(2*indexmodel_nbinom_inla$summary.fixed["doy_scaled_2", "mean"])) {
      c(exp(Intercept + doy_scaled * doy + doy_scaled_2 * doy^2),
        exp(Intercept + fjaar2019 + doy_scaled * doy + doy_scaled_2 * doy^2), 
        exp(Intercept + fjaar2020 + doy_scaled * doy + doy_scaled_2 * doy^2),
        exp(Intercept + fjaar2021 + doy_scaled * doy + doy_scaled_2 * doy^2))
    }
    
  } else if (min(analyseset_species$jaar) == 2019) {
    
    fun = function(doy = -indexmodel_nbinom_inla$summary.fixed["doy_scaled", "mean"]/(2*indexmodel_nbinom_inla$summary.fixed["doy_scaled_2", "mean"])) {
      c(exp(Intercept + doy_scaled * doy + doy_scaled_2 * doy^2),exp(Intercept + fjaar2020 + doy_scaled * doy + doy_scaled_2 * doy^2),
        exp(Intercept + fjaar2021 + doy_scaled * doy + doy_scaled_2 * doy^2))
    }
    
  }
  
  model_inla.samples = inla.posterior.sample(1000, indexmodel_nbinom_inla)
  
  quantile_values <- c(0.025, 0.05, 0.20, 0.35,  0.65, 0.80, 0.95, 0.975)
  
  predict_max_count_inla <- inla.posterior.sample.eval(fun, model_inla.samples, doy = doy_scaled_max) %>%
    as.data.frame() %>%
    mutate(jaar = (min(analyseset_species$jaar)):max(analyseset_species$jaar)) %>%
    gather(starts_with("sample"), key = "sample", value = "waarde") %>%
    group_by(jaar) %>%
    mutate(mean = mean(waarde),
           sd = sd(waarde)) %>%
    ungroup() %>%
    group_by(jaar, mean, sd) %>%
    summarise(qs = quantile(waarde, quantile_values), prob = quantile_values) %>%
    ungroup() %>%
    mutate(l_u = ifelse(prob < 0.5, "lcl", "ucl"),
           ci = ifelse(prob %in% c(0.025, 0.975), "0.95",
                       ifelse(prob %in% c(0.05, 0.95), "0.90",
                              ifelse(prob %in% c(0.20, 0.80), "0.60",
                                     ifelse(prob %in% c(0.35, 0.65), "0.30", NA)))),
           type = str_c(l_u, "_", ci)) %>%
    select(-prob, -l_u, -ci) %>%
    spread(key = type, value = qs) %>%
    mutate(soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           parameter = "max_count") %>%
    select(parameter, soort_nl, soort_wet, jaar, everything())
  
  return(predict_max_count_inla)
}

fit_trendmodel_nbinom_inlabru <- function(analyseset_species) {
  
  analyseset_species_2 <- analyseset_species %>%
    mutate(loc_id = as.integer(factor(locatie)),
           min_year = min(jaar),
           year_scaled = jaar - min_year)
  
  n_loc <- n_distinct(analyseset_species$locatie)
  
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  
  formula <- aantal ~ doy_scaled + doy_scaled_2 + year_scaled +
    site(map = loc_id, model = "iid", n = n_loc, hyper = prec.prior)
  
  trendmodel_nbinom_inlabru <- bru(formula, 
                                   data = analyseset_species_2, 
                                   family = "nbinomial")
  
  return(trendmodel_nbinom_inlabru)
  
}

fit_trendmodel_nbinom_inla <- function(analyseset_species, offset_var = NULL, generation_effect = FALSE) {
  
  analyseset_species <- analyseset_species %>%
    mutate(fjaar = factor(jaar),
           locatie = as.character(locatie),
           locatie = as.factor(locatie),
           min_year = min(jaar),
           year_scaled = jaar - min_year)
  
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  
  if (generation_effect) {
    
    trendmodel_formula <- as.formula("aantal ~ year_scaled + doy_scaled*generatie + doy_scaled_2*generatie + f(locatie, model = \"iid\", hyper = prec.prior)")
    
  } else  {
    
    trendmodel_formula <- as.formula("aantal ~ year_scaled + doy_scaled + doy_scaled_2 + f(locatie, model = \"iid\", hyper = prec.prior)")
    
  }
  
  
  if(is.null(offset_var)) {
    
    trendmodel_nbinom_doy_iid <- inla(trendmodel_formula,
                                      family = "nbinomial",
                                      data = analyseset_species,
                                      control.compute = list(config = TRUE, waic = TRUE),
                                      control.predictor = list(compute = TRUE))
  } else {
    
    analyseset_species2 <- analyseset_species %>%
      rename(offset = offset_var)
    
    trendmodel_nbinom_doy_iid <- inla(trendmodel_formula,
                                      family = "nbinomial",
                                      data = analyseset_species2,
                                      offset = offset,
                                      control.compute = list(config = TRUE, waic = TRUE),
                                      control.predictor = list(compute = TRUE))
  }
  
  return(trendmodel_nbinom_doy_iid)
  
}


derive_trend <- function(analyseset_species, trendmodel_nbinom) {
  
  #gemiddelde jaarlijkse trend
  
  trend_average <- trendmodel_nbinom$marginals.fixed$year_scaled %>%
    inla.tmarginal(fun = function(x) (exp(x) - 1) * 100) %>%
    inla.zmarginal(silent = TRUE) %>%
    data.frame() %>%
    mutate(parameter = "trend_average",
           soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           jaar_min = min(analyseset_species$jaar),
           jaar_max = max(analyseset_species$jaar)) %>%
    select(parameter, soort_nl, soort_wet, jaar_min, jaar_max, mean, sd, lcl_0.95 = quant0.025, ucl_0.95 = quant0.975)
  
  trend_quantiles <- trendmodel_nbinom$marginals.fixed$year_scaled %>%
    inla.tmarginal(fun = function(x) (exp(x) - 1) * 100) %>%
    inla.qmarginal(p = c(0.05, 0.20, 0.35, 0.65, 0.80, 0.95)) %>%
    data.frame() %>%
    mutate(type = c("lcl_0.90", "lcl_0.60", "lcl_0.30", "ucl_0.30", "ucl_0.60", "ucl_0.90")) %>%
    spread(key = "type", value = ".")
  
  trend_average <- trend_average %>%
    bind_cols(trend_quantiles)
  
  #totale trend over volledige periode 
  
  periode <- n_distinct(analyseset_species$jaar)

  trend_totaal <- trendmodel_nbinom$marginals.fixed$year_scaled %>%
    inla.tmarginal(fun = function(x) (exp(x * (periode - 1)) - 1) * 100) %>%
    inla.zmarginal(silent = TRUE) %>%
    data.frame() %>%
    mutate(parameter = "trend_total",
           soort_nl = unique(analyseset_species$soort_nl),
           soort_wet = unique(analyseset_species$soort_wet),
           jaar_min = min(analyseset_species$jaar),
           jaar_max = max(analyseset_species$jaar)) %>%
    select(parameter, soort_nl, soort_wet, jaar_min, jaar_max, mean, sd, lcl_0.95 = quant0.025, ucl_0.95 = quant0.975)
  
  trend_totaal_quantiles <- trendmodel_nbinom$marginals.fixed$year_scaled %>%
    inla.tmarginal(fun = function(x) (exp(x * (periode - 1)) - 1) * 100) %>%
    inla.qmarginal(p = c(0.05, 0.20, 0.35, 0.65, 0.80, 0.95)) %>%
    data.frame() %>%
    mutate(type = c("lcl_0.90", "lcl_0.60", "lcl_0.30", "ucl_0.30", "ucl_0.60", "ucl_0.90")) %>%
    spread(key = "type", value = ".")
  
  trend_totaal <- trend_totaal %>%
    bind_cols(trend_totaal_quantiles)
  
  trend <- bind_rows(trend_average,
                     trend_totaal)
  
  return(trend)
  
}  

derive_trend_inlabru <- derive_trend 

get_waic <- function(analyseset_species, model) {
  
  result_waic <- data.frame(
    parameter = "waic",
    soort_nl = unique(analyseset_species$soort_nl),
    soort_wet = unique(analyseset_species$soort_wet),
    waic = model$waic$waic)
  
  return(result_waic)
}

get_waic_inlabru <- get_waic

check_random_effect <- function(model_inla){
  
  precision_loc <- model_inla$summary.hyperpar$mean[2]
  
  x <- data.frame(sigma = 1/sqrt(precision_loc),
                  precision = precision_loc,
                  sim_iid = as.numeric(simulate_iid(tau = precision_loc)))
  
  return(x)
}



simulate_data_model_inlabru <- function(analyseset_species, model_inlabru) {
  
  analyseset_species <- analyseset_species %>%
    mutate(fjaar = factor(jaar),
           locatie = as.character(locatie),
           locatie = as.factor(locatie))
  
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
                             ))
                             
  
  doy_year_loc_effect <- predict(model_inlabru, 
                                 data = doy_year_loc_simdata, 
                                 formula = as.formula(
                                   str_c(
                                   "~ exp(Intercept + doy_scaled + doy_scaled_2 + ",
                                   fjaar_formula,
                                   " + site)")
                                   )
                                 )
  
  doy_loc_simdata <- expand.grid(
    soort_nl = unique(analyseset_species$soort_nl),
    loc_id = 1:n_loc)

  doy_site_effect <- predict(model_inlabru,
                             data = doy_loc_simdata,
                             formula = as.formula(
                               str_c(
                                 "~ exp(Intercept + site)")
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

get_results_analysis <- function(path = "analysis_vlinders", name_analysis) {
  
  if (dir.exists(fileman_up(path))) {
    
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
    
     
  
classification_tw <- function(lcl, ucl, threshold_low, treshold_high, reference = 0) {
  # assert_that(is.numeric(lcl), is.numeric(ucl), length(lcl) == length(ucl),
  #             is.numeric(threshold), noNA(threshold), is.number(reference),
  #             noNA(reference), all(lcl <= ucl))
  # if (length(threshold) == 1) {
  #   threshold <- reference + c(-1, 1) * abs(threshold)
  # } else {
  #   # assert_that(length(threshold) == 2, min(threshold) < reference,
  #   #             reference < max(threshold))
  #   threshold <- sort(threshold)
  # }
  # 
  classification <- ifelse(
    ucl < treshold_high,
    ifelse(
      ucl < reference,
      ifelse(ucl < threshold_low, "--", ifelse(lcl < threshold_low, "-", "-~")),
      ifelse(lcl > reference, "+~", ifelse(lcl < threshold_low, "?-", "~"))
    ),
    ifelse(
      lcl > reference,
      ifelse(lcl > treshold_high, "++", "+"),
      ifelse(lcl > threshold_low, "?+", "?")
    )
  ) %>%
    factor(levels =  c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?"))

}

fit_indexmodel_rw_nbinom_inla <- function(analyseset_species, offset_var = NULL) {
  
  analyseset_species <- analyseset_species %>%
    mutate(locatie = as.character(locatie),
           locatie = as.factor(locatie),
           jaar_centered = jaar - min(jaar),
           doy_centered = doy - min(doy))
  
  formula_indexmodel <- as.formula("aantal ~ f(jaar_centered, model = \"rw1\", 
                                  hyper = list(theta = list(prior = \"pc.prec\", param = c(0.3, 0.05)))) + 
                                f(doy_centered, model = \"rw2\", 
                                  hyper = list(theta = list(prior = \"pc.prec\", param = c(0.01, 0.05)))) + 
                                 f(locatie, model = \"iid\", 
                                   hyper = list(theta = list(prior = \"pc.prec\", param = c(1, 0.05))))")
  
  if(is.null(offset_var)) {
    
    model <- inla(formula_indexmodel,
                                      family = "nbinomial",
                                      data = analyseset_species,
                                      control.compute = list(config = TRUE, waic = TRUE),
                                      control.predictor = list(compute = TRUE))
  } else {
    
    analyseset_species2 <- analyseset_species %>%
      rename(offset = offset_var)
    
    model <- inla(formula_indexmodel,
                                      family = "nbinomial",
                                      data = analyseset_species2,
                                      offset = offset,
                                      control.compute = list(config = TRUE, waic = TRUE),
                                      control.predictor = list(compute = TRUE))
    
  }
  
  return(model)
  
}

fit_trendmodel_rw_nbinom_inla <- function(analyseset_species, offset_var = NULL) {
  
  analyseset_species <- analyseset_species %>%
    mutate(locatie = as.character(locatie),
           locatie = as.factor(locatie),
           year_scaled = jaar - min(jaar),
           doy_centered = doy - min(doy))
  
  formula_indexmodel <- as.formula("aantal ~ year_scaled + 
                                f(doy_centered, model = \"rw2\", 
                                  hyper = list(theta = list(prior = \"pc.prec\", param = c(0.01, 0.05)))) + 
                                 f(locatie, model = \"iid\", 
                                   hyper = list(theta = list(prior = \"pc.prec\", param = c(1, 0.05))))")
  
  if(is.null(offset_var)) {
    
    model <- inla(formula_indexmodel,
                  family = "nbinomial",
                  data = analyseset_species,
                  control.compute = list(config = TRUE, waic = TRUE),
                  control.predictor = list(compute = TRUE))
  } else {
    
    analyseset_species2 <- analyseset_species %>%
      rename(offset = offset_var)
    
    model <- inla(formula_indexmodel,
                  family = "nbinomial",
                  data = analyseset_species2,
                  offset = offset,
                  control.compute = list(config = TRUE, waic = TRUE),
                  control.predictor = list(compute = TRUE))
    
  }
  
  return(model)
  
}


