library(tidyverse)
library(here)

source(here("src", "functions_smp.R"))


name_analysis_gebiedstelling <- "libellen_gebiedstelling_2021-02-10"
name_analysis_transecttelling <- "libellen_transecten_2021-02-10"

results_analysis_gebiedstelling <- get_results_analysis(name_analysis = name_analysis_gebiedstelling)
results_analysis_transecttelling <- get_results_analysis(name_analysis = name_analysis_transecttelling)

result_trendmodel <- bind_rows(
  results_analysis_gebiedstelling$trendmodel,
  results_analysis_transecttelling$trendmodel)

result_trendmodel_10year <- result_trendmodel %>%
  filter(parameter == "trend_average") %>%
  mutate(periode = jaar_max - jaar_min + 1,
         parameter = "trend_10years",
         trend_10 = ((mean/100 +1)^9 - 1) * 100,
         trend_10_lcl_0.90 = ((lcl_0.90/100 +1)^9 - 1) * 100,
         trend_10_ucl_0.90 = ((ucl_0.90/100 +1)^9 - 1) * 100,
         trend_10_lcl_0.95 = ((lcl_0.95/100 +1)^9 - 1) * 100,
         trend_10_ucl_0.95 = ((ucl_0.95/100 +1)^9 - 1) * 100,) %>%
  select(soort_nl, soort_wet, jaar_max, jaar_min, parameter, starts_with("trend_10"))

write_csv2(result_trendmodel_10year, "analysis_libellen/output/libellen_trend_10years.csv")
         