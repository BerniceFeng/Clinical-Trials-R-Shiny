library(dplyr)
library(tidyr)
source("ct-util4-copy-copy.R")

conditions = conditions |>
  group_by(nct_id) %>%
  summarize(condition_name = str_flatten(downcase_name, ", "))
countries = countries |>
  group_by(nct_id) %>%
  summarize(country = str_flatten(name, ", "))
sponsors <- sponsors %>%
  mutate(sponsor_name = sql("name")) %>%
  select(-name)
calculated = calculated %>%
  group_by(nct_id) %>%
  summarize(min_age = min(minimum_age_num, na.rm = TRUE), 
            max_age = max(maximum_age_num, na.rm = TRUE))
studies = studies |> left_join(conditions, by = "nct_id")
studies = studies |> left_join(countries, by = "nct_id")
studies = studies |> left_join(sponsors, by = "nct_id")
studies = studies |> left_join(calculated, by = "nct_id")

# cumulative studies
d = studies |> 
  query_kwds("pembrolizumab", "brief_title") |>
  filter(source_class = "NIH") |>
  filter(sponsor = "lead") |>
  filter(study_type = "Interventional") |>
  collect() 

start_date_range = c("2000-1-1", "2050-12-31")
completion_date_range = c("2000-1-1", "2050-12-31")
if (!is.null (start_date_range)) {
  start_date <- as.Date(start_date_range [1])
  end_date <- as.Date(start_date_range [2])
  ret = ret |>
  filter (start_date - start_date & start_date <= end_date)
}
if (!is.null (completion_date_range)) {
  start_date_c <- as.Date(completion_date_range [1])
  end_date_c <- as.Date(completion_date_range [2])
  ret = ret |>
        filter (completion_date >= start_date_c & completion_date <= end_date_c)
}

age_start <- 0
age_end <- 100


# Phase Histogram
d |> plot_phase_histogram()
# Plot Concurrent Studies
d |> plot_concurrent_studies()
# Plot Conditions Histogram
d |> plot_conditions_histogram()
# Plot Countries Plot
d |> countries_plot
# Plot Status Plot
d |> status_plot