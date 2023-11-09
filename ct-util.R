library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2) 
library(tidyr)
library(purrr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# Create the connection to a database and "studies" and "sponsors" tables.
con = dbConnect(
  duckdb(
    file.path("ctgov.duckdb"), 
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
conditions = tbl(con, "conditions")
countries = tbl(con, "countries")
calculated = tbl(con, "calculated_values")

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

#' @title Query keywords from a database table.
#' @description This function searches for rows in a database table where the specified column contains
#' any or all of the provided keywords. It allows for case-sensitive or insensitive searches and can match
#' rows containing all keywords (using AND logic) or any of the keywords (using OR logic). It's useful for
#' filtering data based on text search criteria.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords 
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query)) 
}
#' @title Create Histogram of Study Phases
#' @description Generates a histogram of different study phases from a dataset. This visualization helps 
#' to understand the distribution of studies across different clinical trial phases. If a keyword filter is 
#' applied to the dataset, it reflects the distribution for the filtered data.
#'  @param x A database table containing a 'phase' column from which to create the histogram.
#'  @param  brief_title_kw the brief title keywords to look for. This is optional.
plot_phase_histogram = function(x) {
  x = x |>
    select(phase) |>
    collect()
  
  all = studies |>
    select(phase) |>
    group_by(phase) |>
    distinct() |>
    collect()
    
  all_phase = x |> bind_rows(all) 
  
  all_phase$phase[is.na(all_phase$phase)] = "NA"
  
  all_phase = all_phase |> group_by(phase) |> summarize(n=n()-1)
  
  ggplot(all_phase, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}

#' @title Plot Concurrent Clinical Trials Over Time
#' @description Creates a line plot representing the number of concurrent clinical trials at any given date.
#' This function is useful to visualize the timeline and overlap of different studies.
#' @param studies the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
plot_concurrent_studies = function(studies) {
    
   all_dates = studies |> 
    select(start_date, completion_date)|>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |> 
    arrange(value) |>
    na.omit() |> 
    rename(date = value)
  
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }
  
  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count = 
    map_dbl(
      all_dates$date, 
      ~ .x |> 
        within_date(studies$start_date, studies$completion_date) |>
        sum(na.rm = TRUE)
    )
  
    all_dates |>
    ggplot(aes(x = date, y = count)) +
    geom_line() +
    xlab("Date") +
    ylab("Count") + 
    theme_bw()
}

#' @title Retrieve and Summarize Study Conditions
#' @description Processes a dataset to count and calculate the percentage of studies for each condition, 
#' filtering out conditions that represent a very small fraction of the dataset.
#' @param c A database table (tibble) with a 'name' column representing the condition names.
#' @return A tibble with condition names, counts, total count, and their percentage of the total.
get_conditions = function(c){
  condition = c |>
    select(condition_name) |>
    collect() |>
    separate_rows(condition_name, sep = ", ") |>
    group_by(condition_name) |>
    summarize(n=n()) |>
    mutate(sum = sum(n)) |>
    mutate(percentage = n/sum) |>
    filter(percentage > 0.003) |>
    arrange(desc(n))|>
    collect()
  
  return(condition)
}

#' @title Histogram of Study Conditions
#' @description Creates a histogram displaying the frequency of different conditions in a dataset of 
#' clinical studies. This function is useful for understanding the most researched medical conditions.
#' @param studies A database table (tibble) containing the study conditions data.
#' @return A ggplot2 histogram showing the distribution of conditions.
plot_conditions_histogram = function(studies) {
  studies |>
  get_conditions()|>
  as.data.frame() |>
  ggplot(aes(x = condition_name, y = n)) +
  geom_col() +
  xlab("Name") +
  ylab("Count") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' @title Map Visualization of Study Count by Country
#' @description Produces a choropleth world map indicating the number of studies conducted in each country. 
#' It provides a visual representation of global clinical research distribution.
#' @param studies A database table (tibble) containing the 'country' column with study locations.
#' @return A ggplot2 map with countries filled according to the count of studies.
plot_countries_map = function(studies) {
  ct = studies|> 
    select(country) |> 
    group_by(country) |> 
    summarize(n=n())
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  merged_data <- merge(world, ct, by.x = "name", by.y = "country", all.x = TRUE)
  
  ggplot() +
    geom_sf(data = merged_data, aes(fill = n)) +
    scale_fill_viridis_c() +
    labs(title = "World Map of Counts by Country") +
    theme_minimal()
}

#' @title Pie Chart of Study Status Distribution
#' @description Draws a pie chart to represent the distribution of overall statuses of clinical trials, such 
#' as 'Recruiting', 'Completed', or 'Terminated'. This helps to quickly grasp the proportion of studies 
#' in each category.
#' @param studies A tibble or dataframe with an 'overall_status' column.
#'  @return A ggplot object representing the pie chart of study status distribution.
plot_status_piechart = function(studies) {
  studies |>
  select(overall_status) |> 
  group_by(overall_status) |> 
  summarize(n=n()) |>
  ggplot(aes(x = "", y = n, fill = overall_status)) +
  geom_bar(stat = "identity") +
  coord_polar("y") + 
  labs(title = "Overall Status Distribution")
}
