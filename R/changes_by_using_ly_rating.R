library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)

used_ly <- read_rds("data/used_ly_ach.rda")

# raw change in oa score

this_rc <- make_mke_rc() |> 
  filter(school_year > "2021-22") |> 
  mutate(used_ly = ifelse(dpi_true_id %in% used_ly, TRUE, FALSE))

this_rc |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         overall_score,
         used_ly) |> 
  pivot_wider(names_from = school_year, 
              values_from = overall_score) |> 
  mutate(diff = `2023-24` - `2022-23`) |> 
  # View() 
  ggplot(aes(diff)) +
  geom_histogram()
  

# change in rating category

cats <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations",
  "Meets Expectations",
  "Exceeds Expectations",
  "Significantly Exceeds Expectations"
)

this_rc |> 
  mutate(overall_rating = factor(overall_rating, levels = cats) |> 
           as.numeric()) |> 
  filter(!is.na(overall_rating)) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         overall_rating) |> 
  pivot_wider(names_from = school_year, values_from = overall_rating) |> 
  mutate(diff = `2023-24` - `2022-23`) |> 
  # View()
  group_by(`2022-23`, diff) |> 
  count() |> View()
