library(tidyverse)
library(wisconsink12) 

levs <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations",
  "Meets Expectations",
  "Exceeds Expectations",
  "Significantly Exceeds Expectations"
)

mke_rc <- make_mke_rc() |> 
  filter(school_year > "2020-21") |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         overall_score,
         overall_rating) |> 
  pivot_wider(names_from = school_year, values_from = overall_score) |> 
  mutate(change = `2022-23` - `2021-22`)

rating_change <- make_mke_rc() |> 
  filter(school_year > "2020-21") |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         overall_rating) |> 
  mutate(or = factor(overall_rating,
                                    levels = levs) |> 
           as.numeric()) |> 
  select(-c(overall_rating)) |> 
  pivot_wider(names_from = school_year, values_from = or) |> 
  mutate(change = `2022-23` - `2021-22`) 

score_change <- make_mke_rc() |> 
  filter(school_year > "2020-21") |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         overall_score) |> 
  pivot_wider(names_from = school_year, values_from = overall_score) |> 
  mutate(change = `2022-23` - `2021-22`)