library(tidyverse)
library(wisconsink12)

make_mke_rc() |> 
  filter(school_year > "2021-22") |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         sch_ach) |> 
  pivot_wider(names_from = school_year, 
              values_from = sch_ach) |> 
  mutate(reverted = ifelse(`2022-23` == `2023-24`, TRUE, FALSE)) |> 
  group_by(reverted, accurate_agency_type) |> 
  count() |> 
  pivot_wider(names_from = reverted, values_from = n)



make_mke_rc() |> 
  filter(school_year == "2023-24" &
           overall_rating == "Significantly Exceeds Expectations") |> 
  mutate(ly_ach = if_else(dpi_true_id %in% used_ly, TRUE, FALSE)) |> 
  select(school_year,
         school_name,
         accurate_agency_type,
         school_enrollment,
         grade_band,
         overall_rating,
         overall_score, 
         ly_ach,
         sch_ach,
         sch_growth) |> 
  arrange(desc(overall_score)) |> 
  # View() |> 
  write_csv("data/mke_five_star_schools_2023_24.csv")

