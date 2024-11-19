library(tidyverse)
library(wisconsink12)

used_ly <- read_rds("data/used_ly_ach.rda")
prof <- read_rds("data/wi_all_school_test_results.rda") |> 
  filter(pa == "pa") |> 
  rename(prof_count = total_count,
         prof_percent = perc) |> 
  select(-pa) |> 
  pivot_wider(names_from = test_subject, values_from = c(prof_count, prof_percent))



make_mke_rc() |> 
  filter(school_year == "2023-24") |> 
  mutate(ly_ach = if_else(dpi_true_id %in% used_ly, TRUE, FALSE)) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         school_enrollment,
         grade_band,
         overall_rating,
         overall_score, 
         ly_ach,
         sch_ach,
         sch_growth,
         sch_tgo,
         sch_ot) |> 
  left_join(prof) |> 
  arrange(desc(overall_score)) |> 
  # View()
  write_csv("data/mke_school_rc_slim.csv")

