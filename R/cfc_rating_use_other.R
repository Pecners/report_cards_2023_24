library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)

ty_rc <- make_mke_rc() |> 
  filter(school_year == "2023-24") |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         score_ach = sch_ach,
         score_growth = sch_growth,
         score_tgo = sch_tgo,
         score_ot = sch_ot,
         weight_ach = ach_weight,
         weight_growth = growth_weight,
         weight_tgo = tgo_weight,
         weight_ot = ot_weight)


ty_rc |> 
  pivot_longer(cols = c(score_ach:last_col()),
               names_to = c(".value", "set"),
               names_pattern = "(.*)_(.*)") |>
  mutate(group = ifelse(set %in% c("ach", "growth"), "ag", "other")) |> 
  group_by(dpi_true_id, group) |> 
  mutate(new_w = ifelse(group == "ag", sum(weight) / 2, weight)) |>
  ungroup() |> 
  group_by(dpi_true_id, school_name) |> 
  summarise(wm = weighted.mean(score, w = new_w, na.rm = TRUE))

               