library(tidyverse)
library(wisconsink12)

these_rc <- make_mke_rc() |> 
  filter(school_year > "2021-22") |> 
  select(school_year,
         dpi_true_id,
         sch_ach) |> 
  pivot_wider(names_from = school_year, values_from = sch_ach) |> 
  mutate(used_ly = ifelse(`2022-23` == `2023-24`, TRUE, FALSE)) |> 
  filter(used_ly) |> 
  pull(dpi_true_id)

saveRDS(these_rc, "data/used_ly_ach.rda")
