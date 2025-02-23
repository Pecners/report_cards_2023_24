floor_schools <- make_wi_rc(exclude_milwaukee = FALSE) |> 
  filter(school_year %in% c("2022-23", "2023-24")) |> 
  select(school_year,
         dpi_true_id,
         sch_ach) |> 
  pivot_wider(names_from = school_year, values_from = sch_ach) |> 
  mutate(ach_floor = ifelse(`2022-23` == `2023-24`, TRUE, FALSE)) |> 
  select(dpi_true_id, ach_floor)
