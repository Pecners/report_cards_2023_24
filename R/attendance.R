library(tidyverse)
library(wisconsink12)

rc_rates <- make_mke_rc() |> 
  filter(school_year == "2022-23" & 
           !is.na(sch_att_rate) &
           accurate_agency_type == "Private") |> 
  select(dpi_true_id,
         school_name,
         rate = sch_att_rate) |> 
  mutate(rate = rate / 100)

these_rates <- make_mke_rc() |> 
  filter(school_year == "2021-22") |> 
  select(dpi_true_id,
         school_name) |> 
  left_join(attendance |> 
              filter(school_year == "2021-22" &
                       (group_by == "All Students")) |> 
              select(-school_year)) |> 
  mutate(rate = actual_days_of_attendance / possible_attendance_days) |> 
  filter(!is.na(rate)) |> 
  select(dpi_true_id,
         school_name,
         rate) |> 
  bind_rows(rc_rates)

for_export <- make_mke_rc() |> 
  filter(school_year == "2022-23" &
           !accurate_agency_type %in% c(
             "2r/2x Charter",
             "Instrumentality Charter"
           )) |> 
  select(dpi_true_id, school_name) |> 
  full_join(these_rates) |> 
  arrange(school_name)

write_csv(for_export, "data/2021-22 attendance.csv")


make_mke_rc() |> 
  filter(school_year == "2022-23" &
           !dpi_true_id %in% (make_mke_rc() |> 
           filter(school_year == "2021-22") |> 
           pull(dpi_true_id)) & 
           !accurate_agency_type %in% c("2r/2x Charter",
                                        "Non-Instrumentality Charter"))
