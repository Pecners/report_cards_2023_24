source("R/one_pager_numbers/setup.R")


make_mke_enrollment(agency_type = "accurate") |> 
  filter(school_year == "2023-24" &
           accurate_agency_type != "Partnership") |> 
  mutate(group = case_when(
    accurate_agency_type %in% charter ~ "Public Charter",
    accurate_agency_type %in% MPS ~ "MPS",
    accurate_agency_type %in% oe_ch ~ "Open Enrollment/Chapter 220",
    TRUE ~ "Private"
  )) |> 
  group_by(group) |> 
  summarise(total = sum(total_enrollment)) |> 
  mutate(perc = (total / sum(total)) |> 
           label_comma(accuracy = .01)()) |> 
  adorn_totals()


make_mke_enrollment(agency_type = "accurate") |> 
  filter(school_year >= "2022-23") |> 
  pivot_wider(names_from = school_year, values_from = total_enrollment) |> 
  mutate(diff = `2023-24`-`2022-23`) |> 
  adorn_totals()


make_mke_schools() |> 
  filter(accurate_agency_type != "Partnership" &
           school_year == "2023-24") |> 
  group_by(broad_agency_type) |> 
  count()
