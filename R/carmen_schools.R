library(tidyverse)
library(wisconsink12)

make_mke_rc() |> 
  filter(school_year == "2023-24") |> 
  View()

car <- c(
  "3619_0407",
  "3619_0451",
  "3619_0444"
)

# Carmen numbers

car_act <- act |> 
  filter(school_year == "2023-24" &
           dpi_true_id %in% car & 
           test_subject %in% c("Mathematics", "ELA") &
           group_by == "All Students")


forward_exam |> 
  filter(school_year == "2023-24" &
           dpi_true_id %in% car & 
           test_subject %in% c("Mathematics", "ELA") &
           grade %in% c("9", "10", "11", "12") & 
           group_by == "All Students") |> 
  bind_rows(car_act) |> 
  group_by(dpi_true_id, test_subject,
           pa = test_result %in% c("Advanced", "Meeting")) |> 
  summarise(total = sum(student_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa) |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc) |> 
  left_join(schools |> 
              filter(school_year == "2023-24") |> 
              select(dpi_true_id, school_name)) |> 
  select(dpi_true_id, school_name, ELA, Mathematics)

forward_exam |> 
  filter(school_year == "2023-24" &
           dpi_true_id %in% car & 
           test_subject %in% c("Mathematics", "ELA") &
           grade %in% c("9", "10", "11", "12") & 
           group_by == "All Students") |> 
  bind_rows(car_act) |> 
  group_by(test_subject,
           pa = test_result %in% c("Advanced", "Meeting")) |> 
  summarise(total = sum(student_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa) |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc) |> 
  left_join(schools |> 
              filter(school_year == "2023-24") |> 
              select(dpi_true_id, school_name)) |> 
  select(dpi_true_id, school_name, ELA, Mathematics)

# MPS

mps_act <- act |> 
  left_join(schools |> 
              select(school_year,
                     dpi_true_id, 
                     broad_agency_type)) |> 
  filter(str_detect(dpi_true_id, "^3619") &
           school_year == "2023-24" &
           broad_agency_type == "District Operated" & 
           test_subject %in% c("Mathematics", "ELA") &
           group_by == "All Students")


forward_exam |> 
  left_join(schools |> 
              filter(school_year == "2023-24") |> 
              select(school_year,
                     dpi_true_id, 
                     broad_agency_type)) |> 
  filter(str_detect(dpi_true_id, "^3619") &
           school_year == "2023-24" &
           broad_agency_type == "District Operated" & 
           test_subject %in% c("Mathematics", "ELA") &
           grade %in% c("9", "10", "11", "12") & 
           group_by == "All Students") |> 
  bind_rows(mps_act) |> View()
  group_by(test_subject,
           pa = test_result %in% c("Advanced", "Meeting")) |> 
  summarise(total = sum(student_count, na.rm = TRUE)) |> 
  mutate(perc = total / sum(total, na.rm = TRUE)) |> 
  filter(pa) |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc) |> 
  select(ELA, Mathematics)

  
# racial gaps
  
car_act <- act |> 
  filter(school_year == "2023-24" &
           dpi_true_id %in% car & 
           test_subject %in% c("Mathematics", "ELA") &
           group_by == "Race/Ethnicity")
  
forward_exam |> 
  filter(school_year == "2023-24" &
           dpi_true_id %in% car & 
           test_subject %in% c("Mathematics", "ELA") &
           grade %in% c("9", "10", "11", "12") & 
           group_by == "Race/Ethnicity") |> 
  bind_rows(car_act) |> 
  group_by(group_by_value, test_subject,
           pa = test_result %in% c("Advanced", "Meeting")) |> 
  summarise(total = sum(student_count, na.rm = TRUE)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa) |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc) 


mps_act <- act |> 
  left_join(schools |> 
              select(school_year,
                     dpi_true_id, 
                     broad_agency_type)) |> 
  filter(str_detect(dpi_true_id, "^3619") &
           school_year == "2023-24" &
           broad_agency_type == "District Operated" & 
           test_subject %in% c("Mathematics", "ELA") &
           group_by == "All Students")

forward_exam |> 
  filter(school_year == "2023-24" &
           str_detect(dpi_true_id, "^3619") & 
           test_subject %in% c("Mathematics", "ELA") &
           grade %in% c("9", "10", "11", "12") & 
           group_by == "Race/Ethnicity") |> 
  # left_join(schools |> 
  #             filter(school_year == "2023-24") |> 
  #             select(school_year,
  #                    dpi_true_id, 
  #                    broad_agency_type)) |> 
  bind_rows(mps_act) |> 
  group_by(group_by_value, test_subject,
           pa = test_result %in% c("Advanced", "Meeting")) |> 
  summarise(total = sum(student_count, na.rm = TRUE)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa) |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc) 


