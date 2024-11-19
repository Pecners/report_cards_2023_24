forward_exam |> 
  filter(school_year > "2020-21" &
           group_by == "All Students" &
           test_subject %in% c("ELA", "Mathematics") &
           test_group != "ACT") |> 
  # ggplot(aes(test_group, student_count)) +
  # geom_col()
  # pull(test_group) |>
  # unique()
  # View()
  group_by(school_year, dpi_true_id, test_subject, test_result) |> 
  summarise(total = sum(student_count, na.rm = TRUE)) |> 
  bind_rows(
    act |> 
      filter(school_year > "2020-21" &
               group_by == "All Students" &
               test_subject %in% c("ELA", "Mathematics")) |> 
      # pull(test_group) |> 
      # unique()
      # View()
      group_by(school_year, dpi_true_id, test_subject, test_result) |> 
      summarise(total = sum(student_count, na.rm = TRUE)) 
  ) |> 
  group_by(school_year, dpi_true_id, test_subject, pa = test_result %in% c("Proficient", "Advanced")) |> 
  summarise(total = sum(total)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa)


elem <- forward_exam %>%
  filter(group_by_value == "All Students" &
           test_subject %in% c("ELA", "Mathematics") &
           grade %in% c("3", "4", "5", "6", "7", "8")) %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  group_by(school_year, dpi_true_id, test_subject, pa) |> 
  summarise(total_count = sum(student_count, na.rm = TRUE))

elem |> 
  filter(school_year > "2020-21") |> 
  # ungroup() %>%
  # group_by(school_year, accurate_agency_type, dpi_true_id, test_subject) %>%
  # mutate(perc = (total_count / sum(total_count))) |> 
  #filter(pa == "pa") %>%
  # select(-total_count) %>%
  ungroup() %>%
  filter(test_subject %in% c("ELA", "Mathematics")) |> 
  group_by(school_year, test_subject, pa) |> 
  summarise(total = sum(total_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)


state_act <- act %>%
  filter(school_year > "2020-21") |> 
  left_join(schools) |> 
  filter(group_by_value == "All Students" & 
           test_subject %in% c("ELA", "Mathematics") &
           !accurate_agency_type %in% c("Partnership", "Private")) %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  filter(pa != "REDACTED")



private  <- schools %>%
  filter(accurate_agency_type == "Private") %>%
  select(dpi_true_id, school_year, accurate_agency_type) %>%
  left_join(., forward_exam) %>%
  filter(test_group == "ACT") %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  filter(pa != "REDACTED" & test_subject %in% c("ELA", "Mathematics"))

aspire <- forward_exam |> 
  filter(group_by_value == "All Students" & 
           ((test_group %in% c("Aspire", "DLM") &
               grade %in% c("9", "10")) |
              test_group == "PreACT")) %>%
  right_join(., schools) %>%
  filter(accurate_agency_type != "Partnership") %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  filter(pa != "REDACTED")

all <- bind_rows(aspire, private, state_act)

hs <- all |> 
  group_by(school_year, dpi_true_id, accurate_agency_type, test_subject, pa) %>%
  summarise(total_count = sum(student_count, na.rm = TRUE))

hs |> 
  filter(school_year > "2020-21") |> 
  # ungroup() %>%
  # group_by(school_year, accurate_agency_type, dpi_true_id, test_subject) %>%
  # mutate(perc = (total_count / sum(total_count))) |> 
  #filter(pa == "pa") %>%
  # select(-total_count) %>%
  ungroup() %>%
  filter(test_subject %in% c("ELA", "Mathematics")) |> 
  group_by(school_year, test_subject, pa) |> 
  summarise(total = sum(total_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)

state_act |> 
  filter(school_year == "2022-23") |> 
  # ungroup() %>%
  # group_by(school_year, accurate_agency_type, dpi_true_id, test_subject) %>%
  # mutate(perc = (total_count / sum(total_count))) |> 
  #filter(pa == "pa") %>%
  # select(-total_count) %>%
  ungroup() %>%
  filter(test_subject %in% c("ELA", "Mathematics")) |> 
  group_by(school_year, test_subject, pa) |> 
  summarise(total = sum(student_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)

