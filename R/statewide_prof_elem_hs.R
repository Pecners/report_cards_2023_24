library(tidyverse)
library(wisconsink12)
library(scales)

mps <- c("Traditional Public",
         "Instrumentality Charter")

charters <- c("Non-Instrumentality Charter",
              "2r/2x Charter")

city_charters <- c(
  "8007_8105",
  "8105_1211", #central city cyber
  "8127_0400", # fuller first code
  "8011_8127", # fuller second code
  "8109_0100",
  "8009_8109",
  "8006_8101",
  "8101_1056",
  "8026_8131",
  "8131_0400",
  "8008_8106",
  "8106_1251",
  "8012_8128",
  "8128_0800"
)


#######
# ACT #
#######

# mke_schools <- make_mke_schools() %>%
#   mutate(accurate_agency_type = case_when(dpi_true_id %in% city_charters ~ "City Charter",
#                                           accurate_agency_type %in% mps ~ "Milwaukee Public Schools",
#                                           accurate_agency_type %in% charters ~ "Public Charter Schools",
#                                           accurate_agency_type == "Private" ~ "Private Choice Schools",
#                                           TRUE ~ accurate_agency_type))

mke_tested_students_aat <- act %>%
  filter(group_by_value == "All Students" & test_subject %in% c("ELA", "Mathematics")) %>%
  left_join(schools) %>%
  filter(!accurate_agency_type %in% c("Partnership", "Private")) %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  filter(pa != "REDACTED")


private  <- schools %>%
  filter(accurate_agency_type == "Private") %>%
  select(dpi_true_id, school_year, accurate_agency_type) %>%
  left_join(forward_exam) %>%
  filter(test_group %in% c("ACT", "DLM") &
           grade == "11") %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  filter(pa != "REDACTED" & test_subject %in% c("ELA", "Mathematics"))

##########
# Aspire #
##########

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

all <- bind_rows(aspire, private, mke_tested_students_aat)

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
  summarise(total = sum(total_count, na.rm = TRUE)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)


elem <- forward_exam %>%
  filter(group_by_value == "All Students") %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED")) %>%
  group_by(school_year, dpi_true_id, test_subject, pa) |> 
  summarise(total_count = sum(student_count, na.rm = TRUE))

elem |> 
  filter(school_year == "2022-23") |> 
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


all_prof <- bind_rows(hs, elem) |> 
  group_by(school_year,
           dpi_true_id,
           test_subject,
           pa) |> 
  summarise(total_count = sum(total_count)) |> 
  # ungroup() %>%
  # group_by(school_year, accurate_agency_type, dpi_true_id, test_subject) %>%
  # mutate(perc = (total_count / sum(total_count))) |> 
  #filter(pa == "pa") %>%
  # select(-total_count) %>%
  ungroup() %>%
  filter(test_subject %in% c("ELA", "Mathematics"))


# saveRDS(all_prof, "../report_cards_2021-22/data/all_school_prof.rda")

x <- all_prof |> 
  filter(school_year == "2022-23" & pa == "pa") |> 
  right_join(make_mke_rc() |> 
               filter(school_year == "2022-23") |> 
               select(dpi_true_id,
                      school_year,
                      school_name)) |> 
  # select(-total_count) |> 
  pivot_wider(names_from = test_subject, values_from = total_count) |> 
  # arrange(desc(ELA + Mathematics)) |>
  arrange(school_name) |> 
  # mutate(missing_aspire = ifelse(highest_enrolled_grade %in% c(
  #   "9", "10", "11", "12"
  # ), TRUE, FALSE)) |> 
  transmute(dpi_true_id,
            "School Year" = school_year,
            "School Name" = school_name,
            "Sector" = accurate_agency_type,
            # "Highest Grade" = highest_enrolled_grade,
            # "ELA Proficiency" = label_percent(.1)(ELA),
            # "Math Proficiency" = label_percent(.1)(Mathematics)
            "ELA Proficiency" = ELA,
            "Math Proficiency" = Mathematics)

# write_csv(x, "data/MKE Schools Proficiency 2022-23.csv")
write_csv(x, "data/MKE Schools Proficiency Counts 2022-23.csv")


these <- all_prof |> 
  ungroup() |> 
  group_by(school_year, accurate_agency_type, dpi_true_id, test_subject, pa) |> 
  summarise(total = sum(total_count)) |> 
  ungroup() |> 
  group_by(school_year, dpi_true_id, test_subject) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  mutate(dpi_true_id = case_when(dpi_true_id == "8105_1211" ~ "8007_8105",
                                 dpi_true_id == "8127_0400" ~ "8011_8127",
                                 dpi_true_id == "8109_0100" ~ "8009_8109",
                                 dpi_true_id == "8101_1056" ~ "8006_8101",
                                 dpi_true_id == "8131_0400" ~ "8026_8131",
                                 dpi_true_id == "8106_1251" ~ "8008_8106",
                                 dpi_true_id == "8128_0800" ~ "8012_8128",
                                 dpi_true_id == "3619_0213" ~ "8027_8152",
                                 dpi_true_id == "8152_8152" ~ "8027_8152",
                                 TRUE ~ dpi_true_id)) |> 
  arrange(school_year) |> 
  pivot_wider(names_from = school_year, values_from = perc) |>
  left_join(schools |> 
              filter(school_year == "2022-23") |> 
              select(dpi_true_id, school_name)) |> 
  select(-pa) |> 
  select(dpi_true_id, school_name, accurate_agency_type, everything()) |> 
  filter(!is.na(school_name) | dpi_true_id %in% city_charters) |> 
  mutate(`2019-20` = NA) 



write_csv(these, "../000_data_temp/prof_scores.csv")

# by sector

all_prof |> 
  filter(school_year > "2021-22") |> 
  group_by(school_year, test_subject, pa) |> 
  summarise(total = sum(total_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)

# citywide

all_prof |> 
  filter(school_year %in% c("2018-19", "2021-22", "2022-23")) |> 
  left_join(schools |> 
              select(dpi_true_id,
                     school_year,
                     broad_agency_type)) |> 
  group_by(school_year, test_subject, pa) |> 
  summarise(total = sum(total_count)) |> 
  mutate(perc = total / sum(total)) |> 
  filter(pa == "pa") |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)



