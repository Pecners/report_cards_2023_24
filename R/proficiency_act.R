mps <- c("Traditional Public",
         "Instrumentality Charter")

charters <- c("Non-Instrumentality Charter",
              "2r/2x Charter")

##########
# ACT    #
##########


mke_schools <- make_mke_schools() %>%
  mutate(accurate_agency_type = case_when(accurate_agency_type %in% mps ~ "Milwaukee Public Schools",
                                          accurate_agency_type %in% charters ~ "Public Charter Schools",
                                          accurate_agency_type == "Private" ~ "Private Choice Schools",
                                          TRUE ~ accurate_agency_type))

mke_tested_students_aat <- act %>%
  filter(str_detect(group_by_value, regex("race", ignore_case = TRUE)) & test_subject %in% c("ELA", "Mathematics")) %>%
  right_join(., mke_schools) %>%
  filter(!accurate_agency_type %in% c("Partnership", "Private Choice Schools")) %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED"),
         accurate_agency_type = factor(accurate_agency_type,
                                       levels = c("Milwaukee Public Schools",
                                                  "Public Charter Schools",
                                                  "Private Choice Schools"))) %>%
  filter(pa != "REDACTED")


private  <- mke_schools %>%
  filter(accurate_agency_type == "Private Choice Schools") %>%
  select(dpi_true_id, school_year, accurate_agency_type) %>%
  left_join(., forward_exam) %>%
  filter(test_group == "ACT") %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED"),
         accurate_agency_type = factor(accurate_agency_type,
                                       levels = c("Milwaukee Public Schools",
                                                  "Public Charter Schools",
                                                  "Private Choice Schools"))) %>%
  filter(pa != "REDACTED" & test_subject %in% c("ELA", "Mathematics"))

##########
# Aspire #
##########

aspire <- forward_exam |> 
  filter(group_by_value == "All Students" & test_group == "Aspire") %>%
  right_join(., mke_schools) %>%
  filter(accurate_agency_type != "Partnership") %>%
  mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                        test_result %in% c("Proficient", "Advanced") ~ "pa",
                        test_result == "No Test" ~ "no_test",
                        TRUE ~ "REDACTED"),
         accurate_agency_type = factor(accurate_agency_type,
                                       levels = c("Milwaukee Public Schools",
                                                  "Public Charter Schools",
                                                  "Private Choice Schools"))) %>%
  filter(pa != "REDACTED")

all <- bind_rows(aspire, private, mke_tested_students_aat)
