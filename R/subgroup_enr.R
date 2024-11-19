library(tidyverse)
library(wisconsink12)

years <- c(
  "2020-21",
  "2021-22"
)

not <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations"
)

sub_enr_hq <- make_mke_rc() |> 
  filter(school_year %in% years & !is.na(overall_score)) |> 
  pivot_longer(cols = starts_with("per_"), names_to = "group", values_to = "value") %>%
  filter(!group %in% c("per_choice", "per_open", "per_lep")) %>%
  mutate(group = ifelse(group %in% c("per_am_in",
                                     "per_tom",
                                     "per_nh_opi"), "per_other", group),
         quality = ifelse(overall_rating %in% not, "not", "meeting"),
         est_enr = school_enrollment * value)  %>%
  filter(!group %in% c("per_other", "per_asian")) %>%
  group_by(quality, group, school_year) %>%
  summarise(enr = sum(est_enr, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(group, school_year) %>%
  mutate(value = enr / sum(enr),
         value = ifelse(quality == "not", value * -1, value),
         group = case_when(group == "per_asian" ~ "Asian",
                           group == "per_b_aa" ~ "Black",
                           group == "per_hisp_lat" ~ "Hispanic/Latino",
                           group == "per_white" ~ "White",
                           group == "per_other" ~ "Other Race",
                           group == "per_swd" ~ "Students with Disabilities",
                           group == "per_ed" ~ "Economic Disadvantage"),
         group = factor(group, levels = rev(c("Asian",
                                              "Black",
                                              "Hispanic/Latino",
                                              "White",
                                              "Economic Disadvantage",
                                              "Students with Disabilities"))))
df <- sub_enr_hq |> 
  select(-enr) |> 
  mutate(value = abs(value)) |> 
  pivot_wider(names_from = school_year, values_from = value) |> 
  mutate(diff = `2020-21` - `2021-22`) |> 
  arrange(diff)
df

df |> 
  mutate(`2021-22` = ifelse(quality == "not", `2021-22` * -1, `2021-22`)) |> 
  ggplot(aes(group, `2021-22`, fill = quality)) +
  geom_col(width = .66) +
  scale_fill_manual(values = cfc_colors, guide = "none") +
  coord_flip()
