targets <- c("Meets Expectations",
             "Exceeds Expectations",
             "Significantly Exceeds Expectations")

demo_gaps <- make_mke_rc() %>%
  filter(school_year > "2021-22" & str_detect(overall_rating, "Expectations")) %>%
  pivot_longer(cols = starts_with("per_"), names_to = "group", values_to = "value") %>%
  filter(!group %in% c("per_choice", "per_open", "per_lep")) %>%
  mutate(group = ifelse(group %in% c("per_am_in",
                                     "per_tom",
                                     "per_nh_opi"), "per_other", group),
         quality = ifelse(overall_rating %in% targets, "hq", "not"),
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

demo_gaps %>%
  # filter(school_year == "2022-23") %>%
  ggplot(aes(group, value, fill = school_year)) +
  # geom_col(aes(fill = cfc_darkblue), position = position_nudge(x = -.1), width = .5) +
  geom_col(data = demo_gaps %>% filter(school_year == "2023-24"), aes(fill = cfc_orange),
           position = position_nudge(x = .1), width = .5) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  scale_y_continuous(breaks = c(-1, 0, 1), limits = c(-1.05, 1.05), 
                     labels = str_wrap(c("Not High Quality", "0%", "High Quality"), 10)) +
  scale_alpha_discrete(range = c(.25, 1), guide = "none") +
  geom_text(data = demo_gaps %>%
              filter(quality == "hq"),
            aes(label = ifelse(school_year == "2023-24", percent(value, 1), ""),
                y = ifelse(school_year == "2023-24" & value > .8, .75, value - .1)),
            position = position_dodge(width = .55), color = "White",
            size = 3) +
  scale_fill_identity(name = "School Year",
                      breaks = c(cfc_darkblue, cfc_orange),
                      labels = c("2022-23", "2023-24"),
                      guide = "legend") +
  geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 0, ymax = -1), fill = "white", alpha = .05) +
  geom_hline(yintercept = 0, color = "grey50", linetype = 2, size = 1) +
  labs(x = "", y = "Enrollment in High Quality Schools", fill = "",
       title = "Opportunity gap trends remain in 2022-23",
       subtitle = "Text labels show 2020-21 percentages",
       caption = "High Quality defined here as 3, 4, or 5 star schools.") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.line.x = element_line(arrow = arrow(length = unit(.25, "cm"), ends = "both")),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)) +
  coord_flip()
