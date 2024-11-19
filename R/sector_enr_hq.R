not <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations"
)

bat_hq <- mke_rc %>%
  filter(school_year == "2022-23" & str_detect(overall_rating, "Expectations") &
           accurate_agency_type != "Partnership") %>%
  mutate(hq = ifelse(overall_rating %in% not, FALSE, TRUE),
         sector = case_when(broad_agency_type == "District Operated" ~ "MPS Schools",
                            broad_agency_type == "Independently Operated" ~ "Public Charter Schools",
                            broad_agency_type == "Private" ~ "Private Schools",
                            TRUE ~ "WHOOPS!")) %>%
  group_by(sector, hq) %>%
  summarise(total_enr = sum(school_enrollment, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(sector) %>%
  mutate(perc = total_enr / sum(total_enr),
         perc = ifelse(hq, perc, perc * -1),
         sector = factor(sector, levels = rev(c("MPS Schools",
                                            "Public Charter Schools",
                                            "Private Schools"))))

bat_hq %>%
  ggplot(aes(sector, perc, fill = hq)) +
  geom_col(width = .5) +
  geom_text(aes(label = percent(abs(perc), 1),
                hjust = ifelse(hq, 1.5, -.5)), color = "white",
            fontface = "bold") +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  scale_y_continuous(breaks = c(-.8, 0, .8),
                     limits = c(-.8, .85),
                     labels = c("Not Meeting Expectations",
                                "",
                                "Meeting Expectations")) +
  scale_fill_manual(values = c(cfc_orange, cfc_darkblue),
                    labels = c("Not Meeting Expectations",
                               "Meeting Expectations")) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50",
             size = 1.5) +
  coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "grey50",
                                   arrow = arrow(length = unit(.3, "cm"),
                                                 ends = "both",
                                                 type = "closed")),
        axis.text.x = element_text(hjust = c(0, .5, .9)),
        text = element_text(family = "Verdana", lineheight = 1.1),
        plot.title = element_text(family = "Georgia", face = "bold",
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(color = "grey30",
                                     margin = margin(b = 20)),
        plot.caption = element_text(color = "grey50", size = 10,
                                    margin = margin(t = 20)),
        plot.title.position = "plot",
        legend.position = "bottom") +
  labs(y = "", x = "", fill = "",
       title = "Sector Enrollment in Schools Meeting Expecations",
       subtitle = glue("Over three-quarters of students in private and public charter ",
                       "schools are in a school meeting expecations, while less than half ",
                       "of students in MPS are in a school meeting expecations.") |> 
         str_wrap(90))

ggsave("plots/2022-23/sector_enr_hq.png", bg = "white")
