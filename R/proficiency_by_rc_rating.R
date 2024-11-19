library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)
library(scales)
library(glue)

prof <- read_rds("data/wi_all_school_test_results.rda")

t <- prof |> 
  group_by(school_year,
           test_subject,
           pa) |> 
  summarise(total = sum(total_count)) |> 
  ungroup() |> 
  group_by(school_year, test_subject) |> 
  mutate(perc = total / sum(total),
         meeting = "Citywide") |> 
  ungroup()

ty_citywide <- t |> 
  filter(school_year == "2023-24") 
  

no_rc <- prof |> 
  filter(school_year == "2021-22") |> 
  select(school_year, dpi_true_id) |> 
  unique() |> 
  anti_join(make_mke_rc() |> 
              filter(!is.na(overall_score)))

prof |> 
  filter(school_year == "2021-22" & dpi_true_id %in% no_rc$dpi_true_id) |> 
  group_by(school_year, pa, test_subject) |> 
  summarise(total = sum(total_count)) |> 
  ungroup() |> 
  group_by(school_year, test_subject) |> 
  mutate(perc = total / sum(total))


rc_with_prof <- make_mke_rc() |> 
  filter(school_year == "2023-24") |> 
  left_join(prof) |> 
  group_by(meeting = case_when(overall_score >= 70.0 ~ "Schools Exceeding\nExpectations", 
                               overall_score >= 58.0 ~ "Schools Meeting\nExpectations",
                               !is.na(overall_score) ~ "Schools Not Meeting\nExpectations",
                               TRUE ~ "NO SCORE"),
           school_year,
           pa,
           test_subject) |> 
  summarise(total = sum(total_count, na.rm = TRUE)) |> 
  filter(!is.na(pa) & meeting != "NO SCORE") |> 
  ungroup() |> 
  group_by(meeting,
           school_year,
           test_subject) |> 
  mutate(perc = total / sum(total))

# prof_pa <- prof |> 
#   filter(pa == "pa")
# 
# 
# rc_with_prof_pa <- make_mke_rc() |> 
#   filter(school_year == "2021-22") |> 
#   left_join(prof_pa) |> 
#   group_by(meeting = overall_score >= 58.0,
#            school_year,
#            pa,
#            test_subject) |> 
#   summarise(wm = weighted.mean(total_count, w = perc)) 

with_city <- rc_with_prof |> 
  # bind_rows(ty_citywide) |> 
  mutate(test_subject = ifelse(test_subject == "Mathematics", "Math", test_subject),
         pa = case_when(pa == "bb" ~ "Below Grade Level",
                        pa == "pa" ~ "At or Above Grade Level",
                        pa == "no_test" ~ "No Test")) |> 
  arrange(meeting, test_subject, desc(pa)) |> 
  group_by(meeting, test_subject) |> 
  mutate(cum = cumsum(perc) - perc / 2)
  
with_city |> 
  ggplot(aes(test_subject, perc, fill = pa)) +
  geom_col(width = .75) +
  geom_text(aes(label = label_percent(1)(perc), y = cum,
                color = ifelse(pa == "No Test", cfc_darkblue, "white")),
            fontface = "bold") +
  # geom_text(data = with_city |> 
  #             filter(pa == "At or Above Grade Level"),
  #           aes(label = ifelse(meeting == "Citywide" & test_subject == "ELA", 
  #               paste0(label_percent(1)(perc), "\n at or above\ngrade level"),
  #               label_percent(1)(perc)),
  #               vjust = ifelse(meeting == "Citywide" & test_subject == "ELA",
  #                              0, .5)),
  #           y = 1.025, lineheight = .9) +
  scale_y_continuous(labels = label_percent()) +
  scale_x_discrete(expand = c(.4, .4)) +
  scale_fill_manual(label = label_wrap(80), values = cfc_colors) +
  scale_color_identity() +
  facet_wrap(~ meeting, strip.position = "bottom", nrow = 1) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.text = element_text(size = 14),
        text = element_text(family = "Verdana", lineheight = 1.1),
        plot.title = element_text(family = "Georgia", face = "bold",
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(color = "grey30",
                                     margin = margin(b = 40)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.placement = "outside",
        # strip.background = element_rect(fill = "red"),
        strip.text = element_text(margin = margin(t = 10, b = 10),
                                  face = "bold"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 16, margin = margin(r = 10)),        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, color = "grey50", size = 8,
                                    margin = margin(t = 20))) +
  labs(x = "", y = "Percent of Students",
       title = "State Assessment Results by Report Card Rating for Milwaukee Schools",
       subtitle = glue("In Milwaukee, schools exceeding expectations had 1 in 4 ",
                       "students reading, writing, and doing math on grade level. Schools ",
                       "meeting expectations had 1 in 10 students on grade level, ",
                       "and schools not meeting expectations had 1 in 20 students ",
                       "on grade level.") |> 
         str_wrap(90),
       fill = "",
       caption = glue("Sources: Wisconsin State Assessment and Report Card data. ",
                      "Note that these data sources redact data in cases where ",
                      "student group sizes are too small. ",
                      "Data limited to schools that received an Overall Score on ",
                      "their School Report Card. Values may not add to 100% due to rounding.") |> 
         str_wrap(120)) +
  guides(fill = guide_legend(label.theme = element_text(size = 10)))

ggsave("plots/proficiency_by_rc_rating.png", bg = "white")



rc_with_prof |> 
  group_by(test_subject, pa) |> 
  summarise(total = sum(total)) |> 
  ungroup() |> 
  group_by(test_subject) |> 
  mutate(perc = total / sum(total))

