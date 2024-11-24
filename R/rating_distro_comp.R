library(tidyverse)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(scales)
library(wisconsink12)
library(cityforwardcollective)
library(glue)
library(ggtext)

mke_rc <- make_mke_rc()

mke_rc |> 
  filter(!is.na(overall_score)) |> 
  group_by(school_year) |> 
  count()

rated_schools <- mke_rc %>%
  ungroup() %>%
  mutate(clean_rating = case_when(str_detect(overall_rating, "\\^$") ~ str_remove_all(overall_rating, "\\^$"),
                                  TRUE ~ overall_rating)) %>%
  filter(!is.na(clean_rating))

rating_change <- rated_schools %>%
  filter(school_year > "2021-22" & !is.na(overall_score)) %>%
  group_by(school_year, clean_rating) %>%
  summarise(n = n(),
            total_enrollment = sum(school_enrollment, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(school_year) %>%
  mutate(perc = n / sum(n),
         perc_enr = total_enrollment / sum(total_enrollment),
         clean_rating = factor(clean_rating, levels = c("Fails to Meet Expectations",
                                                        "Meets Few Expectations",
                                                        "Meets Expectations",
                                                        "Exceeds Expectations",
                                                        "Significantly Exceeds Expectations"))) %>%
  arrange(school_year, clean_rating)

meeting <- rating_change |> 
  filter(as.numeric(clean_rating) > 2) |> 
  ungroup() |> 
  group_by(school_year) |> 
  summarise(schools = sum(n),
            students = sum(total_enrollment),
            perc_of_schools = sum(perc),
            perc_of_enr = sum(perc_enr))

three_groups <- rating_change |> 
  mutate(group = case_when(as.numeric(clean_rating) < 3 ~ "Low Quality",
                           as.numeric(clean_rating) == 3 ~ "Meets Expectations",
                           as.numeric(clean_rating) > 3 ~ "High Quality")) |> 
  ungroup() |> 
  group_by(school_year, group) |> 
  summarise(students = sum(total_enrollment),
            perc_of_enr = sum(perc_enr))



# percent of schools 
rating_change %>%
  ggplot(aes(clean_rating, perc, fill = school_year)) +
  geom_col(position = position_dodge(.5)) +
  scale_x_discrete(label = function(x) str_wrap(x, 15)) +
  scale_y_continuous(label = percent) +
  geom_text(aes(label = percent(perc, 1)), vjust = 2, color = "white", 
            position = position_dodge(width = .5)) +
  labs(y = "Percent of Rated Schools", x = "Report Card Rating",
       title = "Rating distribution of Milwaukee schools",
       fill = "School Year") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")

# percent of student enrollment

rating_change |> 
  filter(school_year > "2020-21") |> 
  ggplot(aes(clean_rating, perc_enr, fill = school_year)) +
  geom_col(position = position_dodge(.5)) +
  scale_x_discrete(label = function(x) str_wrap(x, 15)) +
  scale_y_continuous(label = "0% of\nenrollment", breaks = 0) +
  geom_text(aes(label = percent(perc_enr, 1)), vjust = 1.5, color = "white", 
            position = position_dodge(width = .75), 
            fontface = "bold", size = 5) +
  geom_segment(aes(x = 2.5, xend = 5.5,
                   y = .375, yend = .375), lineend = "round",
               color = "grey50") +
  geom_segment(aes(x = 2.5, xend = 2.5,
                   y = .375, yend = .35), lineend = "round",
               color = "grey50") +
  geom_segment(aes(x = 5.5, xend = 5.5,
                   y = .375, yend = .35), lineend = "round",
               color = "grey50") +
  geom_segment(aes(x = 4, xend = 4,
                   y = .375, yend = .39), lineend = "round",
               color = "grey50") +
  annotate(geom = "text", x = 4, y = .395, vjust = 0, 
           label = "62% of students were in schools\nMeeting Expectations in 2023-24", color = "grey50") +
  coord_cartesian(clip = "off") +
  labs(y = "", x = "Report Card Rating",
       title = "Report Card Ratings of Milwaukee Schools",
       subtitle = glue("6 out of 10 Milwaukee students are enrolled in schools meeting the state's expectations ",
                       "even though fewer than 1 in 5 are able to read, write, and do math on grade level.*") |>
         str_wrap(70),
       caption = glue("*The 1 in 5 rate is based on proficiency rates from the statewide assessment tests which form ",
                      "the foundation of Report Card scores.") |> 
         str_wrap(),
       fill = "School Year") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Verdana", lineheight = 1.1),
        plot.title = element_text(family = "Georgia", face = "bold",
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(color = "grey30",
                                     margin = margin(b = 20)),
        plot.caption = element_text(color = "grey50", size = 10,
                                    margin = margin(t = 20)),
        plot.title.position = "plot",
        legend.position = "bottom",
        axis.text.y = element_text(size = 8, hjust = .5))

ggsave("plots/2023-24/enrollment_by_rc_rating_citywide_23-24.png", bg = "white", width = 9, height = 7)

# no subtitle

rating_change |> 
  filter(school_year > "2020-21") |> 
  ggplot(aes(clean_rating, perc_enr, fill = school_year)) +
  geom_col(position = position_dodge(.5)) +
  scale_x_discrete(label = function(x) str_wrap(x, 15)) +
  scale_y_continuous(label = "0% of\nenrollment", breaks = 0) +
  geom_text(aes(label = percent(perc_enr, 1)), vjust = 1.5, color = "white", 
            position = position_dodge(width = .75), 
            fontface = "bold", size = 5) +
  geom_segment(aes(x = 2.5, xend = 5.5,
                   y = .375, yend = .375), lineend = "round",
               color = "grey50") +
  geom_segment(aes(x = 2.5, xend = 2.5,
                   y = .375, yend = .35), lineend = "round",
               color = "grey50") +
  geom_segment(aes(x = 5.5, xend = 5.5,
                   y = .375, yend = .35), lineend = "round",
               color = "grey50") +
  geom_segment(aes(x = 4, xend = 4,
                   y = .375, yend = .39), lineend = "round",
               color = "grey50") +
  annotate(geom = "text", x = 4, y = .395, vjust = 0, size = 5,
           label = "63% of students were in schools\nmeeting or exceeding expectations in 2023-24", color = "grey50") +
  annotate(geom = "text", x = 1, y = .3, vjust = 0, size = 5,
           color = "grey50",
           label = "Share of students in\none star schools increased") +
  geom_segment(aes(x = 1, xend = 1,
                   y = .29, yend = .15), lineend = "round",
               color = "grey50", 
               arrow = arrow(length = unit(3, "mm"), type = "closed")) +
  coord_cartesian(clip = "off") +
  labs(y = "", x = "Report Card Rating",
       title = "Milwaukee student enrollment by School Report Card rating",
       # title = glue("Comparing <span style='color:{cfc_darkblue}'>**2021-22 school year**</span> ",
       #                 "with <span style='color:{cfc_orange}'>**2023-24 school year**</span> results"),
       fill = "School Year") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Verdana", lineheight = 1.1),
        plot.title = element_text(family = "Georgia", face = "bold",
                                  margin = margin(b = 40), size = 16),
        plot.caption = element_text(color = "grey50", size = 10,
                                    margin = margin(t = 20)),
        plot.title.position = "plot",
        legend.position = c(.15, 1.02),
        legend.direction = "horizontal",
        # legend.position = "top",
        # legend.margin = margin(b = 20),
        axis.text.y = element_text(size = 8, hjust = .5))

ggsave("plots/2023-24/enrollment_by_rc_rating_citywide_23-24_no_sub.png", bg = "white",
       width = 9, height = 7)

