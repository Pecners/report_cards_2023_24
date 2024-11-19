library(tidyverse)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(scales)
library(wisconsink12)
library(cityforwardcollective)

mke_rc <- make_mke_rc()

rated_schools <- mke_rc %>%
  ungroup() %>%
  mutate(clean_rating = case_when(str_detect(overall_rating, "\\^$") ~ str_remove_all(overall_rating, "\\^$"),
                                  TRUE ~ overall_rating)) %>%
  filter(!is.na(clean_rating))

rating_change_bat <- rated_schools %>%
  filter(school_year > "2017-18" & !is.na(overall_score) & accurate_agency_type != "Partnership") %>%
  group_by(school_year, clean_rating, broad_agency_type) %>%
  summarise(n = n(),
            total_enrollment = sum(school_enrollment, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(school_year, broad_agency_type) %>%
  mutate(perc = n / sum(n),
         perc_enr = total_enrollment / sum(total_enrollment),
         clean_rating = factor(clean_rating, levels = c("Fails to Meet Expectations",
                                                        "Meets Few Expectations",
                                                        "Meets Expectations",
                                                        "Exceeds Expectations",
                                                        "Significantly Exceeds Expectations"))) %>%
  arrange(school_year, clean_rating) |> 
  filter(school_year > "2021-22")

meeting_bat <- rating_change_bat |> 
  group_by(meeting = as.numeric(clean_rating) > 1,
           school_year, broad_agency_type) |> 
  summarise(schools = sum(n),
            students = sum(total_enrollment),
            perc_of_schools = sum(perc),
            perc_of_enr = sum(perc_enr))

three_groups_bat <- rating_change_bat |> 
  mutate(group = case_when(as.numeric(clean_rating) < 3 ~ "Low Quality",
                           as.numeric(clean_rating) == 3 ~ "Meets Expectations",
                           as.numeric(clean_rating) > 3 ~ "High Quality")) |> 
  ungroup() |> 
  group_by(school_year, broad_agency_type, group) |> 
  summarise(students = sum(total_enrollment),
            perc_of_enr = sum(perc_enr))

meeting_bat |> 
  filter(school_year == "2022-23" & meeting)

rating_change_bat |> 
  mutate(meeting = ifelse(as.numeric(clean_rating) >= 3, TRUE, FALSE)) |> 
  filter(school_year >= "2022-23") |> 
  arrange(broad_agency_type) |> 
  group_by(school_year, broad_agency_type, meeting) |> 
  summarise(total = sum(total_enrollment)) |> 
  mutate(perc = total / sum(total))

# just count students

rating_change_bat |> 
  mutate(meeting = ifelse(as.numeric(clean_rating) >= 3, TRUE, FALSE)) |> 
  filter(school_year >= "2022-23") |> 
  arrange(broad_agency_type) |> 
  group_by(school_year, broad_agency_type) |> 
  summarise(total = sum(total_enrollment)) |> 
  mutate(perc = total / sum(total))

# percent of schools 
rating_change_bat %>%
  filter(school_year > "2021-22") |> 
  ggplot(aes(clean_rating, perc, fill = school_year)) +
  geom_col(position = position_dodge(.5)) +
  scale_x_discrete(label = function(x) str_wrap(x, 15)) +
  scale_y_continuous(label = percent) +
  geom_text(aes(label = percent(perc, 1)), vjust = 2, color = "white", 
            position = position_dodge(width = .5)) +
  labs(y = "Percent of Rated Schools", x = "Report Card Rating",
       title = "Rating distribution of Milwaukee schools",
       fill = "School Year") +
  facet_wrap(~ broad_agency_type, ncol = 1) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")

# percent of student enrollment

rating_change_bat %>%
  ggplot(aes(clean_rating, perc_enr, fill = school_year)) +
  geom_col(position = position_dodge(.5)) +
  scale_x_discrete(label = function(x) str_wrap(x, 15)) +
  scale_y_continuous(label = percent) +
  geom_text(aes(label = percent(perc_enr, 1)), vjust = 2, color = "white", 
            position = position_dodge(width = .5)) +
  labs(y = "Percent of Enrollment", x = "Report Card Rating",
       title = "Report Card Ratings of Milwaukee Schools",
       fill = "School Year") +
  facet_wrap(~broad_agency_type, ncol = 1) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")
