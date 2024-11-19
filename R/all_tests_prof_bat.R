library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)
library(scales)
library(glue)

prof <- read_rds("data/all_school_prof.rda")

# Excluding Partnership schools

bat_prof <- left_join(prof, schools |> 
            select(school_year,
                   dpi_true_id,
                   accurate_agency_type,
                   broad_agency_type)) |> 
  filter(accurate_agency_type != "Partnership") |> 
  mutate(is_prof = ifelse(pa == "pa", TRUE, FALSE)) |> 
  group_by(school_year,
           broad_agency_type,
           test_subject,
           is_prof) |> 
  summarise(total = sum(total_count)) |> 
  ungroup() |> 
  group_by(school_year, test_subject, broad_agency_type) |> 
  mutate(perc = total / sum(total)) |> 
  filter(school_year == "2021-22")

bat_prof |> 
  mutate(perc = ifelse(is_prof, perc, perc * -1),
         broad_agency_type = case_when(broad_agency_type == "District Operated" ~ "Milwaukee Public Schools",
                                       broad_agency_type == "Independently Operated" ~ "Public Charter Schools",
                                       broad_agency_type == "Private" ~ "Private Schools"),
         broad_agency_type = factor(broad_agency_type, levels = c("Milwaukee Public Schools",
                                                                  "Public Charter Schools",
                                                                  "Private Schools"))) |> 
  filter(is_prof) |> 
  ggplot(aes(broad_agency_type, perc, fill = test_subject)) +
  geom_col(width = .5, position = "dodge") +
  geom_text(aes(label = ifelse(broad_agency_type == "Milwaukee Public Schools" &
                                 test_subject == "ELA",
                               glue("{label_percent(.1)(perc)} ",
                                    "at or above grade level") |> 
                                 str_wrap(20),
                               label_percent(.1)(perc))),
            position = position_dodge(width = .5),
            fontface = "bold", vjust = -.5,
            lineheight = .9) +
  scale_y_continuous(labels = label_percent()) +
  scale_x_discrete(labels = label_wrap(18)) +
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
        panel.grid.major = element_blank(),
        # panel.spacing = unit(0, "lines"),
        strip.placement = "outside",
        # strip.background = element_rect(fill = "red"),
        strip.text = element_text(margin = margin(t = 10, b = 10),
                                  face = "bold"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 16, margin = margin(r = 10)),        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, color = "grey50", size = 8,
                                    margin = margin(t = 20))) +
  labs(x = "", fill = "",
       title = "Share of Students on Grade Level",
       subtitle = glue("Across all three sectors, fewer than one in five students ",
                       "is able to read, write, and do math on grade level.") |> 
         str_wrap())

ggsave("plots/all_test_prof_bat.png", bg = "white",
       width = 9, height= 7)
