library(tidyverse)
library(cityforwardcollective)
library(wisconsink12)
library(glue)

scores <- make_mke_rc() %>%
  select(school_year, dpi_true_id,
         overall_score,
         sch_ach,
         sch_growth,
         sch_cg,
         sch_tgo,
         sch_ot) %>%
  pivot_longer(cols = -c(school_year, dpi_true_id), names_to = "cat", values_to = "score") %>%
  filter(!is.na(score)) %>%
  mutate(cat = case_when(cat == "overall_score" ~ "Overall Score",
                         cat == "sch_ach" ~ "Achievement",
                         cat == "sch_growth" ~ "Growth",
                         cat %in% c("sch_cg", "sch_tgo") ~ "Target Group",
                         cat == "sch_ot" ~ "On-Track"),
         cat = factor(cat, levels = c("Overall Score",
                                      "Achievement",
                                      "Growth",
                                      "Target Group",
                                      "On-Track"))) %>%
  group_by(school_year, cat) %>%
  summarise(avg_score = mean(score, na.rm = TRUE))

scores %>%
  filter(school_year > "2021-22") %>%
  ggplot(aes(cat, avg_score, fill = school_year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  geom_text(aes(label = format(round(avg_score, digits = 1), nsmall = 1)), position = position_dodge(.9), vjust = 2,
            color = "white") +
  labs(y = "Average Score for Milwaukee", x = "Score Category", fill = "School Year",
       title = "Overall Scores nudge up minimally,\nother scores drop"
       # subtitle = glue("The Overall Score is computed as a weighted average, ",
       #                 ) |> 
         # str_wrap()
         ) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Verdana", lineheight = 1.1),
        plot.title = element_text(family = "Georgia", face = "bold",
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(color = "grey30", margin = margin(b = 20)),
        plot.caption = element_text(color = "grey50", size = 10,
                                    margin = margin(t = 20)),
        plot.title.position = "plot",
        legend.position = "bottom",
        axis.text.y = element_text(size = 8, hjust = .5))


ggsave("plots/2022-23/priority_area_score_changes.png", bg = "white",
       width = 9, height = 7)

