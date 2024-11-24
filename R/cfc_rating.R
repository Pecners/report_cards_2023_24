library(tidyverse)
library(wisconsink12)

# make_mke_rc() |> 
#   filter(is.na(sch_tgo) & 
#            !is.na(sch_ach) &
#            !is.na(sch_growth) &
#            school_year == "2022-23") |> 
#   select(school_year,
#          dpi_true_id,
#          sch_ach,
#          sch_growth,
#          sch_tgo,
#          sch_ot,
#          ends_with("weight")) |> 
#   mutate(new_agw = (ach_weight + growth_weight) / 2) |> 
#   View()

cfc_scored <- map2_df(c("sch_ach", "sch_growth", "sch_tgo", "sch_ot"),
                      c("ach_weight", "growth_weight", "tgo_weight", "ot_weight"),
                      function(x, y) {
                        make_wi_rc(exclude_milwaukee = FALSE) |> 
                          ungroup() |> 
                          filter(school_year == "2023-24") |> 
                          select(school_year,
                                 dpi_true_id,
                                 value = x,
                                 weight = y) |> 
                          mutate(score = x)
                        # new_weight = ifelse(aw, half_weight / 2, weight))
                      }) |> 
  group_by(school_year, 
           dpi_true_id,
           aw = score %in% c("sch_ach", "sch_growth")) |> 
  mutate(half_weight = sum(weight) / 2,
         new_weight = ifelse(aw, half_weight, weight)) |> 
  arrange(dpi_true_id) |> 
  ungroup() |> 
  group_by(school_year, dpi_true_id) |> 
  summarise(cfc_score = weighted.mean(value, new_weight, na.rm = TRUE) |> 
              round(1)) |> 
  ungroup() |> 
  mutate(cfc_rating = case_when(
    cfc_score < 48.0 ~ "Fails to Meet Expectations",
    cfc_score < 58.0 ~ "Meets Few Expectations",
    cfc_score < 70.0 ~ "Meets Expectations",
    cfc_score < 83.0 ~ "Exceeds Expectations",
    cfc_score < 100.0 ~ "Significantly Exceeds Expectations",
    TRUE ~ NA
  ))

all <- cfc_scored |> 
  right_join(make_mke_rc() |> 
               filter(school_year == "2023-24") |> 
               select(school_year, 
                      dpi_true_id, 
                      school_name,
                      accurate_agency_type,
                      school_enrollment,
                      overall_rating)) |> 
  mutate(cfc_rating = factor(cfc_rating, levels = c("Fails to Meet Expectations",
                                                        "Meets Few Expectations",
                                                        "Meets Expectations",
                                                        "Exceeds Expectations",
                                                        "Significantly Exceeds Expectations")))

all |> 
  group_by(cfc_rating) |> 
  summarise(total = sum(school_enrollment)) |> 
  mutate(perc_enr = total / sum(total, na.rm = TRUE)) |> 
  filter(!is.na(total)) |> 
  ggplot(aes(cfc_rating, perc_enr)) +
  geom_col(position = position_dodge(.5)) +
  scale_x_discrete(label = function(x) str_wrap(x, 15)) +
  scale_y_continuous(label = "0% of\nenrollment", breaks = 0) +
  geom_text(aes(label = percent(perc_enr, 1)), vjust = 1.5, color = "white", 
            position = position_dodge(width = .75), 
            fontface = "bold", size = 5) +
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
        legend.position = c(.1, .85),
        axis.text.y = element_text(size = 8, hjust = .5))


