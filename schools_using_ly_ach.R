library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)

used_ly <- read_rds("data/used_ly_ach.rda")

make_mke_rc() |> 
  filter(school_year == "2023-24") |> 
  mutate(used_ly = ifelse(dpi_true_id %in% used_ly, TRUE, FALSE)) |> 
  group_by(overall_rating, used_ly) |> 
  summarise(total = sum(school_enrollment)) |> 
  mutate(overall_rating = factor(overall_rating,
                                 levels = c(
                                   "Fails to Meet Expectations",
                                   "Meets Few Expectations",
                                   "Meets Expectations",
                                   "Exceeds Expectations",
                                   "Significantly Exceeds Expectations"
                                 ))) |> 
  filter(!is.na(overall_rating)) |> 
  ggplot(aes(overall_rating, total, fill = used_ly)) +
  geom_col(position = "dodge")


make_mke_rc() |> 
  filter(school_year == "2023-24") |> 
  mutate(used_ly = ifelse(dpi_true_id %in% used_ly, TRUE, FALSE)) |> 
  ggplot(aes(sch_ach, sch_growth, color = used_ly)) +
  geom_point() +
  geom_smooth()
