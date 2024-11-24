library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)
library(sf)

used_ly <- read_rds("data/used_ly_ach.rda")

# raw change in oa score

this_rc <- make_mke_rc() |> 
  filter(school_year > "2021-22") |> 
  mutate(used_ly = ifelse(dpi_true_id %in% used_ly, TRUE, FALSE))

school_changes <- this_rc |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         overall_score,
         used_ly) |> 
  pivot_wider(names_from = school_year, 
              values_from = overall_score) |> 
  mutate(diff = `2023-24` - `2022-23`)

school_changes |> 
  ggplot(aes(diff)) +
  geom_histogram()


# change in rating category

cats <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations",
  "Meets Expectations",
  "Exceeds Expectations",
  "Significantly Exceeds Expectations"
)

this_rc |> 
  mutate(overall_rating = factor(overall_rating, levels = cats) |> 
           as.numeric()) |> 
  filter(!is.na(overall_rating)) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         overall_rating) |> 
  pivot_wider(names_from = school_year, values_from = overall_rating) |> 
  mutate(diff = `2023-24` - `2022-23`) |> 
  # View()
  group_by(`2022-23`, diff) |> 
  count() |> View()

changes <- this_rc |> 
  mutate(overall_rating = factor(overall_rating, levels = cats) |> 
           as.numeric()) |> 
  filter(!is.na(overall_rating)) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         overall_rating) |> 
  pivot_wider(names_from = school_year, values_from = overall_rating) |> 
  mutate(diff = `2023-24` - `2022-23`) |> 
  arrange(diff, `2023-24`) |> 
  mutate(lows = ifelse(`2023-24` == 1 & diff < 0, TRUE, FALSE),
         lows = ifelse(is.na(lows), FALSE, lows))
# View()
# group_by(`2022-23`, diff) |> 
# count() |> View()

changes |> 
  left_join(geocodes) |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  ggplot() +
  geom_sf(aes(color = lows))

# calculate enrollment demos

changes |> 
  filter(lows) |> 
  left_join(make_mke_rc() |> 
              filter(school_year == "2023-24")) |> View()
summarise(across(starts_with("per_"), 
                 function(x) weighted.mean(x, w = school_enrollment, na.rm = TRUE)))

