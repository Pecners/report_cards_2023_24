library(tidyverse)
library(wisconsink12)

d <- read_csv("../forward_exam_2020_21/data/mke_school_prof_rates_2022-23.csv")

ty <- make_mke_rc() |> 
  filter(school_year == "2022-23") |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         school_enrollment,
         overall_rating,
         overall_score,
         starts_with("per_")) |> 
  left_join(x |> 
              select(dpi_true_id,
                     ELA, Mathematics)) |> 
  select(-dpi_true_id)

names(ty) <- c(
  "School Year",
  "School Name",
  "Sector",
  "School Enrollment (2022-23)",
  "Overall Rating",
  "Overall Score",
  "% American Indian",
  "% Asian",
  "% Black or African American",
  "% Hispanic/Latino",
  "% Native Hawaiian or Other Pacific Islander",
  "% White",
  "% Two or More Races",
  "% Students with Disabilities",
  "% Economically Disadvantaged",
  "% Limited English Proficiency",
  "% Choice",
  "% Open Enrollment",
  "ELA % Proficient",
  "Math % Proficient"
)

write_csv(ty, "data/Milwaukee School Report Cards 2022-23.csv")

five <- ty |> 
  filter(overall_rating == "Significantly Exceeds Expectations")

make_mke_rc() |> 
  filter(school_year > "2020-21" & 
           overall_rating == "Significantly Exceeds Expectations") |> 
  group_by(school_year) |> 
  count()
