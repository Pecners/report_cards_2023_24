done <- rated_schools |> 
  filter(school_year == "2021-22" & overall_score >= 83.0) |> 
  select(school_name, 
         accurate_agency_type, 
         overall_score,
         per_ed,
         per_b_aa,
         per_hisp_lat) |> 
  mutate_at(c("per_ed", "per_b_aa", "per_hisp_lat"), label_percent(.1)) |> 
  arrange(desc(overall_score)) |> 
  transmute("School" = school_name,
            Sector = ifelse(accurate_agency_type == "Private",
                            "Private Schools",
                            "Milwaukee Public Schools"),
            "Overall Score" = overall_score,
            "% Ec. Disadvantaged" = per_ed,
            "% Black" = per_b_aa,
            "% Hispanic" = per_hisp_lat)

write_csv(done, "data/five_stars.csv")
