#needed to run the 01_analysis.R script twice (for the two different LMOs)  Save data between runs (line 41ish)

library(tidyverse)
library(here)
completions_vs_demand <- read_rds(here("out", "completions_vs_demand.rds"))|>
  filter(NOC_2021 %in% c("Plasterers, drywall installers and finishers and lathers",
                         "Carpenters",
                         "Painters and decorators (except interior decorators)",
                         "Plumbers",
                         "Electricians (except industrial and power system)",
                         "Industrial electricians"
                         )
         )|>
  mutate(NOC_2021=if_else(NOC_2021 %in% c("Electricians (except industrial and power system)",
                                           "Industrial electricians"),
                          "Aggregate Electricians",
                          NOC_2021))|>
  group_by(year, NOC_2021, series)|>
  summarize(value=sum(value))|>
  pivot_wider(names_from = series, values_from = value)|>
  arrange(NOC_2021, year)

new_regs_vs_demand <- read_rds(here("out", "new_regs_vs_demand.rds"))|>
  filter(NOC_2021 %in% c("Plasterers, drywall installers and finishers and lathers",
                         "Carpenters",
                         "Painters and decorators (except interior decorators)",
                         "Plumbers",
                         "Electricians (except industrial and power system)",
                         "Industrial electricians"
  )
  )|>
  mutate(NOC_2021=if_else(NOC_2021 %in% c("Electricians (except industrial and power system)",
                                          "Industrial electricians"),
                          "Aggregate Electricians",
                          NOC_2021))|>
  group_by(year, NOC_2021, series)|>
  summarize(value=sum(value))|>
  pivot_wider(names_from = series, values_from = value)|>
  arrange(NOC_2021, year)

#save after running each year's version
#sheets_2023 <- list("apprentice D&S 2023"=new_regs_vs_demand, "journeyperson D&S 2023"=completions_vs_demand)
#sheets_2024 <- list("apprentice D&S 2024"=new_regs_vs_demand, "journeyperson D&S 2024"=completions_vs_demand)

sheets=c(sheets_2023,sheets_2024)

openxlsx::write.xlsx(sheets, here("out","construction_apprentice_journeyperson_forecasts.xlsx"))



