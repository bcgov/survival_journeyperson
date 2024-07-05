library(tidyverse)
library(survival)
library(ggfortify)
library(janitor)
library(readxl)
library(here)
library(lubridate)
library(fpp3)
library(patchwork)
library(conflicted)
conflicts_prefer(dplyr::filter)
fcast_horizon=60

# get the data, clean it up-----------------------------------
reg_and_complete <- read_excel(here("data", "Completion App Data.xlsx"),
                               sheet = "Sheet1",
                               na = "NULL") |>
  clean_names()|>
  select(start_date=registration_start_date, end_date=registration_end_date, trade_desc) |>
  mutate(
    start_date = tsibble::yearmonth(lubridate::ymd(start_date)),
    end_date = tsibble::yearmonth(lubridate::ymd(end_date)),
    last_observed = tsibble::yearmonth(max(start_date, na.rm = TRUE)), # not clear when observation ended... using this as proxy.
    completed = if_else(is.na(end_date), 0, 1),
    time = if_else(is.na(end_date), last_observed - start_date, end_date - start_date)
  ) |>
  filter(end_date > start_date | is.na(end_date)) # sanity check: cant end before you start

monthly_completions <- reg_and_complete|>
  group_by(end_date, trade_desc)|>
  summarise(completions=n())|>
  filter(!is.na(end_date))|>
  group_by(trade_desc)|>
  mutate(max_end=max(end_date))|>
  filter(max_end>tsibble::yearmonth(today()-months(6)))|>
  select(-max_end)|>
  as_tsibble(key = trade_desc, index=end_date)|>
  tsibble::fill_gaps(completions=0, .full=TRUE)|>
  as_tibble()|>
  group_by(trade_desc)|>
  mutate(`12 month rolling mean`=zoo::rollmean(completions, k=12, align = "right", na.pad = TRUE))|>
  as_tsibble(key = trade_desc, index=end_date)|>
  na.omit()

ets_fit <- monthly_completions|>
  model(ETS(`12 month rolling mean`~ error("A")+trend("Ad")+season("N")))

make_plot <- function(tbbl, trade){
  plt <- tbbl|>
    ggplot(aes(end_date, completions, colour=series, alpha=alpha))+
    geom_line()+
    labs(title=trade)
  plotly::ggplotly(plt)
}

all_data <- ets_fit |>
  forecast(h = fcast_horizon)|>
  as_tibble()|>
  select(trade_desc, end_date, .mean)|>
  rename(forecast=.mean)|>
  full_join(monthly_completions)|>
  pivot_longer(cols=-c(trade_desc, end_date), names_to = "series", values_to="completions")|>
  mutate(alpha=if_else(series=="completions",.5, 1))|>
  group_by(trade_desc)|>
  nest()|>
  mutate(plot=map2(data, trade_desc, make_plot))




