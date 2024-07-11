#' This script produces a 5 year forecast for STC apprenticeship completion using survival analysis.
#' Using the terminology of survival analysis, an individual who registers for apprenticeship is "at risk"
#' of completing their apprenticeship a.k.a. "Death".
#'
#' The first component is an ETS forecast of the "at risk" population based on the historic data for the
#' arrival of "at risk" individuals.
#'
#' The second component is based on survival analysis of the historic data, yielding
#' 1) the probability of survival given how much time has elapsed since registering and
#' 2) the hazard rate: the probability of dying now conditional on survival thus far.
#'
#' The joint probability of "death" is the probability of survival thus far times the hazard rate.
#'
#' These joint probabilities are applied to the "at risk" population (both historic and forecast) yielding the
#' apprenticeship completion forecast.

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
fcast_horizon=120
font_size=18
fcast_range=2023:2033

complete_wrapper <- function(surv_dat, at_risk) {#at_risk is the historical+ets forecast of new registrations tibble
  complete <- function(start_date, new_regs) {
    tibble(
      complete_date = yearmonth(ym(start_date) + months(surv_dat$time)),
      expected_complete = new_regs * surv_dat$joint
    )
  }
  at_risk |>
    mutate(data = map2(start_date, new_regs, complete))
}

tidy_up <- function(tbbl) {
  start <- yearmonth(ym(min(reg_and_complete$start_date)) + months(fcast_horizon))
  end <- yearmonth(ym(reg_and_complete$last_observed[1]) + months(fcast_horizon))
  tbbl |>
    select(data) |>
    unnest(data) |>
    group_by(complete_date) |>
    summarize(expected_complete = sum(expected_complete)) |>
    filter(complete_date > start & complete_date < end)
}

survfit_constant <- function(tbbl) {
  survfit(Surv(time, completed) ~ 1, tbbl)
}
survfit_split <- function(tbbl) {
  survfit(Surv(time, completed) ~ era, tbbl)
}

get_joint <- function(mod_lst) {
  tibble(
    haz_rate = c(diff(mod_lst$cumhaz), 0),
    surv = mod_lst$surv,
    time = mod_lst$time
  ) |>
    mutate(joint = haz_rate * surv)
}

fcast_plot <- function(tbbl, series, ylab){
  tbbl|>
    mutate(date=ym(date))|>
    ggplot(aes(date, value))+
    geom_line() +
    scale_y_continuous(trans="log10", labels = scales::comma)+
    annotate(
      geom = "rect",
      xmin = ym(reg_and_complete$last_observed[1]),
      xmax = ym(max(f_and_b_cast$complete_date)+month(1)),
      ymin = 0,
      ymax = +Inf,
      alpha = 0.4
    ) +
    labs(x = NULL,
         y = ylab,
         title=series)+
    theme(text=element_text(size=font_size))
}
obs_vs_back_plot <- function(tbbl, name){
  tbbl|>
    ggplot(aes(date, value, colour=series))+
    geom_line()+
    scale_y_continuous(trans="log10", labels = scales::comma)+
    labs(title=name,
         y="Annual Apprenticeship Completions",
         x=NULL)+
    theme(text=element_text(size=font_size))
}

# get the data, clean it up-----------------------------------

stc<- read_csv(here("out","stc_trade_noc_mapping.csv"))|>
  select(trade_desc=Trade)

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
  semi_join(stc)|>
  filter(end_date > start_date | is.na(end_date)) # sanity check: cant end before you start
#the historical time series of completions-------------------------------
observed_complete <- reg_and_complete |>
  filter(!is.na(end_date)) |>
  group_by(trade_desc, end_date) |>
  summarize(observed_complete = n()) |>
  arrange(trade_desc, end_date)
# do the survival analysis--------------------------
survival <- reg_and_complete |>
  mutate(era=case_when(start_date<ym(min(start_date))+years(8)~"early",
                          start_date>ym(last_observed)-years(8)~"late",
                          TRUE ~ "middle"))|>
  group_by(trade_desc) |>
  nest()|>
  mutate(
    km_model = map(data, survfit_constant),
    km_split_model = map(data, survfit_split),
    surv_dat = map(km_model, get_joint),
    km_prob_complete = map_dbl(surv_dat, function(x) 1-min(x$surv))
  )|>
  arrange(km_prob_complete)


# the time series of arrivals (new registrants at risk of completion)--------------------------------
observed_at_risk <- reg_and_complete |>
  group_by(start_date, trade_desc) |>
  summarize(new_regs = n()) |>
  as_tsibble(key = trade_desc, index = start_date) |>
  fill_gaps(.full=TRUE)|>
  mutate(new_regs = if_else(is.na(new_regs), 0, new_regs))|>
  tibble()|>
  group_by(trade_desc)|>
  mutate(max_start=max(start_date))|>
#  filter(ym(max_start) > today()-years(1))|> #at least one person has registered in last year
  select(-max_start)|>
  as_tsibble(key = trade_desc, index = start_date)

ets_fit <- observed_at_risk |>
  model(ets_model = ETS(sqrt(new_regs)))|> #sqrt transform keeps forecast from going negative
  filter(!is_null_model(ets_model))

ets_fcast <- ets_fit |>
  forecast(h = fcast_horizon)

#bind the historical at risk with the forecast at risk------------------------------------
at_risk <- ets_fcast |>
  tibble()|>
  select(start_date, trade_desc, new_regs = .mean)|>
  bind_rows(observed_at_risk) |>
  nest(at_risk_data=c(start_date, new_regs))

# all data applies joint probabilities of "death" to historical+forecast at risk(i.e. forecasts AND backcasts)
f_and_b_cast <- survival |>
  semi_join(ets_fit|>tibble()|>select(trade_desc))|> #only the series we fit models to
  select(trade_desc, surv_dat)|>
  full_join(at_risk)|>
  mutate(complete = map2(surv_dat, at_risk_data, complete_wrapper))|>
  select(trade_desc, complete) |> #complete is itself a nested tibble (tibble f_and_b_cast is double nested.)
  mutate(complete = map(complete, tidy_up)) |> #unnests the lowest level and aggregates by end_date
  unnest(complete)

#some plots-----------------------------------

forecasts <- f_and_b_cast|>
  filter(complete_date>max(reg_and_complete$start_date))|>
  rename(value=expected_complete,
         date=complete_date)

actual_plus_forecast <- observed_complete|>
  semi_join(ets_fit|>tibble()|>select(trade_desc))|> #only the series we fit models to
  rename(value=observed_complete,
         date=end_date)|>
  bind_rows(forecasts)|>
  arrange(trade_desc, date)|>
  group_by(trade_desc)|>
  nest()|>
  mutate(plot=map2(data, trade_desc, fcast_plot, "Monthly Apprentice Completions"))

#plot using ets_fcast and at risk (bind them together nest them, then map plotting function)---------------

at_risk_plus_forecast <- ets_fcast |>
  as_tibble()|>
  select(trade_desc, start_date, .mean)|>
  rename(new_regs=.mean)|>
  bind_rows(at_risk|>unnest(at_risk_data))|>
  rename(date=start_date,
         value=new_regs)|>
  group_by(trade_desc)|>
  nest()|>
  mutate(plot=map2(data, trade_desc, fcast_plot, "Monthly New Registrations"))

#backcasts vs observed complete----------------------------

observed_vs_backcast <- observed_complete|>
  rename(date=end_date)|>
  inner_join(f_and_b_cast|>rename(date=complete_date))|>
  mutate(date=year(date))|>
  filter(date<max(date))|> #get rid of incomplete last year
  group_by(trade_desc, date)|>
  summarize(observed=sum(observed_complete),
            backcast=sum(expected_complete))|>
  pivot_longer(cols=c("observed","backcast"), names_to = "series", values_to = "value")|>
  group_by(trade_desc)|>
  nest()|>
  mutate(plot=map2(data, trade_desc, obs_vs_back_plot))

#write data to disk---------------------------

survival|>
  select(trade_desc, km_split_model, data)|>
  write_rds(here("out","survival.rds"))

observed_vs_backcast|>
  select(-data)|>
  write_rds(here("out","observed_vs_backcast.rds"))

at_risk_plus_forecast|>
  select(-data)|>
  write_rds(here("out","at_risk_plus_forecast.rds"))

actual_plus_forecast|>
  select(-data)|>
  write_rds(here("out","actual_plus_forecast.rds"))

actual_plus_forecast|>
  select(-plot)|>
  unnest(data)|>
  mutate(year=year(date))|>
  group_by(trade_desc, year)|>
  summarize(apprentice_completion_fcast=round(sum(value)))|>
  filter(year %in% fcast_range)|>
  write_csv(here("out","annual_apprentice_completion_forecast.csv"))
