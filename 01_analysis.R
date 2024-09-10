#' This script produces a 5 year forecast for STC (plus some others) apprenticeship completion using survival analysis.
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
largest_trades <- 20
fcast_start <- 2025
fcast_end <- fcast_start+9
fcast_range <- fcast_start:fcast_end
fcast_horizon <- 180 #longer than needed
font_size <- 15

get_cor <- function(tbbl, var){
  with(tbbl, cor(cohort_size, get(var), use = "pairwise.complete.obs"))
}

get_stats <- function(tbbl){
  size_and_prop <- tbbl|>
    group_by(start_date)|>
    summarize(cohort_size=n(),
              prop_complete=sum(completed)/n()
    )
  delay <- tbbl|>
    filter(completed==1)|>
    group_by(start_date)|>
    summarise(mean_delay=mean(time, na.rm = TRUE))

  full_join(size_and_prop, delay)
}


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
  end <- yearmonth(paste(fcast_end,"/12"))
  tbbl |>
    select(data) |>
    unnest(data) |>
    group_by(complete_date) |>
    summarize(expected_complete = sum(expected_complete)) |>
    filter(complete_date > start & complete_date <= end)
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
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL,
         y = ylab,
         title=series)+
    theme(text=element_text(size=font_size))
}
obs_vs_back_plot <- function(tbbl, name){
  tbbl|>
    ggplot(aes(date, value, color=series))+
    geom_line()+
    scale_y_continuous(labels = scales::comma)+
    labs(title=name,
         y="Annual Apprenticeship Completions",
         x=NULL)+
    theme(text=element_text(size=font_size))
}

get_closest <- function(desired_value, actual_values){
  keep <- which.min(abs(desired_value-actual_values))
  actual_values[keep]
}

split_data <- function(mod_list){
  temp <- tibble(era=rep(names(mod_list[["strata"]]), times=mod_list[["strata"]]),
         surv=mod_list[["surv"]],
         time=mod_list[["time"]])|>
    as_tsibble(key=era, index=time)|>
    group_by_key() %>%
    fill_gaps() %>%
    tidyr::fill(surv, .direction = "down")
}

get_yearly <- function(tbbl){
  tbbl|>
    filter(time %in% seq(12,240, 12))|>
    mutate(years_since_registration=time/12,
           proportion_complete=1-surv)|>
    as_tibble()|>
    select(era, years_since_registration, proportion_complete)
}
get_median <- function(tbbl){
  tbbl|>
    as_tibble()|>
    group_by(era)|>
    mutate(distance=abs(surv-.5))|>
    slice_min(distance, n=1, with_ties = FALSE)|>
    mutate(median_time_to_complete_in_months=if_else(distance>.1, NA_real_, time))|>
    select(era, median_time_to_complete_in_months)
}


# get the data, clean it up-----------------------------------

stc<- read_csv(here("out","trade_noc_mapping.csv"))|>
  filter(STC_Trades=="Y")|>
  select(trade_desc=Trade)

reg_and_complete <- read_excel(here("data", "Completion App Data.xlsx"),
                               sheet = "Sheet1",
                               na = "NULL") |>
  clean_names()|>
  mutate(registration_end_date=as.character(registration_end_date),
         registration_end_date=if_else(registration_status_desc=="DEREG", NA_character_, registration_end_date), #DEREGs should be missing date of completion
         registration_end_date=ymd(registration_end_date))|>
  select(start_date=registration_start_date, end_date=registration_end_date, trade_desc) |>
  mutate(
    start_date = tsibble::yearmonth(lubridate::ymd(start_date)),
    end_date = tsibble::yearmonth(lubridate::ymd(end_date)),
    last_observed = tsibble::yearmonth(max(start_date, na.rm = TRUE)), # not clear when observation ended... using this as proxy.
    completed = if_else(is.na(end_date), 0, 1),
    time = if_else(is.na(end_date), last_observed - start_date, end_date - start_date)
  )


largest <- tibble(trade_desc=table(reg_and_complete$trade_desc)|>sort()|>tail(n=largest_trades)|>names())
additional <- tibble(trade_desc=c("Lather (Interior Systems Mechanic) (Wall & Ceiling Installer)",
                                  "Drywall Finisher"))#,
                     #              "Cook",
                     #              "Piledriver And Bridgeworker",
                     #              "Asphalt Paving/Laydown Technician",
                     #              "Residential Steep Roofer")
                     # )
                     #these additional trades feed into occupations shared by the largest and stc trades... but very uncommon.
stc_plus <- full_join(stc, largest)|>
  full_join(additional)

reg_and_complete <-reg_and_complete|>
  semi_join(stc_plus)|>
  filter(end_date > start_date | is.na(end_date)) # sanity check: cant end before you start

# are there congestion effects?

reg_and_complete|>
  filter(start_date<today()-years(5))|> #gives a reasonable amount of time to complete.
  group_by(trade_desc)|>
  nest()|>
  mutate(stats=map(data, get_stats))|>
  mutate(completion_cor=map_dbl(stats, get_cor, "prop_complete"),
         delay_cor=map_dbl(stats, get_cor, "mean_delay")
         )|>
  select(-data, -stats)|>
  write_rds(here("out", "congestion.rds"))

#the historical time series of completions-------------------------------
observed_complete <- reg_and_complete |>
  filter(!is.na(end_date)) |>
  group_by(trade_desc, end_date) |>
  summarize(observed_complete = n()) |>
  arrange(trade_desc, end_date)
# do the survival analysis--------------------------
survival <- reg_and_complete |>
  mutate(era=case_when(start_date<ym(min(start_date))+years(8)~"first 8 years",
                          start_date>ym(last_observed)-years(8)~"last 8 years",
                          TRUE ~ "middle 8 years"))|>
  group_by(trade_desc) |>
  nest()|>
  mutate(
    km_model = map(data, survfit_constant),
    km_split_model = map(data, survfit_split),
    surv_dat = map(km_model, get_joint),
    split_data = map(km_split_model, split_data),
    yearly_split = map(split_data, get_yearly),
    median_delay = map(split_data, get_median),
    km_prob_complete = map_dbl(surv_dat, function(x) 1-min(x$surv))
  )|>
  arrange(km_prob_complete)

#write some key findings to disk-------------------------------
survival|>
  select(trade_desc, yearly_split)|>
  unnest(yearly_split)|>
  write_csv(here("out","apprentice_completion_rates_annual.csv"))

survival|>
  select(trade_desc, median_delay)|>
  unnest(median_delay)|>
  write_csv(here("out","median_time_to_complete_in_months.csv"))

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
  select(-max_start)|>
  as_tsibble(key = trade_desc, index = start_date)

ets_fit <- observed_at_risk |>
  model(ets_model = ETS(new_regs~trend("Ad")+error("A")))|>
  filter(!is_null_model(ets_model))

ets_fcast <- ets_fit |>
  forecast(h = fcast_horizon)

#bind the historical at risk with the forecast at risk------------------------------------
at_risk <- ets_fcast |>
  tibble()|>
  select(start_date, trade_desc, new_regs = .mean)|>
  bind_rows(observed_at_risk)|>
  filter(start_date<=yearmonth(paste(fcast_end,"/12")))|>
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

#plot at risk arrival data (observed + forecast)

 at_risk_plots <-  at_risk|>
  unnest(at_risk_data)|>
  rename(date=start_date,
         value=new_regs)|>
  nest(at_risk_data=c("date", "value"))|>
  mutate(plot=map2(at_risk_data, trade_desc, fcast_plot, "Monthly New Registrations"))

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
  select(trade_desc, surv_dat, km_model, km_split_model, data)|>
  write_rds(here("out","survival.rds"))

observed_vs_backcast|>
  select(-data)|>
  write_rds(here("out","observed_vs_backcast.rds"))

at_risk_plots|>
  select(-at_risk_data)|>
  write_rds(here("out","at_risk_plots.rds"))

at_risk|>
  write_rds(here("out","at_risk.rds"))

actual_plus_forecast|>
  select(-data)|>
  write_rds(here("out","actual_plus_forecast.rds"))

by_trade_completions <- actual_plus_forecast|>
  select(-plot)|>
  unnest(data)|>
  mutate(year=year(date))|>
  group_by(trade_desc, year)|>
  summarize(apprentice_completion_fcast=round(sum(value)))|>
  filter(year %in% fcast_range)

write_csv(by_trade_completions, here("out","annual_apprentice_completion_forecast.csv"))

#start and end dates for shading plots--------------------------------
tibble(x0 = as.integer(ym(reg_and_complete$last_observed[1])),
       x1 = as.integer(ym(paste(fcast_end,"/12"))))|>
  write_rds(here("out","dates.rds"))

#compare with LMO demand----------------------------------------

mapping <- read_csv(here("out","trade_noc_mapping.csv"))|>
  select(-STC_Trades)|>
  rename(trade_desc=Trade)

by_noc_completions <- by_trade_completions|>
  inner_join(mapping)|>
  group_by(year, NOC_Code_2021, NOC_2021)|>
  summarize(apprentice_completions=sum(apprentice_completion_fcast))

new_reg_forecast <- at_risk|>
  inner_join(mapping)|>
  unnest(at_risk_data)|>
  mutate(start_date=year(ym(start_date)))|>
  group_by(year=start_date, NOC_Code_2021, NOC_2021)|>
  summarize(apprentice_new_reg=sum(new_regs))

#' demand for journeypersons is assumed to be twice replacement demand plus 1/3 of expansion demand.
#' Re twice, for every retirement or death we assume there is another journeyperson that changes occupations.
#' Re 1/3, want a 1:2 relationship between journeyperson and apprentice.
#' Apprentice demand is double journeyperson demand (1:2 relationship)

demand <- read_excel(here("data","demand2024.xlsx"), skip = 3)|>
  filter(Variable %in% c("Expansion Demand","Replacement Demand"))|>
  select(NOC_Code_2021=NOC, NOC_2021=Description, Variable, starts_with("2"))|>
  pivot_longer(cols = starts_with("2"), names_to = "year", values_to = "value")|>
  mutate(NOC_Code_2021=as.numeric(str_remove_all(NOC_Code_2021, "#")),
         year=as.numeric(year))|>
  pivot_wider(names_from = Variable, values_from = value)|>
  semi_join(mapping)|>
  mutate(journeyperson_demand=2*`Replacement Demand`+1/3*`Expansion Demand`,
         apprentice_demand=2*journeyperson_demand)|>
  select(year, contains("NOC"), contains("_demand"))

inner_join(by_noc_completions, demand)|>
  select(-apprentice_demand)|>
  rename(demand=journeyperson_demand,
         supply=apprentice_completions)|>
  pivot_longer(cols=c(demand, supply), names_to = "series", values_to = "value")|>
  write_rds(here("out","completions_vs_demand.rds"))

#for apprentices-------------------------

full_join(new_reg_forecast, demand)|>
  select(-journeyperson_demand)|>
  rename(demand=apprentice_demand,
         supply=apprentice_new_reg)|>
  na.omit()|>
  pivot_longer(cols=c(demand, supply), names_to = "series", values_to = "value")|>
  write_rds(here("out","new_regs_vs_demand.rds"))













