#' This script produces a 10 year forecast for STC (plus some others) apprenticeship completion using survival analysis.
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
library(fpp3)
library(patchwork)
library(conflicted)
conflicts_prefer(dplyr::filter)
#constants----------------------
largest_trades <- 20 #we consider STC plus some other large trades
fcast_start <- 2025 #this needs to be incremented
fcast_end <- fcast_start+9
fcast_range <- fcast_start:fcast_end
fcast_horizon <- 180 #longer than needed
font_size <- 10
#functions------------------------------
get_trade_counts <- function(the_trade){
  #' gets the ids of each person who completed for the trade, counts the number of trades completed, then tables counts.
  ids <- complete|>
    filter(trade_desc==the_trade)|>
    distinct(unique_key)|>
    pull(unique_key)

  filtered <- complete|>
    filter(unique_key %in% ids)|>
    group_by(unique_key)|>
    summarize(number_of_trades=n())|>
    tabyl(number_of_trades)
}

complete_wrapper <- function(surv_dat, at_risk) {
  #this function applies the joint probability of completion to the stream of at_risk (historical+forecast new_regs)
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

survfit_last8 <- function(tbbl) {
  survfit(Surv(time, completed) ~ 1, tbbl|>filter(era=="last 8 years")) #most recent data only
}
survfit_all <- function(tbbl) {
  survfit(Surv(time, completed) ~ 1, tbbl)
}
survfit_split <- function(tbbl) {
  survfit(Surv(time, completed) ~ era, tbbl)
}

get_joint <- function(mod_lst) {
  # calculate the joint probability of survival thus far and completion now: P(S and C)= P(S)*P(C|S)
  # Data doesn't make sense in some cases (for our purposes)... code below fixes.
  if(mod_lst$cumhaz[1]!=0 | mod_lst$surv[1]!=1){ #if survival curve doesn't start at surv=1...
    mod_lst$cumhaz=c(0, mod_lst$cumhaz) #at time =0 cumulative hazard should be 0
    mod_lst$surv=c(1, mod_lst$surv) #at time=0 the survival probability should be 1.
    mod_lst$time=c(0, mod_lst$time) #at time=0 time should be... 0
  }
  cumhaz <- c(mod_lst$cumhaz, NA_real_)
  surv <- c(1, mod_lst$surv) #pushes down surv by one row relative to cumhaz and time
  time <- c(mod_lst$time, NA_real_)

  tibble(
    haz_rate = c(0, diff(cumhaz)),
    surv = surv,
    time = time
  ) |>
    mutate(joint = haz_rate * surv)|>
    na.omit()
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

split_data <- function(mod_list){
  #takes survival model list, and creates a tsibble with the relevant info
  temp <- tibble(era=rep(names(mod_list[["strata"]]), times=mod_list[["strata"]]),
         surv=mod_list[["surv"]],
         time=mod_list[["time"]])|>
    as_tsibble(key=era, index=time)|>
    group_by_key() %>%
    fill_gaps() %>%
    tidyr::fill(surv, .direction = "down")
}

get_mean_delay <- function(tbbl){
  # conditional on completion, how long (on average) does it take to complete
  tbbl|>
    mutate(joint_sum_to_one=joint/sum(joint))|> #adjust probabilities for non-completions.
    summarize(mean_delay_year=sum(time*joint_sum_to_one)/12)|>
    pull(mean_delay_year)
}

# get the data, clean it up-----------------------------------

reg_and_complete <- read_excel(here("data", "Completion App Data.xlsx"),
                               sheet = "Sheet1",
                               na = "NULL") |>
  clean_names()|>
  mutate(registration_end_date=as.character(registration_end_date), #because we need to overwrite dates with NAs below (there is no NA_date_ in R)
         registration_end_date=if_else(registration_status_desc=="DEREG", NA_character_, registration_end_date), #DEREGs should be missing date of completion
         registration_end_date=ymd(registration_end_date))|> #convert it back to a date
  select(unique_key, registration_status_desc, start_date=registration_start_date, end_date=registration_end_date, trade_desc) |>
  mutate(
    start_date = tsibble::yearmonth(ymd(start_date)),
    end_date = tsibble::yearmonth(ymd(end_date)),
    last_observed = tsibble::yearmonth(max(start_date, na.rm = TRUE)), # not clear when observation ended... using this as proxy.
    completed = if_else(is.na(end_date), 0, 1),
    time = if_else(is.na(end_date), last_observed - start_date, end_date - start_date)
  )

#only look at the largest, stc and a couple additional trades-------------------------------------

largest <- reg_and_complete|>
  tabyl(trade_desc)|>
  slice_max(n, n=20)|>
  pull(trade_desc)

#additional trades that feed into the same NOCs that largest and STC trades feed into.
additional <- c("Lather (Interior Systems Mechanic) (Wall & Ceiling Installer)",
                                  "Drywall Finisher")

stc<- read_csv(here("out","trade_noc_mapping.csv"))|>
  filter(STC_Trades=="Y")|>
  pull(Trade)

keep_these_trades <- unique(c(largest, additional, stc))

reg_and_complete <-reg_and_complete|>
  filter(trade_desc %in% keep_these_trades,
         end_date > start_date | is.na(end_date)) # sanity check: cant end before you start

#the historical time series of completions-------------------------------
observed_complete <- reg_and_complete |>
  filter(!is.na(end_date))|>
  group_by(trade_desc, end_date) |>
  summarize(observed_complete = n()) |>
  as_tsibble(key=trade_desc, index = end_date)|>
  tsibble::fill_gaps(observed_complete=0, .full=TRUE)|> #make implicit missing explicit zeros
  as_tibble()|>
  arrange(trade_desc, end_date)
# do the survival analysis--------------------------
survival <- reg_and_complete |>
  mutate(era=case_when(start_date<yearmonth(ym(min(start_date))+years(8))~"first 8 years",
                          start_date>yearmonth(ym(last_observed)-years(8))~"last 8 years",
                          TRUE ~ "middle 8 years"))|>
  group_by(trade_desc) |>
  nest()|>
  mutate(
    km_model_last8 = map(data, survfit_last8),
    km_split_model = map(data, survfit_split),
    km_model_all_data = map(data, survfit_all),
    surv_dat = map(km_model_last8, get_joint),
    surv_dat_all = map(km_model_all_data, get_joint),
    split_data = map(km_split_model, split_data),
    `What is the mean duration of a successful apprenticeship` = map_dbl(surv_dat, get_mean_delay),
    `What is the probability of completion?` = map_dbl(surv_dat, function(x) 1-min(x$surv))
  )|>
  arrange(`What is the probability of completion?`)

#write key findings to disk---------------------------

survival|>
  select(trade_desc, split_data)|>
  mutate(split_data=map(split_data, tibble))|>
  unnest(split_data)|>#view()
  openxlsx::write.xlsx(here("out","survival_rates_by_trade_and_era.xlsx"))

survival|>
  select(trade_desc, surv_dat_all)|>
  unnest(surv_dat_all)|>
  select(trade_desc, surv, time)|>
  mutate(era="All 24 years")|>
  openxlsx::write.xlsx(here("out","survival_rates_by_trade.xlsx"))

survival|>
  select(trade_desc,
         `What is the mean duration of a successful apprenticeship`,
         `What is the probability of completion?`)|>#view()
  write_csv(here("out","trades_prob_complete_and_mean_duration.csv"))


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

#fit the models----------------------------------
ets_fit <- observed_at_risk |>
  model(ets_model = ETS(new_regs~trend("Ad")+error("A")))|>
  filter(!is_null_model(ets_model))

#forecast the models--------------------------------------
ets_fcast <- ets_fit |>
  forecast(h = fcast_horizon)

#bind the historical at risk with the forecast at risk------------------------------------
at_risk <- ets_fcast |>
  tibble()|>
  select(start_date, trade_desc, new_regs = .mean)|>
  group_by(trade_desc)|>
  mutate(mean_new_regs=mean(new_regs))|>
  mutate(new_regs=if_else(mean_new_regs<0, 0, new_regs))|> #if the mean of the forecast is negative, make 0
  select(-mean_new_regs)|>
  ungroup()|>
  bind_rows(observed_at_risk)|>
  filter(start_date<=yearmonth(paste(fcast_end,"/12")))|> #trims the ets_fcast down to LMO forecast horizon.
  nest(at_risk_data=c(start_date, new_regs))

# applies joint probabilities of "death" to historical+forecast at risk(i.e. forecasts AND backcasts)
f_and_b_cast <- survival |>
  semi_join(ets_fit|>tibble()|>select(trade_desc))|> #only the series we were actually able to fit models to
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

write_rds(forecasts, here("out", "forecasts.rds"))

actual_plus_forecast <- observed_complete|>
  semi_join(ets_fit|>tibble()|>select(trade_desc))|> #only the series we fit models to
  rename(value=observed_complete,
         date=end_date)|>
  bind_rows(forecasts)|>
  arrange(trade_desc, date)

actual_plus_forecast <- actual_plus_forecast|>
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
  select(trade_desc, surv_dat, km_model_last8, km_split_model, data)|>
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

#deflate to account for double/triple/quadruple counting (one person can do multiple trades, but can only work one job)

complete <- reg_and_complete|>
  filter(registration_status_desc=="PRGCMP")

deflation_factors <- tibble(trade_desc=survival$trade_desc)|>
  mutate(trade_counts=map(trade_desc, get_trade_counts))|>
  unnest(trade_counts)|>
  group_by(trade_desc)|>
  mutate(percent_over_number_of_trades=percent/number_of_trades)|>
  summarize(multiply_supply_by=sum(percent_over_number_of_trades))

deflated_completions <- full_join(by_trade_completions, deflation_factors)|>
  mutate(apprentice_completion_fcast=apprentice_completion_fcast*multiply_supply_by)|>
  select(trade_desc, year, apprentice_completion_fcast)

#start and end dates for shading plots--------------------------------
tibble(x0 = as.integer(ym(reg_and_complete$last_observed[1])),
       x1 = as.integer(ym(paste(fcast_end,"/12"))))|>
  write_rds(here("out","dates.rds"))

#compare with LMO demand----------------------------------------

mapping <- read_csv(here("out","trade_noc_mapping.csv"))|>
  select(-STC_Trades)|>
  rename(trade_desc=Trade)

by_noc_completions <- deflated_completions|>
  inner_join(mapping)|>
  group_by(year, NOC_Code_2021, NOC_2021)|>
  summarize(apprentice_completions=sum(apprentice_completion_fcast))

new_reg_forecast <- at_risk|>
  unnest(at_risk_data)|>
  full_join(deflation_factors)|> #deflate the new reg forecast by factor to compensate for double/triple/quad counting
  mutate(new_regs=new_regs*multiply_supply_by)|>
  select(-multiply_supply_by)|>
  inner_join(mapping)|>
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
  na.omit()|> #keeps only the forecast part
  pivot_longer(cols=c(demand, supply), names_to = "series", values_to = "value")|>
  write_rds(here("out","new_regs_vs_demand.rds"))


# are there congestion effects?

urate <- read_csv(here("data","urate.csv"), skip = 14, n_max = 24)|>
  mutate(`mean unemployment rate subsequent 4 years`=zoo::rollmean(urate, k=4, align="left", na.pad = TRUE))|>#mean u rate over 4 years following registration
  select(-urate)

prop_complete  <- reg_and_complete|>
  filter(ym(start_date)<floor_date(today()-years(5), unit = "year"))|>
  group_by(trade_desc, year=year(floor_date(ym(start_date), unit="year")))|>
  summarize(`cohort size`=n(),
            prop_complete=sum(completed)/n()
  )

mean_delay <- reg_and_complete|>
  filter(completed==1)|>
  group_by(trade_desc, year=year(floor_date(ym(start_date), unit="year")))|>
  summarise(mean_delay=mean(time, na.rm = TRUE))

full_join(prop_complete, mean_delay)|>
  left_join(urate)|>
  na.omit()|>
  write_rds(here("out", "congestion.rds"))










