#@iPCQ
#All costprices following the costing manual

func_iPCQ <- function(dat, reference_year, first_measurement, yeardays = 365.25){

#Import required packages
  require("dplyr")
  require("cbsodataR")
  require("tidyverse")
  require("here")


#Import datafile reference prices

  df_ref_prices <- data.frame(openxlsx::read.xlsx(xlsxFile = here("data/Referentieprijzen hoofdstuk 4.xlsx"), sheet = "tab_iPCQ"))



#Download inflation index

  cbs_inflation <-  cbsodataR::cbs_get_data("83131ned") %>%
    cbsodataR::cbs_add_date_column() %>%
    cbsodataR::cbs_add_label_columns() %>%
    filter(Perioden_freq == "Y",
           Bestedingscategorieen_label == "000000 Alle bestedingen") %>%
    select(Perioden_label, CPI_1) %>%
    mutate(Perioden_label = as.numeric(as.character(Perioden_label)))

#Inflation index 2022

  cbs_inflation_2022 <- cbs_inflation %>%
    filter(Perioden_label == 2022) %>%
    pull(CPI_1)

#Inflation index new

  cbs_inflation_new <- cbs_inflation %>%
    filter(Perioden_label == referentiejaar) %>%
    pull(CPI_1)

#Mutate inflation index across reference price

  df_ref_prices <- df_ref_prices %>%
    mutate(Referentieprijs = (((cbs_inflation_new - cbs_inflation_2022) / cbs_inflation_2022) + 1) * Referentieprijs)


#Calculate friction period

  cbs_friction_period_days <- cbsodataR::cbs_get_data("80472NED") %>%
    cbsodataR::cbs_add_label_columns() %>%
    cbsodataR::cbs_add_date_column() %>%
    filter(Perioden_freq == "Y",
           Bedrijfskenmerken %in% c("T001081")) %>%
    mutate(Year = lubridate::year(Perioden_Date)) %>%
    mutate(Friction_period_days  = yeardays / (VervuldeVacatures_3 / OpenstaandeVacatures_1) + 4 * 7) %>%
    select(Year, Friction_period_days) %>%
    filter(Year == 2022) %>%
    pull(Friction_period_days)

  cbs_friction_period_weeks <- cbs_friction_period_days / 7

#Calculations in case of prior measurement

  if(first_measurement == F){

#Names of columns that have to be included in data file (dat)

    col_names_first_measurement_F <- c("date_of_prior_measurement",
                                       "date_of_this_measurement",
                                       "hours_work_week",
                                       "days_work_week",
                                       "sick_longer_than_4_weeks",
                                       "days_sick",
                                       "date_start_sickness",
                                       "days_suffering_from_problems",
                                       "rate_of_work",
                                       "days_less_unpaid_work",
                                       "average_hours_unpaid_work")

#Check for the presence of required columns in the input dataset

    if(length(setdiff(col_names_first_measurement_F, names(dat))) > 0) stop(cat(
      "All iPCQ columns need to be present in dat. The following are missing:",
      setdiff(col_names, names(dat))))

#Calculate difference in days between date_of_this_measurement and date_of_prior_measurement

    diff_this_prior_days <- as.numeric(difftime(dat$date_of_this_measurement, dat$date_of_prior_measurement, units = "days"))
    diff_this_prior_weeks <- as.numeric(difftime(dat$date_of_this_measurement, dat$date_of_prior_measurement, units = "weeks"))
    diff_this_prior_consider_recall <- diff_this_prior_weeks / 4


#Calculate difference between date start of sickness and date of this measurement

    diff_this_start_days <- as.numeric(difftime(dat$date_of_this_measurement, dat$date_start_sickness, units = "days"))
    diff_this_start_weeks <- as.numeric(difftime(dat$date_of_this_measurement, dat$date_start_sickness, units = "weeks"))

#Calculate difference between date start of sickness and date of prior measurement

    diff_prior_start_days <- as.numeric(difftime(dat$date_of_prior_measurement, dat$date_start_sickness, units = "days"))
    diff_prior_start_weeks <- as.numeric(difftime(dat$date_of_prior_measurement, dat$date_start_sickness, units = "weeks"))

#Calculate  hours per day

    hours_per_day <- dat$hours_work_week / dat$days_work_week

#Calculating Absenteeism
#Short absenteeism

    if(dat$sick_longer_than_4_weeks == 0){

      dat$abs_short = hours_per_day * days_sick * diff_this_prior_consider_recall * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende "]

#Long absenteeism
#Situation 1: Duration of absence is shorter than the friction period

    }else{
      if(cbs_friction_period_days >= diff_this_start_days){

#Situation 1A: Difftime start sickness-this measurement => difftime prior measurement - this measurement

         if(diff_this_start_days >= diff_this_prior_days){
          dat$abs_long <- dat$hours_work_week * diff_this_prior_weeks * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende"]

#Situation 1B: Difftime start sickness-this measurement < difftime prior measuremnt - this measurement

        }else{
          dat$abs_long <- dat$hours_work_week * diff_this_start_weeks * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende"]
        }

#Sitation 2: Duration of absence is longer than the friction period

      }else{

#Situation 2A: Duration of absence is longer than the friction period and the whole friction period is before the prior measurement

      if(diff_prior_start_weeks >= cbs_friction_period_weeks){
      dat$abs_long <- 0

#Situation 2B: Duration of absence is longer than the friction period and the whole friction period is after the prior measurement

      }else if(diff_this_prior_weeks >= cbs_friction_period_weeks){
        dat$abs_long <- cbs_friction_period_weeks * dat$hours_work_week * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende"]

#Situation 2C: Duration of absence is longer than the friction period, and the fricition is partly after the prior measurement

      }else{
        dat$abs_long <- (cbs_friction_period_weeks - diff_prior_start_weeks) * dat$hours_work_week * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende"]
      }
      }
    }

#Presenteeism

    dat$presenteeism <-  dat$days_suffering_from_problems * (1 - (rate_of_work/10)) * hours_per_day * diff_this_prior_consider_recall * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende"]


#Unpaid work

    dat$unpaid_work <- dat$days_less_unpaid_work * dat$average_hours_unpaid_work * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Vervangingskosten per uur"] * diff_this_prior_consider_recall


#Calculations in case of no prior measurement
#Names of columns that have to be included in data file (dat)

  }else{
    col_names_first_measurement_T <- c("recall_period_in_weeks",
                                       "date_of_this_measurement",
                                       "hours_work_week",
                                       "days_work_week",
                                       "sick_longer_than_4_weeks",
                                       "days_sick",
                                       "date_start_sickness",
                                       "days_suffering_from_problems",
                                       "rate_of_work",
                                       "days_less_unpaid_work",
                                       "average_hours_unpaid work")

#Check for the presence of required columns in the input dataset

    if(length(setdiff(col_names_first_measurement_T, names(dat))) > 0) stop(cat(
      "All iPCQ columns need to be present in dat. The following are missing:",
      setdiff(col_names, names(dat))))

#Calculate difference between date start of sickness and date of this measurement

    diff_this_start_days <- as.numeric(difftime(dat$date_of_this_measurement, dat$date_start_sickness, units = "days"))
    diff_this_start_weeks <- as.numeric(difftime(dat$date_of_this_measurement, dat$date_start_sickness, units = "weeks"))


#Calculate  hours per day

    hours_per_day <- dat$hours_work_week / dat$days_work_week

#Short absenteeism

    if(dat$sick_longer_than_4_weeks == 0){

      dat$abs_short = hours_per_day * dat$days_sick * dat$recall_period_in_weeks * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende "]


#Situation 1 Duration of absence is shorter than friction period

    }else{
      if(cbs_friction_period_weeks >= diff_this_start_weeks){
        dat$abs_long = dat$hours_work_week * dat$recall_period_in_weeks * dat$df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende "]

#Situation 1 Duration of absence is longer than friction period

      }else{
        if(cbs_friction_period_weeks >= dat$recall_period_in_weeks){
        dat$abs_long = dat$hours_work_week * dat$recall_period_in_weeks * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende "]
        }else{
      dat$abs_long = dat$hours_work_week *cbs_friction_period_weeks * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende "]
    }
    }
}

#Presenteeism

    dat$presenteeism <-  dat$days_suffering_from_problems * (1 - (rate_of_work/10)) * hours_per_day * (dat$recall_period_in_weeks/4) * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Productiviteitskosten per uur per betaald werkende"]


#Unpaid work

    dat$unpaid_work <- dat$days_less_unpaid_work * dat$average_hours_unpaid_work * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Vervangingskosten per uur"] * (dat$recall_period_in_weeks/4)


  }

#Return dat
  dat

    }



