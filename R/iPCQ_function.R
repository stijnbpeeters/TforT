#'All costprices following the costing manual
#' @export func_iPCQ

func_iPCQ <- function(dat, recall_weeks, reference_year, currency, yeardays = 365.25){

#Import required packages
  require("dplyr")
  require("cbsodataR")
  require("tidyverse")
  require("here")
  require("tatooheene")



# Load in friction period function ----------------------------------------

  # Download new dataset with year


# Function for productivity price

costprice_prod <- nl_ref_prices(short_unit = "prodloss_paid_hour", currency = currency, year = reference_year) %>%
  select("Price") %>%
  pull(.)


#Calculate friction period

cbs_friction_period_weeks <- friction_period(year = reference_year,
                                               output = "value")

cbs_friction_period_days <- friction_period(year = reference_year,
                                              output = "value",
                                              units = "days")

#Calculations in case of prior measurement



#Names of columns that have to be included in data file (dat)

col_names_first_measurement_F <- c("date_of_this_measurement",
                                   "hours_work_week",
                                   "days_work_week",
                                   "days_sick",
                                   "sick_longer_than_4_weeks",
                                   "date_start_sickness",
                                   "days_suffering_from_problems",
                                   "rate_of_work",
                                   "days_less_unpaid_work",
                                   "average_hours_unpaid_work")

#Check for the presence of required columns in the input dataset

if(length(setdiff(col_names_first_measurement_F, names(dat))) > 0) stop(cat(
      "All iPCQ columns need to be present in dat. The following are missing:",
      setdiff(col_names_first_measurement_F, names(dat))))

# Recall period


recall_weeks_days <- as.numeric(recall_weeks) * 7

start_date <- dat$date_of_this_measurement - recall_weeks_days

#Calculate difference in days between date_of_this_measurement and date_of_prior_measuremen

diff_this_prior_days <- as.numeric(
  dat$date_of_this_measurement - start_date
)

diff_this_prior_weeks <- diff_this_prior_days / 7

#Calculate difference between date start of sickness and date of this measurement

diff_this_start_days <- as.numeric(difftime(dat$date_of_this_measurement, dat$date_start_sickness, units = "days"))
diff_this_start_weeks <- as.numeric(difftime(dat$date_of_this_measurement, dat$date_start_sickness, units = "weeks"))

#Calculate difference between date start of sickness and date of prior measurement

diff_prior_start_days <- as.numeric(difftime(start_date, dat$date_start_sickness, units = "days"))
diff_prior_start_weeks <- as.numeric(difftime(start_date, dat$date_start_sickness, units = "weeks"))

#Calculate  hours per day

hours_per_day <- dat$hours_work_week / dat$days_work_week




#Calculating Absenteeism
#Short absenteeism

dat$abs_short <- ifelse(dat$sick_longer_than_4_weeks == 0,
                        hours_per_day * dat$days_sick * costprice_prod * recall_weeks_days,
                        0)

#Long absenteeism
#Situation 1a: Duration of absence is shorter than the friction period and the start of the sickness starts before the recall period date
abs_long_1a <- ifelse(dat$sick_longer_than_4_weeks == 1 & cbs_friction_period_days >= diff_this_start_days & diff_this_start_days >= diff_this_prior_days,
                          dat$hours_work_week * diff_this_prior_weeks * costprice_prod,
                          0)

#Situation 1B: Duration of absence is shorter than the friction period and the start of the sickness starts after the recall period date
abs_long_1b <- ifelse(dat$sick_longer_than_4_weeks == 1 & cbs_friction_period_days >= diff_this_start_days & diff_this_prior_days > diff_this_start_days,
                      dat$hours_work_week * diff_this_start_weeks * costprice_prod,
                      0)

#Situation 2A: Duration of absence is longer than the friction period and the whole friction period is before the prior measurement
abs_long_2a <- ifelse(dat$sick_longer_than_4_weeks == 1 & diff_this_start_days > cbs_friction_period_days & diff_prior_start_days >= cbs_friction_period_days,
                      0,
                      0)

#Situation 2B: Duration of absence is longer than the friction period and the  friction period is (partly) between the two measurements
abs_long_2b <- ifelse(dat$sick_longer_than_4_weeks == 1 & diff_this_start_days > cbs_friction_period_days & cbs_friction_period_days - diff_prior_start_days > 0,
                      (cbs_friction_period_weeks - diff_prior_start_weeks) * dat$hours_work_week * costprice_prod,
                      0)


#Difference in time between the start sickness and timepoint in days

dat$difftime_days_sick <- diff_this_start_days

#Difference in time between the two timepoints

dat$difftime_days_recall <- diff_this_prior_days

#long absenteeism

dat$abs_long <- abs_long_1a + abs_long_1b + abs_long_2a + abs_long_2b

#Absenteeism

dat$absenteeism <- dat$abs_short +dat$abs_long

#Calculations for Presenteeism

dat$presenteeism <- dat$days_suffering_from_problems * (1 - (dat$rate_of_work/10)) * hours_per_day * recall_weeks_days * costprice_prod

#Calculations for Unpaid work

replacement_cost <- nl_ref_prices(short_unit = "prodloss_unpaid_hour", currency = "EUR", reference_year) %>%
  select("Price") %>%
  pull(.)

dat$unpaid_work <- dat$days_less_unpaid_work * dat$average_hours_unpaid_work * replacement_cost * recall_weeks_days

dat <-  data.frame(dat)

return(dat)
}






