set.seed(123)
n <- 50

dat <- data.frame(
  ID = 1:50,
  Group = sample(c(0,1), replace = TRUE, prob = c(0.5, 0.5)),
  date_of_this_measurement = rep(as.Date("2024-02-01"), n),
  hours_work_week = sample(c(16,20,24,28,32,36,40), n, replace = TRUE),
  days_work_week = sample(2:5, n, replace = TRUE),
  days_sick = sample(
    c(0:10, 15,20,25,30),
    n,
    replace = TRUE,
    prob = c(rep(8,11), 2,2,1,1))
)

dat$date_start_sickness <- ifelse(
  dat$days_sick == 0,
  NA,
  as.character(
    dat$date_of_this_measurement -
      sample(1:60, n, replace = TRUE)
  )
)

date_dif <- dat$date_of_this_measurement  - as.Date(dat$date_start_sickness)


dat$sick_longer_than_4_weeks <- ifelse(date_dif >=28, 1, 0)



dat$date_start_sickness <-
  as.Date(dat$date_start_sickness)


# Number of days suffering from health problems
dat$days_suffering_from_problems <-
  ifelse(
    dat$days_sick == 0,
    0,
    pmax(dat$days_sick,
         sample(1:30, n, replace = TRUE))
  )

# Self-rated productivity (0 = unable to work, 10 = normal productivity)
dat$rate_of_work <-
  ifelse(
    dat$days_suffering_from_problems == 0,
    10,
    sample(3:10, n, replace = TRUE)
  )

# Unpaid work
dat$days_less_unpaid_work <-
  sample(0:7, n, replace = TRUE)

dat$average_hours_unpaid_work <-
  sample(c(0, 1, 2, 3, 4, 5, 6, 8), n, replace = TRUE)

## Write as test file
writexl::write_xlsx(dat, "~/TforT/TforT-package/R/data/example_data.xlsx")






