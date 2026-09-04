make_ipcq_row <- function(...) {
  defaults <- list(
    date_of_this_measurement = as.Date("2024-04-01"),
    hours_work_week = 40, days_work_week = 5, days_sick = 0,
    sick_longer_than_4_weeks = 0, date_start_sickness = as.Date(NA),
    days_suffering_from_problems = 0, rate_of_work = 10,
    days_less_unpaid_work = 0, average_hours_unpaid_work = 0
  )
  as.data.frame(utils::modifyList(defaults, list(...)))
}

run_ipcq <- function(dat, ...) {
  calculate_ipcq(
    dat, reference_year = 2024, paid_work_hour_price = 10,
    unpaid_work_hour_price = 5, friction_period_days = 84, ...)
}

test_that("short absence is calculated from working days", {
  # 2 days * (40 / 5) hours/day * EUR 10 = EUR 160
  result <- run_ipcq(make_ipcq_row(days_sick = 2))
  expect_equal(result$absenteeism_hours, 16)
  expect_equal(result$absenteeism_costs, 160)
  expect_equal(result$absenteeism_short_costs, 160)
  expect_equal(result$absenteeism_long_costs, 0)
})

test_that("reported counts can be extrapolated to a longer period", {
  dat <- make_ipcq_row(days_sick = 2, days_suffering_from_problems = 3,
                       rate_of_work = 5, days_less_unpaid_work = 2,
                       average_hours_unpaid_work = 3)
  result <- run_ipcq(dat, input_period_weeks = 4, target_period_weeks = 12)
  expect_equal(result$ipcq_extrapolation_factor, 3)
  expect_equal(result$absenteeism_hours, 48)       # 2 * 3 * 8
  expect_equal(result$presenteeism_hours, 36)      # 3 * 3 * 8 * 0.5
  expect_equal(result$unpaid_work_hours, 18)       # 2 * 3 * 3
  expect_equal(result$total_productivity_costs, 930) # 480 + 360 + 90
})

test_that("long absence uses only overlap with the target period", {
  dat <- make_ipcq_row(sick_longer_than_4_weeks = 1,
                       date_start_sickness = as.Date("2024-02-19"))
  result <- run_ipcq(dat, target_period_weeks = 12)
  expect_equal(result$absenteeism_hours, 240) # 6 weeks * 40 hours
  expect_equal(result$absenteeism_costs, 2400)
  expect_equal(result$absenteeism_short_costs, 0)
  expect_equal(result$absenteeism_long_costs, 2400)
})

test_that("published manual examples reproduce every expected component", {
  case_file <- system.file("extdata", "ipcq_manual_test_cases.csv",
                           package = "TforT")
  if (!nzchar(case_file)) {
    case_file <- testthat::test_path(
      "..", "..", "inst", "extdata", "ipcq_manual_test_cases.csv"
    )
  }
  cases <- utils::read.csv(case_file, na.strings = "", check.names = FALSE)
  cases$date_of_this_measurement <- as.Date(cases$date_of_this_measurement)
  cases$date_start_sickness <- as.Date(cases$date_start_sickness)

  for (i in seq_len(nrow(cases))) {
    result <- calculate_ipcq(
      cases[i, ], reference_year = 2024,
      input_period_weeks = cases$input_period_weeks[i],
      target_period_weeks = cases$target_period_weeks[i],
      paid_work_hour_price = cases$paid_work_hour_price[i],
      unpaid_work_hour_price = cases$unpaid_work_hour_price[i],
      friction_period_days = cases$friction_period_days[i]
    )
    label <- paste("scenario", cases$scenario[i])
    expect_equal(result$absenteeism_short_hours,
                 cases$expected_absenteeism_short_hours[i], info = label)
    expect_equal(result$absenteeism_long_hours,
                 cases$expected_absenteeism_long_hours[i], info = label)
    expect_equal(result$presenteeism_hours,
                 cases$expected_presenteeism_hours[i], info = label)
    expect_equal(result$unpaid_work_hours,
                 cases$expected_unpaid_work_hours[i], info = label)
    expect_equal(result$absenteeism_short_costs,
                 cases$expected_absenteeism_short_costs[i], info = label)
    expect_equal(result$absenteeism_long_costs,
                 cases$expected_absenteeism_long_costs[i], info = label)
    expect_equal(result$presenteeism_costs,
                 cases$expected_presenteeism_costs[i], info = label)
    expect_equal(result$unpaid_work_costs,
                 cases$expected_unpaid_work_costs[i], info = label)
    expect_equal(result$total_productivity_costs,
                 cases$expected_total_productivity_costs[i], info = label)
  }
})

test_that("long absence is capped at the friction period", {
  dat <- make_ipcq_row(sick_longer_than_4_weeks = 1,
                       date_start_sickness = as.Date("2023-11-13"))
  result <- run_ipcq(dat, target_period_weeks = 20)
  expect_equal(result$absenteeism_hours, 12 * 40)
  expect_equal(result$absenteeism_costs, 4800)
})

test_that("a completed friction period costs zero in a later target period", {
  dat <- make_ipcq_row(sick_longer_than_4_weeks = 1,
                       date_start_sickness = as.Date("2023-01-01"))
  result <- run_ipcq(dat, target_period_weeks = 12)
  expect_equal(result$absenteeism_hours, 0)
  expect_equal(result$absenteeism_costs, 0)
})

test_that("only the part of a friction period inside the target period is costed", {
  # Target period: 2024-01-08 through 2024-04-01.
  # Sickness starts 2023-12-18 and its 42-day friction window ends 2024-01-29.
  # Therefore 21 days = 3 weeks overlap: 3 * 40 hours * EUR 10 = EUR 1,200.
  dat <- make_ipcq_row(
    sick_longer_than_4_weeks = 1,
    date_start_sickness = as.Date("2023-12-18")
  )
  result <- calculate_ipcq(
    dat, reference_year = 2024, target_period_weeks = 12,
    paid_work_hour_price = 10, unpaid_work_hour_price = 5,
    friction_period_days = 42
  )
  expect_equal(result$absenteeism_short_hours, 0)
  expect_equal(result$absenteeism_long_hours, 120)
  expect_equal(result$absenteeism_long_costs, 1200)
  expect_equal(result$absenteeism_costs, 1200)
})

test_that("additional uploaded columns are preserved", {
  dat <- make_ipcq_row()
  dat$participant_id <- "P001"
  expect_equal(run_ipcq(dat)$participant_id, "P001")
})

test_that("custom country prices and currency labels are supported", {
  result <- calculate_ipcq(
    make_ipcq_row(days_sick = 1), reference_year = 2024, currency = "GBP",
    paid_work_hour_price = 20, unpaid_work_hour_price = 8,
    friction_period_days = 70
  )
  expect_equal(result$absenteeism_costs, 160)
  expect_equal(result$cost_currency, "GBP")
  expect_equal(result$paid_work_price_source, "user supplied")
  expect_equal(result$friction_period_source, "user supplied")
})

test_that("Dutch EUR prices are obtained from tatooheene", {
  skip_if_not_installed("tatooheene")

  expected_paid <- tatooheene::nl_ref_prices(
    short_unit = "prodloss_paid_hour", currency = "EUR", year = 2024
  )$Price[[1]]
  expected_unpaid <- tatooheene::nl_ref_prices(
    short_unit = "prodloss_unpaid_hour", currency = "EUR", year = 2024
  )$Price[[1]]
  result <- calculate_ipcq(make_ipcq_row(days_sick = 1),
                           reference_year = 2024, currency = "EUR")

  expect_equal(result$paid_work_hour_price, expected_paid)
  expect_equal(result$unpaid_work_hour_price, expected_unpaid)
  expect_equal(result$absenteeism_costs, 8 * expected_paid)
  expect_equal(result$cost_currency, "EUR")
  expect_equal(result$paid_work_price_source,
               "tatooheene (Dutch reference price)")
  expect_equal(result$friction_period_source,
               "tatooheene (Dutch friction period)")
})

test_that("Dutch international-dollar prices are obtained from tatooheene", {
  skip_if_not_installed("tatooheene")

  expected_paid <- tatooheene::nl_ref_prices(
    short_unit = "prodloss_paid_hour", currency = "INT$", year = 2024
  )$Price[[1]]
  expected_unpaid <- tatooheene::nl_ref_prices(
    short_unit = "prodloss_unpaid_hour", currency = "INT$", year = 2024
  )$Price[[1]]
  result <- calculate_ipcq(
    make_ipcq_row(days_sick = 1),
    reference_year = 2024, currency = "INT$"
  )

  expect_equal(result$paid_work_hour_price, expected_paid)
  expect_equal(result$unpaid_work_hour_price, expected_unpaid)
  expect_equal(result$absenteeism_costs, 8 * expected_paid)
  expect_equal(result$cost_currency, "INT$")
})

test_that("one price can be overridden while the other uses tatooheene", {
  skip_if_not_installed("tatooheene")

  expected_unpaid <- tatooheene::nl_ref_prices(
    short_unit = "prodloss_unpaid_hour", currency = "EUR", year = 2024
  )$Price[[1]]
  result <- calculate_ipcq(
    make_ipcq_row(days_sick = 1), reference_year = 2024, currency = "EUR",
    paid_work_hour_price = 99
  )

  expect_equal(result$paid_work_hour_price, 99)
  expect_equal(result$unpaid_work_hour_price, expected_unpaid)
  expect_equal(result$paid_work_price_source, "user supplied")
  expect_equal(result$unpaid_work_price_source,
               "tatooheene (Dutch reference price)")
})

test_that("non-Dutch currency requires complete custom prices", {
  expect_error(
    calculate_ipcq(make_ipcq_row(), reference_year = 2024, currency = "GBP",
                   paid_work_hour_price = 20, friction_period_days = 70),
    "Supply both hourly prices"
  )
})

test_that("the input specification lists all required upload columns", {
  expect_equal(nrow(ipcq_input_spec()), 10)
  expect_true(all(ipcq_input_spec()$column %in% names(make_ipcq_row())))
})

test_that("invalid response combinations are rejected", {
  expect_error(run_ipcq(make_ipcq_row(days_sick = 21)),
               "possible working days")
  expect_error(run_ipcq(make_ipcq_row(days_sick = 10,
                                      days_suffering_from_problems = 11)),
               "exceeds the days worked")
  expect_error(run_ipcq(make_ipcq_row(rate_of_work = 11)), "between 0 and 10")
})
