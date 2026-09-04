#' Calculate productivity costs from iPCQ responses
#'
#' Counts in `dat` must describe `input_period_weeks`. They can be extrapolated
#' to `target_period_weeks`; long absence is calculated from its start date and
#' capped at the friction period.
#'
#' @param dat A data frame containing the required iPCQ columns. Additional
#'   columns are preserved.
#' @param reference_year Year used to select Dutch reference prices.
#' @param currency Currency label for the results. Use `"EUR"` or `"INT$"`
#'   when obtaining Dutch prices from `tatooheene`. Any non-empty label (for
#'   example `"GBP"`) is accepted when both hourly prices are supplied by the
#'   user.
#' @param input_period_weeks Weeks represented by reported counts. The standard
#'   iPCQ uses four weeks.
#' @param target_period_weeks Period for which costs are returned.
#' @param paid_work_hour_price Optional paid-work price per hour.
#' @param unpaid_work_hour_price Optional unpaid-work replacement price per hour.
#' @param friction_period_days Optional friction period in calendar days.
#' @param na_action Either `"error"` or `"propagate"`.
#' @return `dat` with calculated hours, component costs, total costs, and inputs.
#' @export
calculate_ipcq <- function(
    dat,
    reference_year,
    currency = "EUR",
    input_period_weeks = 4,
    target_period_weeks = input_period_weeks,
    paid_work_hour_price = NULL,
    unpaid_work_hour_price = NULL,
    friction_period_days = NULL,
    na_action = c("error", "propagate")) {

  na_action <- match.arg(na_action)

  required <- c(
    "date_of_this_measurement", "hours_work_week", "days_work_week",
    "days_sick", "sick_longer_than_4_weeks", "date_start_sickness",
    "days_suffering_from_problems", "rate_of_work",
    "days_less_unpaid_work", "average_hours_unpaid_work"
  )
  if (!is.data.frame(dat)) stop("`dat` must be a data frame.", call. = FALSE)
  missing_columns <- setdiff(required, names(dat))
  if (length(missing_columns)) {
    stop(sprintf("Missing required iPCQ columns: %s.",
                 paste(missing_columns, collapse = ", ")), call. = FALSE)
  }
  .ipcq_scalar(input_period_weeks, "input_period_weeks", positive = TRUE)
  .ipcq_scalar(target_period_weeks, "target_period_weeks", positive = TRUE)
  .ipcq_scalar(reference_year, "reference_year", positive = TRUE)
  if (!is.character(currency) || length(currency) != 1L || is.na(currency) ||
      !nzchar(currency)) {
    stop("`currency` must be one non-empty character value.", call. = FALSE)
  }

  date_measurement <- .ipcq_date(dat$date_of_this_measurement,
                                 "date_of_this_measurement")
  date_start <- .ipcq_date(dat$date_start_sickness, "date_start_sickness")
  numeric_columns <- c(
    "hours_work_week", "days_work_week", "days_sick",
    "days_suffering_from_problems", "rate_of_work",
    "days_less_unpaid_work", "average_hours_unpaid_work"
  )
  not_numeric <- numeric_columns[!vapply(dat[numeric_columns], is.numeric, logical(1))]
  if (length(not_numeric)) {
    stop(sprintf("These columns must be numeric: %s.",
                 paste(not_numeric, collapse = ", ")), call. = FALSE)
  }
  long_absence <- dat$sick_longer_than_4_weeks
  if (is.logical(long_absence)) long_absence <- as.integer(long_absence)
  if (!is.numeric(long_absence) ||
      any(!is.na(long_absence) & !long_absence %in% c(0, 1))) {
    stop("`sick_longer_than_4_weeks` must contain only 0, 1, or NA.", call. = FALSE)
  }
  if (na_action == "error") {
    required_na <- required[vapply(dat[required], anyNA, logical(1))]
    required_na <- setdiff(required_na, "date_start_sickness")
    missing_long_start <- any(long_absence == 1 & is.na(date_start), na.rm = TRUE)
    if (length(required_na) || missing_long_start) {
      affected <- unique(c(required_na,
                           if (missing_long_start) "date_start_sickness"))
      stop(sprintf("Missing required values in: %s.",
                   paste(affected, collapse = ", ")), call. = FALSE)
    }
  }
  nonnegative <- setdiff(numeric_columns, "rate_of_work")
  bad_negative <- nonnegative[vapply(dat[nonnegative], function(x) {
    any(x < 0, na.rm = TRUE)
  }, logical(1))]
  if (length(bad_negative)) {
    stop(sprintf("Negative values are not allowed in: %s.",
                 paste(bad_negative, collapse = ", ")), call. = FALSE)
  }
  if (any(dat$days_work_week <= 0 | dat$days_work_week > 7, na.rm = TRUE)) {
    stop("`days_work_week` must be greater than 0 and no greater than 7.", call. = FALSE)
  }
  if (any(dat$rate_of_work < 0 | dat$rate_of_work > 10, na.rm = TRUE)) {
    stop("`rate_of_work` must be between 0 and 10.", call. = FALSE)
  }
  possible_workdays <- dat$days_work_week * input_period_weeks
  if (any(dat$days_sick > possible_workdays, na.rm = TRUE)) {
    stop("`days_sick` exceeds the possible working days in the input period.",
         call. = FALSE)
  }
  if (any(dat$days_suffering_from_problems > possible_workdays - dat$days_sick,
          na.rm = TRUE)) {
    stop(paste0("`days_suffering_from_problems` exceeds the days worked after ",
                "subtracting sickness absence."), call. = FALSE)
  }

  paid_price_source <- if (is.null(paid_work_hour_price)) {
    "tatooheene (Dutch reference price)"
  } else "user supplied"
  unpaid_price_source <- if (is.null(unpaid_work_hour_price)) {
    "tatooheene (Dutch reference price)"
  } else "user supplied"
  friction_source <- if (is.null(friction_period_days)) {
    "tatooheene (Dutch friction period)"
  } else "user supplied"
  if ((is.null(paid_work_hour_price) || is.null(unpaid_work_hour_price)) &&
      !currency %in% c("EUR", "INT$")) {
    stop(paste0("`tatooheene` supports Dutch reference prices in `EUR` or ",
                "`INT$`. Supply both hourly prices to use another currency."),
         call. = FALSE)
  }

  paid_work_hour_price <- .ipcq_price(
    paid_work_hour_price, "prodloss_paid_hour", reference_year, currency,
    "paid_work_hour_price")
  unpaid_work_hour_price <- .ipcq_price(
    unpaid_work_hour_price, "prodloss_unpaid_hour", reference_year, currency,
    "unpaid_work_hour_price")
  if (is.null(friction_period_days)) {
    .ipcq_require_tatooheene()
    friction_period_days <- tatooheene::friction_period(
      year = reference_year, units = "days", output = "value")
  }
  .ipcq_scalar(friction_period_days, "friction_period_days", positive = TRUE)

  period_factor <- target_period_weeks / input_period_weeks
  hours_per_day <- dat$hours_work_week / dat$days_work_week
  short_hours <- dat$days_sick * period_factor * hours_per_day
  target_start <- date_measurement - target_period_weeks * 7
  friction_end <- date_start + friction_period_days
  overlap_days <- as.numeric(pmax(
    0, pmin(date_measurement, friction_end) - pmax(target_start, date_start)
  ))
  long_hours <- overlap_days / 7 * dat$hours_work_week

  dat$ipcq_extrapolation_factor <- period_factor
  dat$absenteeism_short_hours <- ifelse(long_absence == 0, short_hours, 0)
  dat$absenteeism_long_hours <- ifelse(long_absence == 1, long_hours, 0)
  dat$absenteeism_hours <- dat$absenteeism_short_hours +
    dat$absenteeism_long_hours
  dat$presenteeism_hours <- dat$days_suffering_from_problems * period_factor *
    hours_per_day * (1 - dat$rate_of_work / 10)
  dat$unpaid_work_hours <- dat$days_less_unpaid_work * period_factor *
    dat$average_hours_unpaid_work
  dat$absenteeism_short_costs <- dat$absenteeism_short_hours *
    paid_work_hour_price
  dat$absenteeism_long_costs <- dat$absenteeism_long_hours *
    paid_work_hour_price
  dat$absenteeism_costs <- dat$absenteeism_short_costs +
    dat$absenteeism_long_costs
  dat$presenteeism_costs <- dat$presenteeism_hours * paid_work_hour_price
  dat$unpaid_work_costs <- dat$unpaid_work_hours * unpaid_work_hour_price
  dat$total_productivity_costs <- dat$absenteeism_costs +
    dat$presenteeism_costs + dat$unpaid_work_costs
  dat$paid_work_hour_price <- paid_work_hour_price
  dat$unpaid_work_hour_price <- unpaid_work_hour_price
  dat$friction_period_days <- friction_period_days
  dat$cost_currency <- currency
  dat$paid_work_price_source <- paid_price_source
  dat$unpaid_work_price_source <- unpaid_price_source
  dat$friction_period_source <- friction_source
  dat
}

.ipcq_scalar <- function(x, name, positive = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (positive) ok <- ok && x > 0
  if (!ok) stop(sprintf("`%s` must be one %sfinite number.", name,
                        if (positive) "positive " else ""), call. = FALSE)
  invisible(x)
}

.ipcq_date <- function(x, name) {
  converted <- tryCatch(as.Date(x), error = function(e) rep(as.Date(NA), length(x)))
  if (any(!is.na(x) & is.na(converted))) {
    stop(sprintf("`%s` contains invalid dates.", name), call. = FALSE)
  }
  converted
}

.ipcq_require_tatooheene <- function() {
  if (!requireNamespace("tatooheene", quietly = TRUE)) {
    stop(paste0("Package `tatooheene` is required when prices or the friction ",
                "period are not supplied explicitly."), call. = FALSE)
  }
}

.ipcq_price <- function(value, short_unit, year, currency, argument) {
  if (is.null(value)) {
    .ipcq_require_tatooheene()
    price_table <- tatooheene::nl_ref_prices(
      short_unit = short_unit, currency = currency, year = year)
    if (!"Price" %in% names(price_table) || nrow(price_table) != 1L) {
      stop(sprintf("Could not find exactly one `%s` reference price.", short_unit),
           call. = FALSE)
    }
    value <- price_table$Price[[1L]]
  }
  .ipcq_scalar(value, argument)
  if (value < 0) stop(sprintf("`%s` cannot be negative.", argument), call. = FALSE)
  value
}
