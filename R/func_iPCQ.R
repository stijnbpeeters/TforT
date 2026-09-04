#' Backward-compatible iPCQ interface
#'
#' `func_iPCQ()` is retained for existing scripts. New code should use
#' [calculate_ipcq()], which makes the input and target periods explicit.
#'
#' @inheritParams calculate_ipcq
#' @param recall_weeks Target period in weeks.
#' @param yeardays Deprecated and ignored.
#' @return The value returned by [calculate_ipcq()].
#' @export
func_iPCQ <- function(dat, recall_weeks, reference_year, currency = "EUR",
                      yeardays = 365.25) {
  warning("`func_iPCQ()` is deprecated; use `calculate_ipcq()`.", call. = FALSE)
  calculate_ipcq(
    dat = dat,
    reference_year = reference_year,
    currency = currency,
    input_period_weeks = 4,
    target_period_weeks = recall_weeks
  )
}
