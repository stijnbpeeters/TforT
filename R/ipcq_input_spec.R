#' Required columns for an iPCQ upload
#'
#' Returns a compact data dictionary suitable for displaying in documentation
#' or a Shiny application. Uploaded data may contain any additional columns;
#' they are preserved by [calculate_ipcq()].
#'
#' @return A data frame with column names, types, requirements, and descriptions.
#' @export
ipcq_input_spec <- function() {
  data.frame(
    column = c(
      "date_of_this_measurement", "hours_work_week", "days_work_week",
      "days_sick", "sick_longer_than_4_weeks", "date_start_sickness",
      "days_suffering_from_problems", "rate_of_work",
      "days_less_unpaid_work", "average_hours_unpaid_work"
    ),
    type = c("Date", "numeric", "numeric", "numeric", "0/1", "Date",
             "numeric", "numeric", "numeric", "numeric"),
    required = c(rep("yes", 5), "only for long absence", rep("yes", 4)),
    description = c(
      "Questionnaire completion date (YYYY-MM-DD)",
      "Contracted paid-work hours per week",
      "Paid working days per week (greater than 0, at most 7)",
      "Working days absent during the input period",
      "1 for one uninterrupted absence longer than the input period; otherwise 0",
      "First calendar day of the uninterrupted long absence (YYYY-MM-DD)",
      "Days worked with health problems during the input period",
      "Work performed on affected days: 0 (none) to 10 (normal)",
      "Days with less unpaid work during the input period",
      "Average unpaid-work hours lost on each affected day"
    ),
    stringsAsFactors = FALSE
  )
}
