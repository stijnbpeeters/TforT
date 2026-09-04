# Development utility; this file is deliberately outside R/ so it is not run
# when the package is loaded.

generate_ipcq_example <- function(
    output_file = file.path("inst", "extdata", "ipcq_example.xlsx"),
    n = 50,
    seed = 123) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Install `writexl` to generate the Excel example.", call. = FALSE)
  }

  set.seed(seed)
  days_work_week <- sample(2:5, n, replace = TRUE)
  possible_workdays <- days_work_week * 4
  days_sick <- vapply(possible_workdays, function(maximum) {
    sample(0:maximum, 1, prob = rev(seq_len(maximum + 1)))
  }, numeric(1))
  long_absence <- stats::rbinom(n, 1, 0.15)
  sickness_start <- as.Date(rep(NA_character_, n))
  sickness_start[long_absence == 1] <-
    as.Date("2024-02-01") - sample(29:120, sum(long_absence), replace = TRUE)
  days_worked <- possible_workdays - days_sick
  affected_days <- vapply(days_worked, function(maximum) {
    sample(0:maximum, 1)
  }, numeric(1))

  dat <- data.frame(
    participant_id = sprintf("P%03d", seq_len(n)),
    date_of_this_measurement = as.Date("2024-02-01"),
    hours_work_week = sample(c(16, 20, 24, 28, 32, 36, 40), n,
                             replace = TRUE),
    days_work_week = days_work_week,
    days_sick = days_sick,
    sick_longer_than_4_weeks = long_absence,
    date_start_sickness = sickness_start,
    days_suffering_from_problems = affected_days,
    rate_of_work = ifelse(affected_days == 0, 10,
                          sample(3:9, n, replace = TRUE)),
    days_less_unpaid_work = sample(0:7, n, replace = TRUE),
    average_hours_unpaid_work = sample(0:8, n, replace = TRUE)
  )

  output_directory <- dirname(output_file)
  if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
  writexl::write_xlsx(dat, output_file)
  invisible(normalizePath(output_file))
}

# Run explicitly during development, from the package root:
# source("data-raw/generate_ipcq_example.R")
# generate_ipcq_example()
