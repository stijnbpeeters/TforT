# TforT

TforT calculates productivity costs from iMTA Productivity Cost Questionnaire
(iPCQ) responses. It supports Dutch reference prices in euros or international
dollars through `tatooheene`, as well as user-supplied prices and friction
periods for other settings.

## Install from GitHub

```r
install.packages("remotes")
remotes::install_github("stijnbpeeters/TforT")
```

Load the package:

```r
library(TforT)
```

## Calculate in R

Inspect the required columns:

```r
ipcq_input_spec()
```

Calculate four-week costs with Dutch 2024 euro prices:

```r
results <- calculate_ipcq(
  dat = my_data,
  reference_year = 2024,
  currency = "EUR"
)
```

Project four-week answers to twelve weeks:

```r
results <- calculate_ipcq(
  my_data,
  reference_year = 2024,
  currency = "INT$",
  input_period_weeks = 4,
  target_period_weeks = 12
)
```

Use assumptions from another country:

```r
results <- calculate_ipcq(
  my_data,
  reference_year = 2024,
  currency = "GBP",
  paid_work_hour_price = 20,
  unpaid_work_hour_price = 8,
  friction_period_days = 70
)
```

## Start the Shiny calculator

```r
run_ipcq_app()
```

See `vignette("ipcq-r-guide", package = "TforT")` and
`vignette("ipcq-shiny-guide", package = "TforT")` for full instructions.

## Methodological note

Counts must describe the selected input period. Projection to a longer target
period assumes the observed short absence, presenteeism, and unpaid-work losses
are representative of that longer period. Long absence is calculated from its
start date and is limited to the part of the friction window overlapping the
target period.

Users remain responsible for selecting an appropriate questionnaire version,
unit costs, friction period, country, currency, and reference year. Permission
and citation requirements for the iMTA questionnaires also remain applicable.
