# Add time and event status to a cohort table

Add the columns needed by standard survival modelling functions, such as
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html), to an
OMOP cohort table. This is a lower-level helper: it creates `time` and
`status` but does not fit a Kaplan-Meier curve or return a
`summarised_result`.

## Usage

``` r
addCohortSurvival(
  x,
  cdm,
  outcomeCohortTable,
  outcomeCohortId = 1,
  outcomeDateVariable = "cohort_start_date",
  outcomeWashout = Inf,
  censorOnCohortExit = FALSE,
  censorOnDate = NULL,
  followUpDays = Inf,
  name = NULL,
  censorTie = c("event", "censor")
)
```

## Arguments

- x:

  Cohort table to add survival information to.

- cdm:

  CDM reference created by CDMConnector.

- outcomeCohortTable:

  Name of the cohort table containing the outcome of interest.

- outcomeCohortId:

  IDs of event cohorts to include. Values can be cohort definition IDs
  or cohort names. With one outcome, the added columns are `time` and
  `status`. With multiple outcomes, one pair is added per outcome and
  named `<cohort_name>_time` and `<cohort_name>_status`.

- outcomeDateVariable:

  Variable containing date of outcome event. This is usually
  `"cohort_start_date"`.

- outcomeWashout:

  Washout time in days for the outcome. If an individual has an outcome
  during the washout period before target cohort entry, `status` and
  `time` will be set to `NA`. Use `Inf` for any prior outcome and `0`
  for no pre-index washout. The default is `Inf`.

- censorOnCohortExit:

  If TRUE, an individual's follow up will be censored at their target
  cohort exit.

- censorOnDate:

  If not NULL, an individual's follow up will be censored at the given
  date. This can be a scalar Date or the name of a date column in `x`.

- followUpDays:

  Number of days to follow up individuals (lower bound 1, upper bound
  Inf). Follow-up is censored at this value.

- name:

  Name of the new table, if NULL a temporary table is returned.

- censorTie:

  How to resolve an outcome occurring on the same day as a censoring
  boundary. Use `"event"` (the default) to count the outcome or
  `"censor"` to censor the record at that time.

## Value

A cohort table with `time` and `status` columns for a single outcome.
For multiple outcomes, it contains a `<cohort_name>_time` and
`<cohort_name>_status` pair for every requested outcome.

## Details

`time` is the number of days from target cohort entry to the first
applicable event or censoring date. Censoring can occur at the end of
observation, at target cohort exit when `censorOnCohortExit = TRUE`, at
`censorOnDate`, or at `followUpDays`. `status` is `1` for people with
the outcome event and `0` for censored records. Records with an outcome
in the washout window are kept in the table with `time` and `status` set
to `NA`, so they can be removed by downstream analyses.

By default, an outcome recorded on the same day as a censoring boundary
is counted as an event. This rule is applied consistently to the end of
the observation period, target cohort exit, `censorOnDate`, and
`followUpDays`. Set `censorTie = "censor"` to censor records when an
outcome and censoring boundary occur on the same day instead.

## Examples

``` r
# \donttest{

cdm <- mockMGUS2cdm()
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpWqcmbM/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Creating a new cdm
#> Uploading table person (1384 rows) - [1/7]
#> Uploading table observation_period (1384 rows) - [2/7]
#> Uploading table visit_occurrence (1 rows) - [3/7]
#> Uploading table death_cohort (963 rows) - [4/7]
#> Uploading table mgus_diagnosis (1384 rows) - [5/7]
#> Uploading table progression (115 rows) - [6/7]
#> Uploading table progression_type (230 rows) - [7/7]
cdm$mgus_diagnosis <- cdm$mgus_diagnosis |>
  addCohortSurvival(
    cdm = cdm,
    outcomeCohortTable = "death_cohort",
    outcomeCohortId = 1
  )
#> ℹ `outcomeWashout` was not provided and defaults to "Inf".
#> ℹ People with any outcome before target cohort entry will be excluded from the
#>   analysis.

cdm$mgus_diagnosis |>
  dplyr::select(subject_id, cohort_start_date, time, status) |>
  dplyr::collect()
#> # A tibble: 1,384 × 4
#>    subject_id cohort_start_date  time status
#>         <int> <date>            <dbl>  <dbl>
#>  1          1 1981-01-01           30      1
#>  2          2 1968-01-01           25      1
#>  3          3 1980-01-01           46      1
#>  4          4 1977-01-01           92      1
#>  5          5 1973-01-01            8      1
#>  6          6 1990-01-01            4      1
#>  7          7 1974-01-01          151      1
#>  8          8 1974-01-01            2      1
#>  9         10 1981-01-01          136      1
#> 10         11 1972-01-01            2      1
#> # ℹ 1,374 more rows
# }
```
