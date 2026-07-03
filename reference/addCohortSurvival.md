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
  name = NULL
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

  ID of event cohorts to include. Only one outcome (and so one ID) can
  be considered. It can either be a cohort_definition_id value or a
  cohort_name.

- outcomeDateVariable:

  Variable containing date of outcome event. This is usually
  `"cohort_start_date"`.

- outcomeWashout:

  Washout time in days for the outcome. If an individual has an outcome
  during the washout period before target cohort entry, `status` and
  `time` will be set to `NA`. Use `Inf` for any prior outcome and `0`
  for no pre-index washout.

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

## Value

A cohort table with two additional columns. The `time` column contains
the number of days to event or censoring. The `status` column indicates
whether the patient had the event (`1`) or was censored (`0`).

## Details

`time` is the number of days from target cohort entry to the first
applicable event or censoring date. Censoring can occur at the end of
observation, at target cohort exit when `censorOnCohortExit = TRUE`, at
`censorOnDate`, or at `followUpDays`. `status` is `1` for people with
the outcome event and `0` for censored records. Records with an outcome
in the washout window are kept in the table with `time` and `status` set
to `NA`, so they can be removed by downstream analyses.

## Examples

``` r
# \donttest{

cdm <- mockMGUS2cdm()
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
