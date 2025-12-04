# Add survival information to a cohort table

Add survival information to a cohort table

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

  cohort table to add survival information

- cdm:

  CDM reference

- outcomeCohortTable:

  The outcome cohort table of interest.

- outcomeCohortId:

  ID of event cohorts to include. Only one outcome (and so one ID) can
  be considered. It can either be a cohort_definition_id value or a
  cohort_name.

- outcomeDateVariable:

  Variable containing date of outcome event

- outcomeWashout:

  Washout time in days for the outcome. If an individual has an outcome
  during the washout period, status and time will be set to NA

- censorOnCohortExit:

  If TRUE, an individual's follow up will be censored at their cohort
  exit

- censorOnDate:

  if not NULL, an individual's follow up will be censored at the given
  date

- followUpDays:

  Number of days to follow up individuals (lower bound 1, upper bound
  Inf)

- name:

  Name of the new table, if NULL a temporary table is returned.

## Value

Two additional columns will be added to x. The "time" column will
contain number of days to censoring. The "status" column will indicate
whether the patient had the event (value: 1), or did not have the event
(value: 0)

## Examples

``` r
# \donttest{

cdm <- mockMGUS2cdm()
cdm$mgus_diagnosis <- cdm$mgus_diagnosis |>
  addCohortSurvival(
    cdm = cdm,
    outcomeCohortTable = "death_cohort",
    outcomeCohortId = 1
  )
  # }
```
