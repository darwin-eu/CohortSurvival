# Estimate survival for a given event of interest using cohorts in the OMOP Common Data Model

Estimate survival for a given event of interest using cohorts in the
OMOP Common Data Model

## Usage

``` r
estimateSingleEventSurvival(
  cdm,
  targetCohortTable,
  outcomeCohortTable,
  targetCohortId = NULL,
  outcomeCohortId = NULL,
  outcomeDateVariable = "cohort_start_date",
  outcomeWashout = Inf,
  censorOnCohortExit = FALSE,
  censorOnDate = NULL,
  followUpDays = Inf,
  strata = NULL,
  eventGap = 30,
  estimateGap = 1,
  restrictedMeanFollowUp = NULL,
  minimumSurvivalDays = 1
)
```

## Arguments

- cdm:

  CDM reference

- targetCohortTable:

  targetCohortTable

- outcomeCohortTable:

  The outcome cohort table of interest.

- targetCohortId:

  Target cohorts to include. It can either be a cohort_definition_id
  value or a cohort_name. Multiple ids are allowed.

- outcomeCohortId:

  Outcome cohorts to include. It can either be a cohort_definition_id
  value or a cohort_name. Multiple ids are allowed.

- outcomeDateVariable:

  Variable containing date of outcome event

- outcomeWashout:

  Washout time in days for the outcome

- censorOnCohortExit:

  If TRUE, an individual's follow up will be censored at their cohort
  exit

- censorOnDate:

  if not NULL, an individual's follow up will be censored at the given
  date

- followUpDays:

  Number of days to follow up individuals (lower bound 1, upper bound
  Inf)

- strata:

  strata

- eventGap:

  Days between time points for which to report survival events, which
  are grouped into the specified intervals.

- estimateGap:

  Days between time points for which to report survival estimates. First
  day will be day zero with risk estimates provided for times up to the
  end of follow-up, with a gap in days equivalent to eventGap.

- restrictedMeanFollowUp:

  number of days of follow-up to take into account when calculating
  restricted mean for all cohorts

- minimumSurvivalDays:

  Minimum number of days required for the main cohort to have survived

## Value

tibble with survival information for desired cohort, including: time,
people at risk, survival probability, cumulative incidence, 95 CIs,
strata and outcome. A tibble with the number of events is outputted as
an attribute of the output

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
surv <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "mgus_diagnosis",
  targetCohortId = 1,
  outcomeCohortTable = "death_cohort",
  outcomeCohortId = 1,
  eventGap = 7
)
#> - Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#> 'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` casted to character.
# }
```
