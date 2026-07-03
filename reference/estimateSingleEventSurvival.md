# Estimate survival for a single event of interest

Estimate Kaplan-Meier survival for one or more target cohorts and
outcome cohorts in an OMOP Common Data Model reference. The target
cohort defines the population at risk and the index date for follow-up.
The outcome cohort defines the event of interest.

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
  weight = NULL,
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

  A CDM reference created by CDMConnector.

- targetCohortTable:

  Name of the cohort table containing the target cohorts. The table must
  be present in `cdm` and contain standard OMOP cohort columns.

- outcomeCohortTable:

  Name of the cohort table containing the outcome cohorts. The table
  must be present in `cdm` and contain standard OMOP cohort columns.

- targetCohortId:

  Target cohorts to include. It can either be a cohort_definition_id
  value or a cohort_name. Multiple ids are allowed. If `NULL`, all
  non-empty cohorts in `targetCohortTable` are used.

- outcomeCohortId:

  Outcome cohorts to include. It can either be a cohort_definition_id
  value or a cohort_name. Multiple ids are allowed. If `NULL`, all
  outcome cohorts in `outcomeCohortTable` are used.

- outcomeDateVariable:

  Variable containing the outcome event date. This is usually
  `"cohort_start_date"`, but another date column in the outcome cohort
  can be used.

- outcomeWashout:

  Number of days before target cohort entry used to exclude people with
  a prior outcome. `Inf` excludes people with any prior outcome before
  index; `0` applies no pre-index washout.

- censorOnCohortExit:

  If TRUE, an individual's follow up will be censored at their target
  cohort exit date.

- censorOnDate:

  If not NULL, an individual's follow up will be censored at the given
  date. This can be a scalar Date or the name of a date column in the
  target cohort table.

- weight:

  If not NULL, the name of a numeric column in the target cohort table
  containing observation weights to use in the Kaplan-Meier estimation.

- followUpDays:

  Number of days to follow up individuals (lower bound 1, upper bound
  Inf). Follow-up is censored at this value.

- strata:

  A list of target cohort column names to stratify by. Each element can
  be one column name or a character vector of column names for a
  combined stratum, for example `list("sex", c("age_group", "sex"))`.

- eventGap:

  Days between time points for which to report survival events, which
  are grouped into the specified intervals.

- estimateGap:

  Days between time points for which to report survival estimates. First
  day will be day zero with risk estimates provided for times up to the
  end of follow-up, with a gap in days equivalent to estimateGap.

- restrictedMeanFollowUp:

  Number of days of follow-up to use when calculating restricted mean
  survival. See Details.

- minimumSurvivalDays:

  Minimum number of days required for the main cohort to contribute to
  the analysis.

## Value

An `omopgenerics::summarised_result` object with result types
`survival_estimates`, `survival_events`, `survival_summary`, and
`survival_attrition` when available.

## Details

The returned object is an `omopgenerics::summarised_result` containing
survival estimates, event counts, summary statistics, and attrition. Use
[`asSurvivalResult()`](https://darwin-eu.github.io/CohortSurvival/reference/asSurvivalResult.md)
when you want a wider, survival-specific view for manual inspection or
downstream modelling.

`restrictedMeanFollowUp` defines the time horizon used for the
restricted mean survival time. It is calculated as the area under the
survival curve up to that horizon. If `restrictedMeanFollowUp = NULL`,
the horizon is left to the underlying survival summary. In stratified
analyses, this can use a common maximum follow-up time across the fitted
curves. A stratum with shorter observed follow-up may therefore have its
last survival estimate carried forward and integrated beyond its own
maximum follow-up. This means restricted mean survival can be larger
than the observed follow-up time for that stratum, and comparisons
across strata may be misleading. Set a common clinically meaningful
value that is supported by follow-up in all groups when restricted mean
survival will be compared across cohorts or strata. If the requested
horizon is beyond the available follow-up for a curve, the restricted
mean is reported as missing.

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
#> ℹ Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#>   'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` cast to character.
# }
```
