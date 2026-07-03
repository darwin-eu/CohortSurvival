# Convert survival summarised results to a survival-specific format

Convert the long `omopgenerics::summarised_result` returned by
[`estimateSingleEventSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/estimateSingleEventSurvival.md)
or
[`estimateCompetingRiskSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/estimateCompetingRiskSurvival.md)
into a wider `survival_result` object that is easier to inspect
manually. The main object contains time-specific estimates when
available. Event counts, summary statistics, and attrition are stored as
attributes named `"events"`, `"summary"`, and `"attrition"`.

## Usage

``` r
asSurvivalResult(result)
```

## Arguments

- result:

  A summarised_result object.

## Value

A `survival_result` object.

## Details

The plotting and table functions in CohortSurvival accept both formats.
The original `summarised_result` is usually preferable for exporting,
binding with other omopgenerics results, and reporting through
visOmopResults.

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
) |>
asSurvivalResult()
#> ℹ Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#>   'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` cast to character.
#> Warning: eventgap column will be added to the survival result object to include all
#> relevant information
# }
```
