# A tidy implementation of the summarised_characteristics object.

A tidy implementation of the summarised_characteristics object.

## Usage

``` r
asSurvivalResult(result)
```

## Arguments

- result:

  A summarised_characteristics object.

## Value

A tibble with a tidy version of the summarised_characteristics object.

## Examples

``` r
# \donttest{
cdm <- mockMGUS2cdm()
surv <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "mgus_diagnosis",
  targetCohortId = 1,
  outcomeCohortTable = "death_cohort",
  outcomeCohortId = 1,
  eventGap = 7
) %>%
  asSurvivalResult()
#> - Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#> 'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` casted to character.
# }
```
