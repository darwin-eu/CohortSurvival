# Variables that can be used for faceting and colouring survival plots

Variables that can be used for faceting and colouring survival plots

## Usage

``` r
availableSurvivalGrouping(result, varying = FALSE)
```

## Arguments

- result:

  Survival results

- varying:

  If FALSE (default), only variables with non-unique values will be
  returned, otherwise all available variables will be returned.

## Examples

``` r
# \donttest{
cdm <- mockMGUS2cdm()
surv <- estimateSingleEventSurvival(cdm,
                                    targetCohortTable = "mgus_diagnosis",
                                    outcomeCohortTable = "death_cohort")
#> - Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#> 'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` casted to character.
availableSurvivalGrouping(surv)
#> Warning: eventgap column will be added to the survival result object to include all
#> relevant information
#> [1] "time"                "estimate"            "estimate_95CI_lower"
#> [4] "estimate_95CI_upper"
# }
```
