# Table with survival summary

Table with survival summary

## Usage

``` r
tableSurvival(
  x,
  times = NULL,
  timeScale = "days",
  header = c("estimate"),
  type = "gt",
  groupColumn = NULL,
  .options = list()
)
```

## Arguments

- x:

  Result from estimateSingleEventSurvival or
  estimateCompetingRiskSurvival

- times:

  Times at which to report survival in the summary table

- timeScale:

  Time unit to report survival in: days, months or years

- header:

  A vector containing which elements should go into the header. Allowed
  are: cdm_name, group, strata, additional, variable, estimate, and
  settings.

- type:

  Type of desired formatted table, possibilities: "gt", "flextable", and
  "tibble".

- groupColumn:

  Columns to use as group labels.

- .options:

  Named list with additional formatting options.
  CohortSurvival::optionsTableSurvival() shows allowed arguments and
  their default values.

## Value

A tibble containing a summary of observed survival in the required units

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
tableSurvival(surv, times = c(50,100,365))


  


CDM name
```

Target cohort

Outcome name

Estimate name

Number records

Number events

Median survival (95% CI)

Restricted mean survival (95% CI)

50 days survival estimate

100 days survival estimate

365 days survival estimate

mock

mgus_diagnosis

death_cohort

1,384

963

98.00 (92.00, 103.00)

133.00 (124.00, 141.00)

69.67 (67.28, 72.13)

48.50 (45.87, 51.29)

6.84 (3.36, 13.92)

\# }
