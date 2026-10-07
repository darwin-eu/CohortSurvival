# Keep survival estimates only when the probability changes

Reduce the size of a survival result by removing time points at which
the reported survival or cumulative-incidence probability is unchanged
from the previous reported time. The first time point in every curve is
always kept. Confidence limits for a retained time point are kept with
its estimate.

## Usage

``` r
filterSurvivalChanges(result)
```

## Arguments

- result:

  A `summarised_result` produced by CohortSurvival.

## Value

A `summarised_result` with unchanged probabilities removed.

## Details

Only `survival_estimates` rows are filtered. Event counts, summaries,
attrition, settings, and other result types are returned unchanged. This
makes the result suitable for
[`plotSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/plotSurvival.md),
while tables requesting an exact removed time point will not be able to
display that time.

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
result <- estimateSingleEventSurvival(
  cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort"
)
#> ℹ `outcomeWashout` was not provided and defaults to "Inf".
#> ℹ People with any outcome before target cohort entry will be excluded from the
#>   analysis.
#> ℹ Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#>   'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` cast to character.
compactResult <- filterSurvivalChanges(result)
# }
```
