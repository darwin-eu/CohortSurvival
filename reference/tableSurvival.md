# Table with survival summary

Table with survival summary

## Usage

``` r
tableSurvival(
  x,
  times = NULL,
  timeScale = "days",
  header = c("estimate"),
  estimates = c("median_survival", "restricted_mean_survival"),
  type = "gt",
  groupColumn = NULL,
  hide = c("result_id", "estimate_type"),
  style = NULL,
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

  A vector specifying the elements to include in the header. The order
  of elements matters, with the first being the topmost header. Elements
  in header can be:

  - Any of the columns returned by `tableColumns(result)` to create a
    header for these columns.

  - Any other input to create an overall header.

- estimates:

  Character vector specifying which estimates to include in the table.
  Options include: "median_survival", "restricted_mean_survival",
  "q0_survival", "q05_survival", "q25_survival", "q75_survival",
  "q95_survival", "q100_survival". By default it includes
  c("median_survival", "restricted_mean_survival").

- type:

  Character string specifying the desired output table format. See
  `tableType()` for supported table types. If `type = NULL`, global
  options (set via `setGlobalTableOptions()`) will be used if available;
  otherwise, a default `'gt'` table is created.

- groupColumn:

  Columns to use as group labels, to see options use
  `tableColumns(result)`. By default, the name of the new group will be
  the tidy\* column names separated by ";". To specify a custom group
  name, use a named list such as: list("newGroupName" =
  c("variable_name", "variable_level")).

  \*tidy: The tidy format applied to column names replaces "\_" with a
  space and converts to sentence case. Use `rename` to customise
  specific column names.

- hide:

  Columns to drop from the output table. By default, `result_id` and
  `estimate_type` are always dropped.

- style:

  Defines the visual formatting of the table. This argument can be
  provided in one of the following ways:

  1.  **Pre-defined style:** Use the name of a built-in style (e.g.,
      `"darwin"`). See `tableStyle()` for available options.

  2.  **YAML file path:** Provide the path to an existing `.yml` file
      defining a new style.

  3.  **List of custome R code:** Supply a block of custom R code or a
      named list describing styles for each table section. This code
      must be specific to the selected table type. If `style = NULL`,
      the function will use global options (see
      `setGlobalTableOptions()`) or an existing `_brand.yml` file (if
      found); otherwise, the default style is applied. For more details,
      see the *Styles* vignette on the package website.

- .options:

  A named list with additional formatting options.
  [`visOmopResults::tableOptions()`](https://darwin-eu.github.io/visOmopResults/reference/tableOptions.html)
  shows allowed arguments and their default values.

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
