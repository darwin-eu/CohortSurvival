# Table with survival events

Create a formatted table of the number at risk, number of events, and
number censored by time interval. The available intervals are controlled
by `eventGap` when the survival result is estimated.

## Usage

``` r
tableSurvivalEvents(
  x,
  eventGap = NULL,
  header = c("estimate"),
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
  estimateCompetingRiskSurvival.

- eventGap:

  Event gap defining the times at which to report the risk table
  information. Must be one of the eventGap inputs used for the
  estimation function. If NULL, all available are reported.

- header:

  A vector specifying the elements to include in the header. The order
  of elements matters, with the first being the topmost header. Elements
  in header can be:

  - Any of the columns returned by `tableColumns(result)` to create a
    header for these columns.

  - Any other input to create an overall header.

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

  3.  **List of custom R code:** Supply a block of custom R code or a
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

A tibble containing the risk table information (n_risk, n_events,
n_censor) for all times within the event gap specified.

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
surv <- estimateSingleEventSurvival(cdm,
                                    targetCohortTable = "mgus_diagnosis",
                                    outcomeCohortTable = "death_cohort")
#> ℹ Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#>   'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` cast to character.
tableSurvivalEvents(surv)


  


Data source
```
