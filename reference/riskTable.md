# Table with survival events

Table with survival events

## Usage

``` r
riskTable(
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

A tibble containing the risk table information (n_risk, n_events,
n_censor) for all times within the event gap specified.

## Examples

``` r
# \donttest{
cdm <- mockMGUS2cdm()
#> ■■■■■■■■■■■■■■■■■■■■■■■■■         80% | ETA:  1s
surv <- estimateSingleEventSurvival(cdm,
                                    targetCohortTable = "mgus_diagnosis",
                                    outcomeCohortTable = "death_cohort")
#> - Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#> 'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` casted to character.
riskTable(surv)
#> Warning: ! `riskTable()` has been renamed to `tableSurvivalEvents()`.
#> ℹ The current function name will be deprecated in a future version.
#> ℹ Please use `tableSurvivalEvents()` instead.


  


CDM name
```

Target cohort

Outcome name

Time

Event gap

Estimate name

Number at risk

Number events

Number censored

mock

mgus_diagnosis

death_cohort

0

30

1,384

0

0

30

30

1,104

285

3

60

30

895

182

27

90

30

652

167

79

120

30

438

131

74

150

30

299

86

54

180

30

187

57

54

210

30

109

20

58

240

30

61

15

33

270

30

31

11

18

300

30

16

4

10

330

30

7

3

6

360

30

3

1

3

390

30

2

0

1

420

30

1

0

1

424

30

1

1

0

\# }
