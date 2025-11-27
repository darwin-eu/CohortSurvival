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

A tibble containing the risk table information (n_risk, n_events,
n_censor) for all times within the event gap specified.

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
riskTable(surv)


  


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
