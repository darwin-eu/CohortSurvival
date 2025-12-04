# Further survival analyses

## Set up

Let us first load the packages required.

``` r
library(CDMConnector)
library(CohortSurvival)
library(dplyr)
library(cmprsk)
library(survival)
```

We will create a cdm reference to use our example MGUS2 survival
dataset.

``` r
cdm <- CohortSurvival::mockMGUS2cdm()
```

The CohortSurvival package does not have implemented functionality to do
more complex survival analyses than Kaplar Meier curves, like Cox
Proportional Hazards modelling. However, the format the data has to be
in to be inputted to well-known modelling functions from packages like
`survival` or `cmprsk`can be retrieved from OMOP data with some in-built
functions in this package. Let us see how to do it in both single event
and competing risk survival settings.

## Further analysis with single event survival

To get the `time` and `status` information we need for the `coxph`
function in the package `survival`, for instance, we only need to call
`addCohortSurvival`. The stratification variables need to be columns
previously added to the cohort by the user.

``` r
input_survival_single <- cdm$mgus_diagnosis |>
       addCohortSurvival(
       cdm = cdm,
       outcomeCohortTable = "death_cohort",
       outcomeCohortId = 1
       ) 

input_survival_single |> 
  glimpse()
#> Rows: ??
#> Columns: 13
#> Database: DuckDB 1.4.2 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <int> 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 1…
#> $ cohort_start_date    <date> 1981-01-01, 1968-01-01, 1980-01-01, 1977-01-01, …
#> $ cohort_end_date      <date> 1981-01-01, 1968-01-01, 1980-01-01, 1977-01-01, …
#> $ age                  <dbl> 88, 78, 94, 68, 90, 90, 89, 87, 79, 86, 89, 87, 8…
#> $ sex                  <fct> F, F, M, M, F, M, F, F, F, M, F, M, F, M, F, F, M…
#> $ hgb                  <dbl> 13.1, 11.5, 10.5, 15.2, 10.7, 12.9, 10.5, 12.3, 9…
#> $ creat                <dbl> 1.30, 1.20, 1.50, 1.20, 0.80, 1.00, 0.90, 1.20, 1…
#> $ mspike               <dbl> 0.5, 2.0, 2.6, 1.2, 1.0, 0.5, 1.3, 1.6, 2.3, 2.3,…
#> $ age_group            <chr> ">=70", ">=70", ">=70", "<70", ">=70", ">=70", ">…
#> $ days_to_exit         <int> 30, 25, 46, 92, 8, 4, 151, 2, 136, 2, 108, 10, 14…
#> $ status               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ time                 <dbl> 30, 25, 46, 92, 8, 4, 151, 2, 136, 2, 108, 10, 14…
```

We can decide to change some of the default parameters in this function.
Information on all these can be found
[`?addCohortSurvival`](https://darwin-eu.github.io/CohortSurvival/reference/addCohortSurvival.md).
For instance, we can choose to exclude people with an outcome only 180
days before index date, instead of anytime, and follow them up for only
one year. We can also decide to use `cohort_end_date` as their outcome
variable and censor them at a particular date, for instance, the 1st of
January of 1994. We see how that gives us different results:

``` r
cdm$mgus_diagnosis |>
       addCohortSurvival(
       cdm = cdm,
       outcomeCohortTable = "death_cohort",
       outcomeWashout = 180,
       followUpDays = 365
       ) |>
  filter(cohort_start_date > "1993-01-01") |>
  glimpse()
#> Rows: ??
#> Columns: 13
#> Database: DuckDB 1.4.2 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <int> 213, 671, 941, 1245, 1248, 1340, 1374, 1376, 1380…
#> $ cohort_start_date    <date> 1994-01-01, 1994-01-01, 1994-01-01, 1994-01-01, …
#> $ cohort_end_date      <date> 1994-01-01, 1994-01-01, 1994-01-01, 1994-01-01, …
#> $ age                  <dbl> 93, 81, 85, 82, 87, 67, 68, 67, 69, 66, 79, 86, 8…
#> $ sex                  <fct> F, M, M, M, M, F, F, F, M, M, M, F, F, F, F, F, M…
#> $ hgb                  <dbl> 12.8, 13.0, 13.6, 11.4, 12.7, 12.2, 9.2, 13.7, 15…
#> $ creat                <dbl> 1.1, 1.4, 1.1, 1.5, 1.5, 1.4, 1.8, 1.1, 0.8, 2.0,…
#> $ mspike               <dbl> 0.8, 1.3, 1.5, 1.4, 0.5, 1.2, 0.5, 1.5, 0.0, 0.0,…
#> $ age_group            <chr> ">=70", ">=70", ">=70", ">=70", ">=70", "<70", "<…
#> $ days_to_exit         <dbl> 19, 43, 12, 1, 10, 46, 40, 41, 22, 31, 6, 57, 52,…
#> $ status               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0…
#> $ time                 <dbl> 19, 43, 12, 1, 10, 46, 40, 41, 22, 31, 6, 57, 52,…
cdm$mgus_diagnosis |>
       addCohortSurvival(
       cdm = cdm,
       outcomeCohortTable = "death_cohort",
       outcomeDateVariable = "cohort_end_date",
       censorOnDate = as.Date("1994-01-01")
       ) |>
    filter(cohort_start_date > "1993-01-01") |>
  glimpse()
#> Rows: ??
#> Columns: 13
#> Database: DuckDB 1.4.2 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <int> 213, 671, 941, 1245, 1248, 1340, 1374, 1376, 1380…
#> $ cohort_start_date    <date> 1994-01-01, 1994-01-01, 1994-01-01, 1994-01-01, …
#> $ cohort_end_date      <date> 1994-01-01, 1994-01-01, 1994-01-01, 1994-01-01, …
#> $ age                  <dbl> 93, 81, 85, 82, 87, 67, 68, 67, 69, 66, 79, 86, 8…
#> $ sex                  <fct> F, M, M, M, M, F, F, F, M, M, M, F, F, F, F, F, M…
#> $ hgb                  <dbl> 12.8, 13.0, 13.6, 11.4, 12.7, 12.2, 9.2, 13.7, 15…
#> $ creat                <dbl> 1.1, 1.4, 1.1, 1.5, 1.5, 1.4, 1.8, 1.1, 0.8, 2.0,…
#> $ mspike               <dbl> 0.8, 1.3, 1.5, 1.4, 0.5, 1.2, 0.5, 1.5, 0.0, 0.0,…
#> $ age_group            <chr> ">=70", ">=70", ">=70", ">=70", ">=70", "<70", "<…
#> $ days_to_exit         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ status               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ time                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```

This table with the added `time` and `status` information should be
enough to call any advanced function, like the aforementioned Cox
Proportional Hazards model:

``` r
survival::coxph(survival::Surv(time, status) ~ age + sex, data = input_survival_single)
#> Call:
#> survival::coxph(formula = survival::Surv(time, status) ~ age + 
#>     sex, data = input_survival_single)
#> 
#>          coef exp(coef) se(coef)      z        p
#> age  0.061622  1.063561 0.003402 18.114  < 2e-16
#> sexM 0.358258  1.430835 0.065693  5.454 4.94e-08
#> 
#> Likelihood ratio test=391.2  on 2 df, p=< 2.2e-16
#> n= 1384, number of events= 963
survival::survdiff(survival::Surv(time, status) ~ sex, data = input_survival_single)
#> Call:
#> survival::survdiff(formula = survival::Surv(time, status) ~ sex, 
#>     data = input_survival_single)
#> 
#>         N Observed Expected (O-E)^2/E (O-E)^2/V
#> sex=F 631      423      471      4.88      9.67
#> sex=M 753      540      492      4.67      9.67
#> 
#>  Chisq= 9.7  on 1 degrees of freedom, p= 0.002
```

## Further analysis with competing risk survival

For competing risk settings, we need to use the same function that adds
`time` and `status` information, but twice. We first need to add time
and status information for the outcome, then for the competing outcome.
Then we leverage all those variables to get what outcome (if any) to
count per individual so that we can feed the result to subsequent
models.

``` r

# Add all status and time information for both outcomes
  input_survival_cr <- cdm$mgus_diagnosis |>
    addCohortSurvival(cdm, "progression") |>
    dplyr::rename(
      "outcome_time" = "time",
      "outcome_status" = "status"
    ) |>
     addCohortSurvival(cdm, "death_cohort") |>
    dplyr::rename(
      "competing_outcome_time" = "time",
      "competing_outcome_status" = "status"
    )
  
  # Collect and 
  input_survival_cr <- input_survival_cr |>
    dplyr::collect() |>
    dplyr::mutate(
      time = pmin(outcome_time, competing_outcome_time),
      status = factor(
        dplyr::if_else(competing_outcome_time <= outcome_time, 2 * competing_outcome_status, outcome_status))
    ) |>
    dplyr::select(-c("outcome_time", "outcome_status", "competing_outcome_time", "competing_outcome_status"))
```

We can use the package `cmprsk` to fit a Fine and Gray model to the
competing risk data. We first change our `sex` covariate to numeric, and
then we can run the analysis:

``` r
input_survival_cr <- input_survival_cr |>
  dplyr::mutate(sex = dplyr::if_else(sex == "M", 0, 1))

covs <- data.frame(input_survival_cr$age, input_survival_cr$sex)
names(covs) <- c("age", "sex")

summary(cmprsk::crr(ftime = input_survival_cr$time,
            fstatus = input_survival_cr$status,
            cov1 = covs,
            failcode = 1,
            cencode = 0))
#> Competing Risks Regression
#> 
#> Call:
#> cmprsk::crr(ftime = input_survival_cr$time, fstatus = input_survival_cr$status, 
#>     cov1 = covs, failcode = 1, cencode = 0)
#> 
#>        coef exp(coef) se(coef)     z p-value
#> age -0.0192     0.981  0.00585 -3.28   0.001
#> sex  0.2871     1.333  0.19309  1.49   0.140
#> 
#>     exp(coef) exp(-coef)  2.5% 97.5%
#> age     0.981       1.02 0.970 0.992
#> sex     1.333       0.75 0.913 1.945
#> 
#> Num. cases = 1384
#> Pseudo Log-likelihood = -726 
#> Pseudo likelihood ratio test = 8.32  on 2 df,
```

## Disconnect from the cdm database connection

We finish by disconnecting from the cdm database connection.

``` r
cdmDisconnect(cdm)
```
