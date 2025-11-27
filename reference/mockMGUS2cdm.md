# Create mock CDM reference with survival::mgus2 dataset

Create mock CDM reference with survival::mgus2 dataset

## Usage

``` r
mockMGUS2cdm()
```

## Value

CDM reference containing data from the survival::mgus2 dataset

## Examples

``` r
 # \donttest{
cdm <- mockMGUS2cdm()
cdm$person
#> # Source:   table<person> [?? x 7]
#> # Database: DuckDB 1.4.2 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#>    person_id gender_concept_id year_of_birth month_of_birth day_of_birth
#>        <int>             <int>         <int>          <int>        <int>
#>  1         1              8532          1980             10            5
#>  2         2              8532          1967             10           15
#>  3         3              8507          1979              9           29
#>  4         4              8507          1976             10           25
#>  5         5              8532          1972             10            3
#>  6         6              8507          1989             10            3
#>  7         7              8532          1973             10            4
#>  8         8              8532          1973             10            6
#>  9         9              8532          1993             10            7
#> 10        10              8532          1980             10           14
#> # ℹ more rows
#> # ℹ 2 more variables: race_concept_id <int>, ethnicity_concept_id <int>
# }
```
