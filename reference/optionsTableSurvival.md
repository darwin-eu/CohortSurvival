# Additional arguments for the function tableSurvival()

It provides a list of allowed inputs for .option argument in
tableSurvival and their given default value.

## Usage

``` r
optionsTableSurvival()
```

## Value

The default .options named list.

## Examples

``` r
{
optionsTableSurvival()
}
#> $decimals
#>    integer percentage    numeric proportion 
#>          0          2          2          2 
#> 
#> $decimalMark
#> [1] "."
#> 
#> $bigMark
#> [1] ","
#> 
#> $delim
#> [1] "\n"
#> 
#> $includeHeaderName
#> [1] TRUE
#> 
#> $includeHeaderKey
#> [1] TRUE
#> 
#> $na
#> [1] "–"
#> 
#> $title
#> NULL
#> 
#> $subtitle
#> NULL
#> 
#> $caption
#> NULL
#> 
#> $groupAsColumn
#> [1] FALSE
#> 
#> $groupOrder
#> NULL
#> 
#> $merge
#> [1] "all_columns"
#> 

```
