# Set the global target year for inflation adjustments

Note, price year adjustment refers to the process of bringing all costs
to a common baseline year to allow for valid comparisons. This involves
adjusting for past inflation using historical consumer price indices or
deflators. For example, a cost reported in 2017 USD might be inflated to
2024 USD using appropriate indices. This is supported by the package and
ensures consistency across input data sources. Future inflation
assumptions, by contrast, involve projecting current prices into future
years based on assumed rates of inflation (e.g. 2 future costs or
budgets over time. This package does not support forward-looking
inflation projections; users wishing to apply such assumptions should do
so outside the package workflow.

## Usage

``` r
set_target_year(year)
```

## Arguments

- year:

  Target year to use when adjusting for inflation
