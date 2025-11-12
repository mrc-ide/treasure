# Cost per outpatient visit

For information on country specific outpatient costs from WHO CHOICE see
`?who_coice`

## Usage

``` r
cost_outpatient(n_visits, cost_per_visit, input_year = 2021, ...)
```

## Arguments

- n_visits:

  Number of visits. Numeric scalar or vector.

- cost_per_visit:

  Cost per visit. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

Outpatient costs
