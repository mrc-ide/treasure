# Cost per inpatient visit

For information on country specific inpatient costs from WHO CHOICE see
`?who_coice`

## Usage

``` r
cost_inpatient(
  n_visits,
  cost_per_day,
  average_stay_duration = 3,
  input_year = 2021,
  ...
)
```

## Arguments

- n_visits:

  Number of visits. Numeric scalar or vector.

- cost_per_day:

  Cost per day. Numeric scalar or vector.

- average_stay_duration:

  Average duration of stay, defaults to 3 days following Patouillard et
  al 2017. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

Inpatient costs
