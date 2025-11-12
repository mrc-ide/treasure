# Number of structure-rounds of IRS

Assumes 1 structure per household

## Usage

``` r
commodity_structure_rounds_irs(irs_cov, n_rounds, par, hh_size)
```

## Arguments

- irs_cov:

  IRS coverage per year. Numeric scalar or vector.

- n_rounds:

  Number of spray rounds per year. Numeric scalar or vector.

- par:

  Population at risk estimates. Numeric scalar or vector.

- hh_size:

  Average number of occupants per household. Numeric scalar or vector.

## Value

The total number of structure-rounds of IRS protection.
