# Cost DDT IRS

Cost DDT IRS

## Usage

``` r
cost_ddt_irs_structure(
  n_sprayed,
  cost_per_structure_sprayed = 2.25,
  input_year = 1999,
  ...
)
```

## Arguments

- n_sprayed:

  Number of structures sprayed. Numeric scalar or vector.

- cost_per_structure_sprayed:

  Cost per structure sprayed. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

DDT IRS costs

## References

**cost_per_structure_sprayed**

Current default is the (mid point) average cost for DDT IRS from Table 3
of Walker (2008)

<https://resjournals.onlinelibrary.wiley.com/doi/10.1046/j.1365-2915.2000.00262.x>.
