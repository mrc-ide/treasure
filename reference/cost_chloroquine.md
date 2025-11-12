# Cost Chloroquine treatment

Cost Chloroquine treatment

## Usage

``` r
cost_chloroquine(n_doses, cost_per_dose = 0.1/10, input_year = 2003, ...)
```

## Arguments

- n_doses:

  Number of doses. Numeric scalar or vector.

- cost_per_dose:

  Cost per dose is for a single dose (250mg base each). Numeric scalar
  or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

Chloroquine costs

## References

**cost_per_dose**

Assumes a full adult course is ~10 tablets of 250mg chloroquine base,
and costs \$0.10 total

<https://www.msf.org/qa-act-now-get-malaria-treatment-works-africa>.
