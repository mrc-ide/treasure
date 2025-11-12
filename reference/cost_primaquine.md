# Cost primaquine treatment

Note the cost per dose is for a single dose (7.5 mg). A treatment course
typically constitutes primaquine 0.25 mg base/kg bw per day for 14
days#' So course for a single adult (weighing 50kg) may constitute 14
days x 0.25mg x 50kg (14 x 0.25 x 50 / 7.5) = 25 doses.

## Usage

``` r
cost_primaquine(n_doses, cost_per_dose = 0.4, input_year = 2022, ...)
```

## Arguments

- n_doses:

  Number of tests. Numeric scalar or vector.

- cost_per_dose:

  Cost per dose is for a single dose (7.5 mg). Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

primaquine costs

## References

**dosing information**

<https://www.who.int/publications/i/item/guidelines-for-malaria>

**cost_per_dose**

Current default is the average cost for 7.5mg.

The Global Fund Pooled Procurement Mechanism Reference Pricing:
Antimalarial medicines, version: quarter 1, 2022

<https://www.theglobalfund.org/en/sourcing-management/health-products/antimalarial-medicines/>.
