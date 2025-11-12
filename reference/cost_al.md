# Cost Artemether/Lumefantrine treatment

Note the cost per dose is for a single dose (20/120 mg), not a full
treatment course.

## Usage

``` r
cost_al(n_doses, cost_per_dose = 0.3, input_year = 2022, ...)
```

## Arguments

- n_doses:

  Number of doses. Numeric scalar or vector.

- cost_per_dose:

  Cost per dose is for a single dose (20/120 mg). Numeric scalar or
  vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

AL costs

## References

**dosing information**

<https://www.who.int/publications/i/item/guidelines-for-malaria>

**cost_per_dose**

Current default is the average cost for 20/120mg.

The Global Fund Pooled Procurement Mechanism Reference Pricing:
Antimalarial medicines, version: quarter 1, 2022

<https://www.theglobalfund.org/en/sourcing-management/health-products/antimalarial-medicines/>.
