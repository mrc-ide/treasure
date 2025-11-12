# Cost PMC

Cost PMC

## Usage

``` r
cost_pmc(n_doses, pmc_cost_per_dose_delivered = 0.3894, input_year = 2007, ...)
```

## Arguments

- n_doses:

  Number of PMC doses. Numeric scalar or vector.

- pmc_cost_per_dose_delivered:

  Cost per dose delivered. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

PMC costs

## References

**PMC_cost_per_dose_delivered**

Current default is the average economic cost of administering 3 rounds
of PMC (annually), of sulfadoxine–pyrimethamine in Tanzania, Ghana,
Mozambique and Gabon. The cost per dose delivered is the average cost of
trial results for three PMC cycles divided by the number of cycles (0.39
/ 3). Costs are in 2007 USD\$. Cost have been inflated to adjust for a
roughly 3 fold increase in SP costs (GF price reference data)

Conteh et al (2010) table S4

<https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0010313>.
