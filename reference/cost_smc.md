# Cost SMC

Cost SMC

## Usage

``` r
cost_smc(n_doses, smc_cost_per_dose_delivered = 0.9075, input_year = 2016, ...)
```

## Arguments

- n_doses:

  Number of SMC doses. Numeric scalar or vector.

- smc_cost_per_dose_delivered:

  Cost per dose delivered. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

SMC costs

## References

**smc_cost_per_dose_delivered**

Current default is the average economic cost of administering four
monthly SMC cycles of sulfadoxine–pyrimethamine plus amodiaquine for
children younger than 5 years across seven countries in the Sahel
subregion (Burkina Faso, Chad, Guinea, Mali, Niger, Nigeria, and The
Gambia) in 2016. These include multiple delivery methods (door to door,
fixed point, mobile point, mixed). The cost per dose delivered is the
average cost of four monthly SMC cycles divided by the number of cycles
(3.63 / 4). Costs are in 2016 USD\$.

Gilmartin et al (2021)

<https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(20)30475-7/fulltext>.
