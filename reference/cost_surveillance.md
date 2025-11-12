# Cost surveillance

Cost of epidemiological and entomological surveillance.

## Usage

``` r
cost_surveillance(
  pop_at_risk,
  cost_per_pop_at_risk = 0.05,
  input_year = 2017,
  ...
)
```

## Arguments

- pop_at_risk:

  Population at risk. Numeric scalar or vector.

- cost_per_pop_at_risk:

  Cost per population at risk. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

Surveillance costs

## References

**cost_per_pop_at_risk**

Estimate from Patouillard et al (2017), consisting of a summary from the
strategic plans from: Botswana, Nigeria, India, Eritrea, Swaziland,
Namibia.

<https://gh.bmj.com/content/2/2/e000176>.
