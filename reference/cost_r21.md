# Cost R21

Cost R21

## Usage

``` r
cost_r21(
  n_doses,
  r21_cost_per_dose = 4,
  r21_consumables_cost = 3.8,
  r21_delivery_cost = 1.48,
  input_year = 2024,
  ...
)
```

## Arguments

- n_doses:

  Number of R21 doses. Numeric scalar or vector.

- r21_cost_per_dose:

  Cost per R21 dose. Numeric scalar or vector.

- r21_consumables_cost:

  Cost for consumables for one dose (e.g injection and reconstitution
  syringes, safety box etc.). Numeric scalar or vector.

- r21_delivery_cost:

  Cost for delivery of one dose. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

R21 costs

## References

**r21_cost_per_dose**

Current default is based on the \$4 per dose as used in
<https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(23)00816-2/fulltext>

Penny et al (2016)
<https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(15)00725-4/fulltext>.

**r21_consumables_cost**

Penny et al (2016)
<https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(15)00725-4/fulltext>.
Pre-inflated so year is the same as dose cost: inflation_adjust(2.52,
2016, 2024) = 3.8

**r21_delivery_cost**

Following methodology summarising available information from trials and
MVIP in SI of
<https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(23)00816-2/fulltext>

Delivery cost per dose: Age-based: \$1.48 (default) Seasonal: \$3.75
Hybrid: \$2.36
