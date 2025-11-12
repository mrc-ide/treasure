# Cost standard LLINS

Cost standard LLINS

## Usage

``` r
cost_llin(
  n_llin,
  llin_unit_cost = 2.02,
  llin_delivery_cost = 1.5,
  input_year = 2024,
  ...
)
```

## Arguments

- n_llin:

  Number of standard LLIN bed nets. Numeric scalar or vector.

- llin_unit_cost:

  Commodity unit cost per standard LLIN bed net. Numeric scalar or
  vector.

- llin_delivery_cost:

  Cost to deliver one standard LLIN bet net. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

LLIN costs

## References

**llin_unit_cost**

Current default is the average cost for a pyrethroid-only ITN,
including, hooks, strings, bag and customisation.

The Global Fund Pooled Procurement Mechanism Reference Pricing:
Insecticide-Treated Nets, accessed 16-12-2024

<https://www.theglobalfund.org/en/sourcing-management/health-products/long-lasting-insecticidal-nets/>.

**llin_delivery_cost**

Sherrard-Smith et al (2022)

<https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00296-5/fulltext>.
