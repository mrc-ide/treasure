# Cost pyrethroid-chlorfenapyr (dual ai) ITN

Cost pyrethroid-chlorfenapyr (dual ai) ITN

## Usage

``` r
cost_dualai_itn(
  n_dualai_itn,
  dualai_itn_unit_cost = 2.7,
  dualai_itn_delivery_cost = 1.5,
  input_year = 2024,
  ...
)
```

## Arguments

- n_dualai_itn:

  Number of pyrethroid-chlorfenapyr bed nets. Numeric scalar or vector.

- dualai_itn_unit_cost:

  Commodity unit cost per pyrethroid-chlorfenapyr ITN bed net. Numeric
  scalar or vector.

- dualai_itn_delivery_cost:

  Cost to deliver one pyrethroid-chlorfenapyr ITN bet net. Numeric
  scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

LLIN costs

## References

**dualai_itn_unit_cost**

Current default is the average cost for a pyrethroid-chlorfenapyr ITN,
including, hooks, strings, bag and customisation.

The Global Fund Pooled Procurement Mechanism Reference Pricing:
Insecticide-Treated Nets, accessed 16-12-2024

<https://www.theglobalfund.org/en/sourcing-management/health-products/long-lasting-insecticidal-nets/>.

**dualai_itn_delivery_cost**

Sherrard-Smith et al (2022)

<https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00296-5/fulltext>.
