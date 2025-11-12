# Cost pyrethroid-PBO ITN

Cost pyrethroid-PBO ITN

## Usage

``` r
cost_pbo_itn(
  n_pbo_itn,
  pbo_itn_unit_cost = 2.63,
  pbo_itn_delivery_cost = 1.5,
  input_year = 2024,
  ...
)
```

## Arguments

- n_pbo_itn:

  Number of pyrethroid-PBO bed nets. Numeric scalar or vector.

- pbo_itn_unit_cost:

  Commodity unit cost per pyrethroid-PBO ITN bed net. Numeric scalar or
  vector.

- pbo_itn_delivery_cost:

  Cost to deliver one pyrethroid-PBO ITN bet net. Numeric scalar or
  vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

LLIN costs

## References

**pbo_itn_unit_cost**

Current default is the average cost for a pyrethroid-PBO ITN, including,
hooks, strings, bag and customisation.

The Global Fund Pooled Procurement Mechanism Reference Pricing:
Insecticide-Treated Nets, accessed 16-12-2024

<https://www.theglobalfund.org/en/sourcing-management/health-products/long-lasting-insecticidal-nets/>.

**pbo_itn_delivery_cost**

Sherrard-Smith et al (2022)

<https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00296-5/fulltext>.
