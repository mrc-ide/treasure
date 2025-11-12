# Cost RDTs

RDTs are used for diagnosis of malaria. When costing it is also common
to add additional costs for RDTs used to diagnose non-malaria fevers.

## Usage

``` r
cost_rdt(
  n_tests,
  rdt_unit_cost = 0.46,
  delivery_mark_up = 0.15,
  input_year = 2022,
  ...
)
```

## Arguments

- n_tests:

  Number of tests. Numeric scalar or vector.

- rdt_unit_cost:

  Unit cost for rapid diagnostic test. Numeric scalar or vector.

- delivery_mark_up:

  A mark up for in-country delivery to a public health facility. Numeric
  scalar or vector. Expressed as a proportion of the test unit cost.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

RDT costs

## References

**rdt_unit_cost**

Current default is the average cost for an RDT from Normal orders,
Reference price per test EXW.

The Global Fund Pooled Procurement Mechanism Reference Pricing: RDTs,
version: quarter 1, 2022

<https://www.theglobalfund.org/en/sourcing-management/health-products/hiv-malaria-rapid-diagnostic-tests/>.

**delivery_mark_up**

Assummed 15

Patouillard et al (2017)

<https://gh.bmj.com/content/2/2/e000176>
