# Inflate cost to a target year using SSA CPI

Adjust a given cost from its original year to a specified target year
using the Consumer Price Index (CPI) values from the \`cpi\` dataset.
Note this approach uses region median CPI estimates. For a more correct
approach at the country level see
<https://linkinghub.elsevier.com/retrieve/pii/S1098-3015(19)32149-7>

## Usage

``` r
inflation_adjust(
  cost,
  cost_year,
  target_year = NULL,
  region = "Sub-Saharan Africa",
  adjust = TRUE
)
```

## Arguments

- cost:

  Numeric value of the original cost.

- cost_year:

  Integer indicating the year corresponding to \`cost\`.

- target_year:

  Integer indicating the year to adjust the cost to. If not supplied,
  the value of \`getOption("treasure.target_year")\` is used.

- region:

  World region. If not supplied, the value of
  \`getOption("treasure.region")\` is used.

- adjust:

  Logical indicator if inflation adjustment should be made. Default is
  true.

## Value

Numeric value of the inflation-adjusted cost in \`target_year\` dollars.

## Details

The \`cpi\` dataset must be included in the package and contain
columns: - \`year\`: Calendar year. - \`cpi\`: CPI index value for that
year.

This function is vectorised over all arguments.

## Examples

``` r
# Adjust $0.26 from 2007 to 2024
inflation_adjust(0.26, 2007, 2024)
#> [1] 0.6746455
```
