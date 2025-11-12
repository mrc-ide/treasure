# Estimate the number of bed nets required to match usage target

Estimate the number of bed nets required to match usage target

## Usage

``` r
commodity_nets(
  usage,
  use_rate,
  distribution_timesteps,
  crop_timesteps,
  half_life,
  par,
  ...
)
```

## Arguments

- usage:

  Desired target usages (%) to model. Numeric scalar or vector.

- use_rate:

  Usage rates. Numeric scalar or vector.

- distribution_timesteps:

  Distribution timesteps (days). Numeric scalar or vector. By default,
  we can assume that net distributions happen on the first day of each
  year. For example c(1, 366)

- crop_timesteps:

  Crop estimate timesteps (days). Numeric scalar or vector. If assuming
  distributions occur on the first day of each year, a reasonable
  assumption would be that the crop (and therefore corresponding usage)
  estimates were taken at the mid-point of each year. For example
  c(1, 366) + 183.

- half_life:

  Country-specific net half-life in days. Numeric scalar or vector.

- par:

  Population at risk estimates. Numeric scalar or vector.

- ...:

  additional arguments for the crop_to_distribution function in netz

## Value

Number of nets required to match usage targets

## References

reference Uses a version of the net stock and flow model as described
by: Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021):
1-12.
