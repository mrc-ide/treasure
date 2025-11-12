# Number of vaccine doses delivered

Note, if duration for full course (primary + boosters) is \>1 year,
costs for the full course will be all assigned to the year of the
primary series.

## Usage

``` r
commodity_doses_vaccine(
  vaccine_cov,
  par_vaccine,
  n_dose_primary_series = 3,
  booster_coverage_downscale = 0.8,
  n_boosters = 1
)
```

## Arguments

- vaccine_cov:

  Vaccine coverage. Numeric scalar or vector.

- par_vaccine:

  Population at risk within vaccine-eligible age range. Numeric scalar
  or vector.

- n_dose_primary_series:

  Number of doses in the primary series. Numeric scalar or vector.

- booster_coverage_downscale:

  Capture the drop off in coverage between primary series and boosters
  such that \`booster coverage = vaccine_cov \*
  booster_coverage_downscale\`. Numeric scalar or vector.

- n_boosters:

  Number of booster doses. Numeric scalar or vector.

## Value

The total number of vaccine doses delivered.
