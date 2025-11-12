# Estimate total number of microscopy diagnostics required

Estimate total number of microscopy diagnostics required

## Usage

``` r
commodity_microscopy_tests(
  n_cases,
  treatment_coverage,
  proportion_microscopy,
  proportion_tested = 1
)
```

## Arguments

- n_cases:

  Malaria case numbers. Numeric scalar or vector.

- treatment_coverage:

  Treatment coverage. Numeric scalar or vector.

- proportion_microscopy:

  Proportion of diagnostics that are microscopy. Numeric scalar or
  vector.

- proportion_tested:

  Proportion of treated cases that are tested. Numeric scalar or vector.

## Value

Vector of the number of microscopy tests
