# Estimate total number of RDT diagnostics required

Estimate total number of RDT diagnostics required

## Usage

``` r
commodity_rdt_tests(
  n_cases,
  treatment_coverage,
  proportion_rdt,
  proportion_tested = 1
)
```

## Arguments

- n_cases:

  Malaria case numbers. Numeric scalar or vector.

- treatment_coverage:

  Treatment coverage. Numeric scalar or vector.

- proportion_rdt:

  Proportion of diagnostics that are RDT. Numeric scalar or vector.

- proportion_tested:

  Proportion of treated cases that are tested. Numeric scalar or vector.

## Value

Vector of the number of RDT tests
