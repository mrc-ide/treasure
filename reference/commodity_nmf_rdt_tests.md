# Estimate total number of RDT diagnostics required as a result of non malarial fevers

Estimate total number of RDT diagnostics required as a result of non
malarial fevers

## Usage

``` r
commodity_nmf_rdt_tests(
  n_nmf,
  treatment_coverage,
  proportion_rdt,
  proportion_tested = 1,
  pfpr,
  pfpr_threshold = 0.05
)
```

## Arguments

- n_nmf:

  Non malarial fever case numbers. Numeric scalar or vector.

- treatment_coverage:

  Treatment coverage. Numeric scalar or vector.

- proportion_rdt:

  Proportion of diagnostics that are RDT. Numeric scalar or vector.

- proportion_tested:

  Proportion of NMFs that are tested. Numeric scalar or vector.

- pfpr:

  Prevalence. Numeric scalar or vector.

- pfpr_threshold:

  Prevalence threshold at which it is assumed NMF are not suspected (and
  subsequently tested) to be malaria. Numeric scalar or vector.

## Value

Vector of the number of RDTs tests used on NMFs
