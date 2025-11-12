# Cost rACD

Cost for reactive case detection.

## Usage

``` r
cost_racd(n_tested, cost_per_person_tested = 38.63, input_year = 2016, ...)
```

## Arguments

- n_tested:

  Number of people tested. Numeric scalar or vector.

- cost_per_person_tested:

  Cost per person tested. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

pACD costs

## References

**cost_per_person_tested**

Larson et al (2016)

<https://malariajournal.biomedcentral.com/articles/10.1186/s12936-016-1457-5>.
