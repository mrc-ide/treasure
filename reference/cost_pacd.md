# Cost pACD

Cost for proactive case detection.

## Usage

``` r
cost_pacd(n_tested, cost_per_person_tested = 4.79, input_year = 2015, ...)
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

Silumbe et al (2015)

<https://malariajournal.biomedcentral.com/articles/10.1186/s12936-015-0722-3>.
