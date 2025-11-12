# Estimate total number of Chloroquine doses required

Note the cost per dose is for a single dose (250 mg chloroquine
phosphate). A treatment course typically constitutes 41mg/kg (max
2500mg) for adults

- 5 to \<15 kg: 2 doses

- 15 to \<25 kg: 4 doses

- 25 to \<35 kg: 5 doses

- \>=35 kg: 10 doses

So course for a single adult may constitute approximately 10 doses.

## Usage

``` r
commodity_chloroquine_doses(
  n_cases,
  treatment_coverage,
  proportion_non_act,
  age_upper
)
```

## Arguments

- n_cases:

  Malaria case numbers by age band. Numeric scalar or vector.

- treatment_coverage:

  Treatment coverage proportions. Numeric scalar or vector.

- proportion_non_act:

  Proportion of treatments that are non-ACT (assumed chloroquine).
  Numeric scalar or vector.

- age_upper:

  Upper bounds for each age group. Numeric scalar or vector.

## Value

A vector giving the number of 250mg chloroquine doses required per age
group.
