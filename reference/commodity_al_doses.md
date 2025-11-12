# Estimate total number of AL doses required

Note the cost per dose is for a single dose (20/120 mg). A treatment
course typically constitutes Artemether + lumefantrine given twice a day
for 3 days following weight-based guidelines:

- 5 to \<15 kg: 20/120 mg

- 15 to \<25 kg: 40/240 mg

- 25 to \<35 kg: 60/360 mg

- \>=35 kg: 80/480 mg

So course for a single adult (weighing \>=35kg) may constitute 3 days x
2 times daily x 4 doses (4 x 20/120mg = 80/480mg) = 24 doses.

## Usage

``` r
commodity_al_doses(n_cases, treatment_coverage, proportion_act, age_upper)
```

## Arguments

- n_cases:

  Malaria case numbers by age band. Numeric scalar or vector.

- treatment_coverage:

  Treatment coverage proportions by age band. Numeric scalar or vector.

- proportion_act:

  Proportion of treatments that are ACTs by age band. Numeric scalar or
  vector.

- age_upper:

  Upper bounds for each age group. Numeric scalar or vector.

## Value

A vector giving the number of 20/120mg Artemether + lumefantrine ACT
doses required per age group.
