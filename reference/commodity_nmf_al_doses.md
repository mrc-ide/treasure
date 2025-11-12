# Estimate total number of AL doses required for test +ve non-malarial fevers

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
commodity_nmf_al_doses(
  n_nmf,
  treatment_coverage,
  proportion_act,
  age_upper,
  pfpr,
  pfpr_threshold = 0.05
)
```

## Arguments

- n_nmf:

  Non malarial fever case numbers by age band. Numeric scalar or vector.

- treatment_coverage:

  Treatment coverage proportions. Numeric scalar or vector.

- proportion_act:

  Proportion of treatments that are ACTs. Numeric scalar or vector.

- age_upper:

  Upper bounds for each age group. Numeric scalar or vector.

- pfpr:

  Prevalence. Numeric scalar or vector.

- pfpr_threshold:

  Prevalence threshold at which it is assumed NMF are not suspected (and
  subsequently tested) to be malaria. Numeric scalar or vector.

## Value

A vector giving the number of 20/120mg Artemether + lumefantrine ACT
doses required per age group.
