# Cost IPTp

Cost IPTp

## Usage

``` r
cost_iptp(
  n_administrations,
  iptp_cost_per_administration = 0.79,
  input_year = 2012,
  ...
)
```

## Arguments

- n_administrations:

  Number of IPTp doses. Numeric scalar or vector.

- iptp_cost_per_administration:

  Cost per dose delivered. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

IPTp costs

## References

**Dosing information**

Starting as early as possible in the second trimester, IPTp-SP is
recommended for all pregnant women at each scheduled antenatal care
(ANC) visit until the time of delivery, provided that the doses are
given at least one month apart.

IPTp-SP should ideally be administered as directly observed therapy
(DOT) of three tablets sulfadoxine/pyrimethamine (each tablet
containing500 mg/25 mg SP) giving the total required dosage of 1500
mg/75 mg SP.

<https://www.who.int/publications-detail-redirect/WHO-HTM-GMP-2014.4>

**IPTp_cost_per_dose_delivered**

Current default is the total cost per administration of IPTp-SP (health
care worker time and commodity cost) with sulfadoxine–pyrimethamine.
Costs are in 2012 USD\$.

Fernandes et al (2016). Table 2.

<https://malariajournal.biomedcentral.com/articles/10.1186/s12936-016-1539-4>
