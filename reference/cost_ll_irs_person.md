# Cost long lasting IRS

Cost long lasting IRS

## Usage

``` r
cost_ll_irs_person(
  n_protected,
  cost_per_person_protected = 7.44,
  input_year = 2020,
  ...
)
```

## Arguments

- n_protected:

  Number of people protected. Numeric scalar or vector.

- cost_per_person_protected:

  Cost per person protected. Numeric scalar or vector.

- input_year:

  Year the unit costs are reported in

- ...:

  Additional arguments passed to \`inflation_adjust()\`

## Value

Long lasting IRS costs

## References

**cost_per_person_protected**

Current default is the (unweighted) average economic cost for long
lasting (Oganophosphate/Neonicotinoid/Neonicotinoid/pyrethroid mix) IRS
from PMI reports for: Uganda, Zambia, Ethiopia, Tanzania, Kenya,
Mozambique, Benin, Ghana, Rwanda, Madagascar, Burkina Faso, Senegal,
Zimbabwe, Mali Malawi and Cote d'Ivoire

PMI IRS Country Programs: 2020, Comparative Cost Analysis, table CC2

<https://www.pmi.gov/pmi-vectorlink-cost-study-report_2020_approved-june-14-2021-sxf-508/>.
