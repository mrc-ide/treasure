# Preparation of raw extracted World Bank SSA consumer price index -------------

# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=ZG

cpi_raw <- read.csv("data-raw/World_bank_CPI.csv")

cpi <- cpi_raw |>
  dplyr::rename(
    country = "Country.Name",
    iso3c = "Country.Code"
  ) |>
  tidyr::pivot_longer(
    cols = -c(country, iso3c), names_to = "year",
                      values_to = "cpi", names_prefix = "X", names_transform = list(year = "as.integer")
    ) |>
  dplyr::mutate(region = countrycode::countrycode(iso3c, "iso3c", "region")) |>
  dplyr::summarise(
    cpi = median(cpi, na.rm = TRUE),
    .by = c("year", "region")
  ) |>
  dplyr::filter(!is.na(region), !is.na(cpi))

usethis::use_data(cpi, overwrite = TRUE)
