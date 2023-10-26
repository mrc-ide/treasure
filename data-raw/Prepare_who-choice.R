# Preparation of raw extracted WHO-choice patient cost data --------------------

raw_who_choice <- read.csv("data-raw/who-choice.csv")

who_choice <- raw_who_choice |>
  dplyr::mutate(
    iso3c = countrycode::countrycode(region_country, "country.name", "iso3c"),
    type = ifelse(grepl("inpatient", definition), "inpatient", "outpatient")
  ) |>
  dplyr::select(
    c("region_country", "iso3c", "type", "level", dplyr::everything())
  )

usethis::use_data(who_choice, overwrite = TRUE)
