create_tsibble <- function() {
  # Read data
  attendant <- readr::read_csv(here::here("data/attendent_incident.csv"))
  verified <- readr::read_csv(here::here("data/verified_incident.csv"))

  # Create tsibble
  left_join(
    attendant |>
      pivot_longer(-date, names_to = "Area", values_to = "Attended"),
    verified |>
      pivot_longer(-date, names_to = "Area", values_to = "Verified"),
    by = c("date", "Area")
  ) |>
    rename(Date = date) |>
    as_tsibble(index = Date, key = Area)
}
