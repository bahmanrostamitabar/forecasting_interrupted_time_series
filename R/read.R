create_tsibble <- function(attendant, verified) {
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
