read_tourism <- function(file) {
  readxl::read_excel(file, sheet = "Data1", skip = 9) |>
    rename(date = `Series ID`, value = A85375847A) |>
    select(date, value) |>
    transmute(
      Month = yearmonth(date),
      Visitors = value / 1e3
    ) |>
    as_tsibble(index = Month) |>
    filter(Month >= yearmonth("2000 Jan"))
}

tourism_plot <- function(austa) {
  austa |>
    as_tibble() |>
    mutate(Month = as.Date(Month)) |>
    ggplot(aes(x = Month, y = Visitors)) +
    geom_line() +
    labs(
      x = "Month",
      y = "Thousands of visitors",
      title = "Total short-term visitors to Australia"
    ) +
    scale_x_date(
      breaks = seq(as.Date("2000-01-01"), by = "5 years", l = 5),
      labels = paste("Jan", seq(2000, 2020, by = 5)),
      minor_breaks = seq(as.Date("2001-01-01"), by = "1 year", l = 25)
    ) 
}

