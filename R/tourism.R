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

tourism_sol1 <- function(austa) {
  austa |>
    stretch_tsibble(.step = 12, .init = 240) |>
    model(
      ets = ETS(Visitors),
      arima = ARIMA(Visitors)
    ) |>
    forecast(h = 12)
}


tourism_sol2 <- function(austa) {
  austa_stretch <- austa |>
    mutate(
      covid = as.numeric(Month >= yearmonth("2020 Mar")),
      recovery = Month >= yearmonth("2022 Nov"),
      recovery = recovery * (Month - yearmonth("2022 Oct"))
    ) |>
    stretch_tsibble(.step = 12, .init = 240)
  # Fit models using none, one or two covariates
  fit1 <- austa_stretch |>
    model(arima1 = ARIMA(Visitors))
  fit2 <- austa_stretch |>
    model(arima2 = ARIMA(Visitors ~ covid)) |> 
    suppressWarnings()
  fit3 <- austa_stretch |>
    model(arima3 = ARIMA(Visitors ~ covid + recovery)) |> 
    suppressWarnings()
  # Choose final model for each series
  fit <- fit1 |>
    left_join(fit2, by = ".id") |>
    left_join(fit3, by = ".id") |>
    mutate(
      arima = if_else(!is_null_model(arima3), arima3,
        if_else(!is_null_model(arima2), arima2, arima1)
      )
    ) |>
    select(.id, arima)
  # Create fable
  bind_rows(
    fit |> filter(.id == 1) |> tourism_sol2_fc(h = 12),
    fit |> filter(.id == 2) |> tourism_sol2_fc(h = 12),
    fit |> filter(.id == 3) |> tourism_sol2_fc(h = 12),
    fit |> filter(.id == 4) |> tourism_sol2_fc(h = 12),
  ) |>
    as_fable(
      index = Month, key = c(.id, .model), response = "Visitors",
      distribution = Visitors
    )
}

tourism_sol2_fc <- function(fit, h = h) {
  training_data <- fit$arima[[1]]$data
  id <- fit$.id[1]
  newd <- new_data(training_data, n = h) |>
    mutate(
      covid = as.numeric(Month >= yearmonth("2020 Mar")),
      recovery = Month >= yearmonth("2022 Nov"),
      recovery = recovery * (Month - yearmonth("2022 Oct"))
    )
  fit |>
    select(-.id) |>
    forecast(new_data = newd) |>
    mutate(.id = id)
}

tourism_sol3 <- function(austa) {
  austa |>
    stretch_tsibble(.step = 12, .init = 240) |>
    mutate(
      Visitors = if_else(Month >= yearmonth("2020 Mar") & Month <= yearmonth("2022 Nov"),
        NA_real_, Visitors
      )
    ) |>
    model(
      arima = ARIMA(Visitors)
    ) |>
    forecast(h = 12)
}

tourism_sol4 <- function(austa) {
  # Replace Mar 2020 - Nov 2022 with average of last three years
  ave_3y <- austa |> 
    filter(Month >= yearmonth("2020 March") - 3*12,
           Month <= yearmonth("2020 Feb")) |> 
    mutate(month = month(Month)) |> 
    as_tibble() |> 
    summarise(ave = mean(Visitors), .by = "month")
  austa <- austa |> 
    mutate(month = month(Month)) |> 
    left_join(ave_3y, by="month") |> 
    mutate(
      Vis = if_else(Month < yearmonth("2022 Nov") &
                         Month > yearmonth("2020 Feb"),
          ave, Visitors),
    )
  austa |> 
    stretch_tsibble(.step = 12, .init = 240) |>
    model(
      ets = ETS(Visitors),
      arima = ARIMA(Visitors)
    ) |>
    forecast(h = 12)
}
