library(fpp3)
library(tidyverse)
library(rwalkr)

init <- 21
h <- 21

fetch_walkers <- function(start = "2017-01-01", end = "2023-12-31") {
  # Download hourly data from start to end
  walkers <- melb_walk(from = as.Date(start), to = as.Date(end)) |> 
    mutate(Count = parse_integer(Count))
  # Compute daily totals by sensor
  daily_walkers <- walkers |>
    group_by(Date, Sensor) |>
    summarise(Count = sum(Count, na.rm = TRUE)) |>
    ungroup() |>
    mutate(Count = if_else(Count == 0L, NA_integer_, Count)) |>
    as_tsibble(index = Date, key = Sensor)

  # Remove censors with large numbers of missing values
  nmiss <- daily_walkers |>
    as_tibble() |>
    group_by(Sensor) |>
    summarise(nmiss = sum(is.na(Count))) |>
    filter(nmiss < 10)

  # Interpolate missing values
  daily_walkers <- daily_walkers |>
    filter(Sensor %in% nmiss$Sensor) |>
    mutate(Count = zoo::na.approx(Count))

  # Average across sensors
  average_daily_walkers <- daily_walkers |>
    as_tibble() |>
    group_by(Date) |>
    summarise(Count = mean(Count) / 1e3) |>
    tsibble::as_tsibble(index = Date)

  return(average_daily_walkers)
}

average_daily_walkers <- fetch_walkers(end = "2017-01-30")

average_daily_walkers |>
  autoplot(Count) +
  labs(
    title = "Daily average number of pedestrians across Melbourne",
    y = "Thousands of persons"
  )

# Solution 1

fc1 <- average_daily_walkers |>
  stretch_tsibble(.step = 7, .init = 21) |>
  model(
    ets = ETS(Count),
    arima = ARIMA(sqrt(Count) ~ fourier(period = "year", K = 4) + PDQ(period = "week"))
  ) |>
  forecast(h = 14) |>
  mutate(.id = paste("Forecasts of", .id + 2019))

# Solution 2

ped_sol2_fc <- function(fit, h = h) {
  training_data <- fit$arima[[1]]$data
  id <- fit$.id[1]
  newd <- new_data(training_data, n = h) |>
    mutate(
      covid = as.numeric(Date >= as.Date("2020-03-01") &
                           Date <= as.Date("2022-10-31")),
      recovery = (Date >= as.Date("2021-10-01") &
                    Date <= as.Date("2022-10-31")),
      recovery = recovery * (Date - as.Date("2021-09-01"))
    )
  fit |>
    select(-.id) |>
    forecast(new_data = newd) |>
    mutate(.id = id) |> 
    as_tibble()
}

walker_stretch <- average_daily_walkers |>
  mutate(
    covid = as.numeric(Date >= as.Date("2020-03-01") &
      Date <= as.Date("2022-10-31")),
    recovery = (Date >= as.Date("2021-10-01") &
      Date <= as.Date("2022-10-31")),
    recovery = recovery * (Date - as.Date("2021-09-01"))
  ) |>
  stretch_tsibble(.step = 7, .init = 21)
# Fit models using none, one or two covariates
fit1 <- walker_stretch |>
  model(arima1 = ARIMA(sqrt(Count)))
fit2 <- walker_stretch |>
  model(arima2 = ARIMA(sqrt(Count) ~ covid)) |>
  suppressWarnings()
fit3 <- walker_stretch |>
  model(arima3 = ARIMA(sqrt(Count) ~ covid + recovery)) |>
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
fc <- bind_rows(
  fit |> filter(.id == 1) |> ped_sol2_fc(h = 12),
  fit |> filter(.id == 2) |> ped_sol2_fc(h = 12),
  #fit |> filter(.id == 3) |> ped_sol2_fc(h = 12),
  #fit |> filter(.id == 4) |> ped_sol2_fc(h = 12),
) |>
  as_fable(
    index = Date, key = c(.id, .model), response = "Count",
    distribution = Count
  ) |>
  mutate(.id = paste("Forecasts of", .id + 2019))

training_data <- fit$arima[[1]]$data
id <- fit$.id[1]
newd <- new_data(training_data, n = h) |>
  mutate(
    covid = as.numeric(Date >= as.Date("2020-03-01") &
                         Date <= as.Date("2022-10-31")),
    recovery = (Date >= as.Date("2021-10-01") &
                  Date <= as.Date("2022-10-31")),
    recovery = recovery * (Date - as.Date("2021-09-01"))
  )
fit |>
  select(-.id) |>
  forecast(new_data = newd) |>
  mutate(.id = id)

# Solution 3

tourism_sol3 <- function(walker) {
  walker |>
    stretch_tsibble(.step = 12, .init = 240) |>
    mutate(
      Visitors = if_else(Month >= yearmonth("2020 Mar") &
        Month <= yearmonth("2022 Oct"),
      NA_real_, Visitors
      )
    ) |>
    model(
      arima = ARIMA(sqrt(Count))
    ) |>
    forecast(h = 12) |>
    mutate(.id = paste("Forecasts of", .id + 2019))
}

tourism_sol4 <- function(walker) {
  # Replace Mar 2020 - Nov 2022 with average of last three years
  ave_3y <- walker |>
    filter(
      Month >= yearmonth("2020 March") - 3 * 12,
      Month <= yearmonth("2020 Feb")
    ) |>
    mutate(month = month(Month)) |>
    as_tibble() |>
    summarise(ave = mean(Visitors), .by = "month")
  walker <- walker |>
    mutate(month = month(Month)) |>
    left_join(ave_3y, by = "month") |>
    mutate(
      Visitors_adj = if_else(Month < yearmonth("2022 Nov") &
        Month > yearmonth("2020 Feb"),
      ave, Visitors
      ),
    )
  walker |>
    stretch_tsibble(.step = 12, .init = 240) |>
    model(
      ets = ETS(log(Visitors_adj)),
      arima = ARIMA(log(Visitors_adj))
    ) |>
    forecast(h = 12) |>
    mutate(.id = paste("Forecasts of", .id + 2019))
}
