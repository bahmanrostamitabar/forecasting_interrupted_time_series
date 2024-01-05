# Read data from API
fetch_walkers <- function(start = "2019-01-01", end = "2023-12-31") {
  # Download hourly data from start to end
  walkers <- rwalkr::melb_walk(from = as.Date(start), to = as.Date(end)) 
}

clean_walkers <- function(walkers) {
  # Remove first values of each day which appear to be sensor identifiers
  walkers <- walkers |> 
    arrange(Sensor, Date, Time) |> 
    group_by(Sensor, Date) |> 
    mutate(obs = row_number()-1) |> 
    filter(obs > 0) |> 
    select(-obs) |> 
    mutate(Count = readr::parse_integer(Count))

  # Find sensors that are used throughout the whole period
  sensors_to_use <- walkers |> 
    as_tsibble(index = Date_Time, key = Sensor) |> 
    fill_gaps(.full = TRUE) |> 
    as_tibble() |> 
    group_by(Sensor) |> 
    summarise(nmiss = sum(is.na(Count))) |> 
    filter(nmiss < 30*24) |> 
    pull(Sensor) 
  
  # Compute daily totals by sensor
  walkers |>
    filter(Sensor %in% sensors_to_use) |> 
    group_by(Date, Sensor) |>
    summarise(Count = sum(Count), .groups = "drop") |>
    as_tsibble(index = Date, key = Sensor)
}

ave_walkers <- function(walkers) {
  # Average across sensors
  walkers |>
    as_tibble() |>
    group_by(Date) |>
    summarise(Count = mean(Count, na.rm=TRUE) / 1e3) |>
    tsibble::as_tsibble(index = Date)
}

lockdown_table <- function() {
  # First lockdown officially started 31 March, but self-lockdown happened from 
  # start of previous week. Other dates below are official periods as per
  # https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Victoria#Lockdown_statistics
  # See also https://www.platinumaccounting.com.au/blog/melbourne-lockdown-dates
  tribble(
    ~Lockdown, ~Start, ~End,
    1, "2020-03-23", "2020-05-12",
    2, "2020-07-09", "2020-10-27",
    3, "2021-02-13", "2021-02-17",
    4, "2021-05-28", "2021-06-10",
    5, "2021-07-16", "2021-07-27",
    6, "2021-08-05", "2021-10-21"
  ) |> 
    mutate(Start = as.Date(Start), End = as.Date(End))
}

# Add lockdown periods to pedestrian data

add_lockdowns <- function(walkers) {
  lockdowns <- lockdown_table()  
  walkers <- walkers |>
    mutate(Lockdown_period = 0)
  for(i in seq(nrow(lockdowns))) {
    awalkers <- walkers |>
      mutate(
        Lockdown_period = if_else(Date >= lockdowns$Start[i] & Date <= lockdowns$End[i], i, Lockdown_period)
      )
  }
  walkers <- walkers |> 
    mutate(
      Lockdown = Lockdown_period > 0,
      Lockdown_period = factor(Lockdown_period)
    )
  return(walkers)
}

pedestrian_plot <- function(data) {
  lockdowns <- lockdown_table()
  # Plot of data showing lockdown periods
  ggplot(data) +
    geom_rect(data = lockdowns, xmin = lockdowns$Start, xmax = lockdowns$End,
              ymin = -Inf, ymax = Inf, fill = "grey60", alpha = 0.5) +
    geom_line(aes(x = Date, y=Count)) +
    labs(
      title = "Daily average number of pedestrians across Melbourne",
      y = "Thousands of persons"
    )
}

# Solution 1
pedestrian_sol1 <- function(walkers, step, init, h) {
  walkers |>
    stretch_tsibble(.step = step, .init = init) |>
    model(
      ets = ETS(Count),
      arima = ARIMA(Count ~ PDQ(period = "week"))
    ) |>
    forecast(h = h) 
}

# Solution 2

ped_sol2_fc <- function(id, model, h = h) {
  lockdowns <- lockdown_table()
  newd <- new_data(model$data, n = h) |>
    mutate(Lockdown = FALSE)
  for(i in seq(nrow(lockdowns))) {
    newd <- newd |>
      mutate(Lockdown = Lockdown | (Date >= lockdowns$Start[i] & Date <= lockdowns$End[i]))
  }
  forecast(model, new_data = newd) |>
    mutate(.id = id) |> 
    as_tibble()
}

# Intervention model, step functions during lockdowns
pedestrian_sol2 <- function(walkers, step, init, h) {
  walker_stretch <- walkers |>
    stretch_tsibble(.step = step, .init = init)
  # Fit models using none, one or two covariates
  fit1 <- walker_stretch |>
    model(arima1 = ARIMA(Count ~ PDQ(period = "week")))
  fit2 <- walker_stretch |>
    model(arima2 = ARIMA(Count ~ Lockdown + PDQ(period = "week"))) |>
    suppressWarnings()
  # Choose final model for each series
  fit <- left_join(fit1, fit2, by = ".id") |>
    mutate(
      arima = if_else(!is_null_model(arima2), arima2, arima1)
    ) |>
    select(.id, arima)
  # Create fable
  purrr::pmap_dfr(fit, 
      function(.id, arima) { ped_sol2_fc(.id, arima, h=h) }
    ) |>
    mutate(.model = "arima") |> 
    as_fable(
      index = Date, key = c(.id, .model), response = "Count",
      distribution = Count
    ) |>
    select(.id, .model, Date, Count, .mean)
}

# Solution 3
# Set Counts in lockdown periods to missing
pedestrian_sol3 <- function(walkers, step, init, h) {
  walkers |>
    stretch_tsibble(.step = step, .init = init) |>
    mutate(Count = if_else(Lockdown, NA_real_, Count)) |>
    model(arima = ARIMA(Count ~ PDQ(period = "week"))) |>
    forecast(h = h) 
}

# Solution 4
# Replace Lockdown periods with interpolations from an ARIMA model
pedestrian_sol4 <- function(walkers, step, init, h) {
  tmp <- walkers |>
    mutate(Count = if_else(Lockdown, NA_real_, Count)) 
  new_walkers <- tmp |> 
    model(arima = ARIMA(Count ~ PDQ(period = "week"))) |>
    interpolate(new_data = tmp) 
  new_walkers |>
    stretch_tsibble(.step = step, .init = init) |>
    model(
      ets = ETS(Count),
      arima = ARIMA(Count ~ PDQ(period = "week"))
    ) |>
    forecast(h = h) 
}
