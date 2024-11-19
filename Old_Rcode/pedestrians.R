## PEDESTRIANS EXAMPLE ----------------------------------------------------------

## Read data --------------------------------------------------------------------

# Download hourly data from start to end
if (fs::file_exists(here::here("data/raw_walkers.rds"))) {
  raw_walkers <- readRDS(here::here("data/raw_walkers.rds"))
} else {
  raw_walkers <- rwalkr::melb_walk(from = as.Date("2019-01-01"), to = as.Date("2023-12-31")) |>
    arrange(Sensor, Date, Time) |>
    group_by(Sensor, Date) |>
    mutate(obs = row_number() - 1) |>
    filter(obs > 0) |>
    select(-obs) |>
    mutate(Count = readr::parse_integer(Count))
  saveRDS(raw_walkers, here::here("data/raw_walkers.rds"))
}

## Find sensors that are used throughout the whole period ------------------------

sensors_to_use <- raw_walkers |>
  group_by(Sensor, Date_Time) |>
  summarise(Count = sum(Count), .groups = "drop") |>
  as_tsibble(index = Date_Time, key = Sensor) |>
  fill_gaps(.full = TRUE) |>
  as_tibble() |>
  group_by(Sensor) |>
  summarise(nmiss = sum(is.na(Count))) |>
  filter(nmiss < 30 * 24) |>
  pull(Sensor)

# Compute daily averages across all sensors -------------------------------------

walkers <- raw_walkers |>
  filter(Sensor %in% sensors_to_use) |>
  group_by(Date, Sensor) |>
  summarise(Count = sum(Count), .groups = "drop") |>
  group_by(Date) |>
  summarise(Count = mean(Count, na.rm = TRUE) / 1e3, .groups = "drop") |>
  tsibble::as_tsibble(index = Date)

## Lockdown periods ------------------------------------------------------------

# First lockdown officially started 31 March, but self-lockdown happened from
# start of previous week. Other dates below are official periods as per
# https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Victoria#Lockdown_statistics
# See also https://www.platinumaccounting.com.au/blog/melbourne-lockdown-dates
lockdowns <- tribble(
  ~Lockdown, ~Start, ~End,
  1, "2020-03-23", "2020-05-12",
  2, "2020-07-09", "2020-10-27",
  3, "2021-02-13", "2021-02-17",
  4, "2021-05-28", "2021-06-10",
  5, "2021-07-16", "2021-07-27",
  6, "2021-08-05", "2021-10-21"
) |>
  mutate(Start = as.Date(Start), End = as.Date(End))

# Add lockdown periods to pedestrian data ----------------------------------------

walkers <- walkers |>
  mutate(Lockdown_period = 0)
for (i in seq(nrow(lockdowns))) {
  walkers <- walkers |>
    mutate(
      Lockdown_period = if_else(Date >= lockdowns$Start[i] & Date <= lockdowns$End[i], i, Lockdown_period)
    )
}
walkers <- walkers |>
  mutate(
    Lockdown = Lockdown_period > 0,
    Lockdown_period = factor(Lockdown_period)
  )

# Plot of data showing lockdown periods ----------------------------------------

walkers |>
  ggplot() +
  geom_rect(
    data = lockdowns, xmin = lockdowns$Start, xmax = lockdowns$End,
    ymin = -Inf, ymax = Inf, fill = "grey60", alpha = 0.5
  ) +
  geom_line(aes(x = Date, y = Count)) +
  labs(
    title = "Daily average number of pedestrians across Melbourne",
    y = "Thousands of persons"
  )

# Solution 1 -----------------------------------------------------------------

walkers_sol1 <- walkers |>
  stretch_tsibble(.step = 7, .init = 365) |>
  model(arima = ARIMA(Count ~ PDQ(period = "week"))) |>
  forecast(h = 21)

# Solution 2 -----------------------------------------------------------------

# Intervention model, step functions during lockdowns
# Use intervention model only when there is at least one period
# of lockdown. Revert to standard ARIMA model if intervention
# model returns NULL

walker_stretch <- walkers |>
  stretch_tsibble(.step = 7, .init = 365)

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

ped_sol2_fc <- function(id, model, h = h) {
  newd <- new_data(model$data, n = h) |>
    mutate(Lockdown = FALSE)
  for (i in seq(nrow(lockdowns))) {
    newd <- newd |>
      mutate(Lockdown = Lockdown | (Date >= lockdowns$Start[i] & Date <= lockdowns$End[i]))
  }
  forecast(model, new_data = newd) |>
    mutate(.id = id) |>
    as_tibble()
}
walkers_sol2 <- purrr::pmap_dfr(
  fit,
  function(.id, arima) {
    ped_sol2_fc(.id, arima, h = 21)
  }
) |>
  mutate(.model = "arima") |>
  as_fable(
    index = Date, key = c(.id, .model), response = "Count",
    distribution = Count
  ) |>
  select(.id, .model, Date, Count, .mean)


# Solution 3 -----------------------------------------------------------------

# Set Counts in lockdown periods to missing
walkers_sol3 <- walkers |>
  stretch_tsibble(.step = 7, .init = 365) |>
  mutate(Count = if_else(Lockdown, NA_real_, Count)) |>
  model(arima = ARIMA(Count ~ PDQ(period = "week"))) |>
  forecast(h = 21)

# Solution 4 -----------------------------------------------------------------

# Replace Lockdown periods with interpolations from an ARIMA model
tmp <- walkers |>
  mutate(Count = if_else(Lockdown, NA_real_, Count))
new_walkers <- tmp |>
  model(arima = ARIMA(Count ~ PDQ(period = "week"))) |>
  interpolate(new_data = tmp)

walkers_sol4 <- new_walkers |>
  stretch_tsibble(.step = 7, .init = 365) |>
  model(arima = ARIMA(Count ~ PDQ(period = "week"))) |>
  forecast(h = 21)


## ENSEMBLE
# Create ensemble from list of fable objects
# Assumes the key structures are identical other than .model

wensemble <- list(walkers_sol1, walkers_sol2) |>
  lapply(function(x) {
    x |>
      as_tibble() |>
      mutate(.model = NULL) |>
      group_by(.id) |>
      slice(1:7)
  }) |>
  bind_rows() |>
  group_by(Date) |>
  summarise(
    .id = unique(.id),
    .mean = mean(.mean)
  ) |>
  mutate(Count = distributional::dist_degenerate(.mean)) |>
  as_fable(index = Date, key = .id, response = "Count", distribution = Count)


pedestrian_fc_plot <- function(fc, data) {
  # Plot of data showing lockdown periods
  p <- data |>
    filter(Date >= "2020-01-01") |>
    ggplot() +
    geom_rect(
      data = lockdowns, xmin = lockdowns$Start, xmax = lockdowns$End,
      ymin = -Inf, ymax = Inf, fill = "grey60", alpha = 0.5
    ) +
    geom_line(aes(x = Date, y = Count)) +
    labs(
      title = "Daily average number of pedestrians across Melbourne",
      y = "Thousands of persons"
    )

  fc <- fc |>
    group_by(.id) |>
    slice(1:7) |>
    mutate(.id = as.character(.id))
  p +
    geom_line(data = fc, mapping = aes(x = Date, y = .mean, group = .id), col = "blue")
}

walkers_plot1 <- pedestrian_fc_plot(walkers_sol1, walkers)
walkers_plot2 <- pedestrian_fc_plot(walkers_sol2, walkers)
walkers_plot3 <- pedestrian_fc_plot(walkers_sol3, walkers)
walkers_plot4 <- pedestrian_fc_plot(walkers_sol4, walkers)
walkers_ensemble_plot <- pedestrian_fc_plot(wensemble, walkers)
