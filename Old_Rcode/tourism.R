## TOURISM EXAMPLE -------------------------------------------------------------

library(fpp3)

# Read data --------------------------------------------------------------------

austa <- readxl::read_excel(
    path = here::here("data/340101.xlsx"),
    sheet = "Data1", skip = 9
  ) |>
  transmute(
    Month = as.Date(`Series ID`),
    Visitors = A85375847A / 1e3
  ) |>
  filter(Month >= "2000-01-01")

# Tourism time plot ------------------------------------------------------------

austa |>
  ggplot(aes(x = Month, y = Visitors)) +
  geom_line() +
  labs(
    x = "Month",
    y = "Thousands of visitors",
    title = "Total short-term visitors to Australia"
  )

# Convert to tsibble -----------------------------------------------------------

austa <- austa |>
  mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month)

# Solution 1 -------------------------------------------------------------------

tsol1 <- austa |>
  stretch_tsibble(.step = 12, .init = 240) |>
  model(
    ets = ETS(log(Visitors)),
    arima = ARIMA(log(Visitors))
  ) |>
  forecast(h = 12) |>
  mutate(.id = paste("Forecasts of", .id + 2019))

# Solution 2 -------------------------------------------------------------------

austa_stretch <- austa |>
  mutate(
    covid = as.numeric(Month >= yearmonth("2020 Mar") &
      Month <= yearmonth("2022 Oct")),
    recovery = Month >= yearmonth("2021 Oct") &
      Month <= yearmonth("2022 Oct"),
    recovery = recovery * (Month - yearmonth("2021 Sep"))
  ) |>
  stretch_tsibble(.step = 12, .init = 240)

# Fit models using none, one or two covariates
fit1 <- austa_stretch |>
  model(arima1 = ARIMA(log(Visitors)))
fit2 <- austa_stretch |>
  model(arima2 = ARIMA(log(Visitors) ~ covid)) |>
  suppressWarnings()
fit3 <- austa_stretch |>
  model(arima3 = ARIMA(log(Visitors) ~ covid + recovery)) |>
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
tourism_sol2_fc <- function(fit, h = h) {
  training_data <- fit$arima[[1]]$data
  id <- fit$.id[1]
  newd <- new_data(training_data, n = h) |>
    mutate(
      covid = as.numeric(Month >= yearmonth("2020 Mar") &
        Month <= yearmonth("2022 Oct")),
      recovery = Month >= yearmonth("2021 Oct") &
        Month <= yearmonth("2022 Oct"),
      recovery = recovery * (Month - yearmonth("2021 Sep"))
    )
  fit |>
    select(-.id) |>
    forecast(new_data = newd) |>
    mutate(.id = id)
}

tsol2 <- bind_rows(
  fit |> filter(.id == 1) |> tourism_sol2_fc(h = 12),
  fit |> filter(.id == 2) |> tourism_sol2_fc(h = 12),
  fit |> filter(.id == 3) |> tourism_sol2_fc(h = 12),
  fit |> filter(.id == 4) |> tourism_sol2_fc(h = 12),
) |>
  as_fable(
    index = Month, key = c(.id, .model), response = "Visitors",
    distribution = Visitors
  ) |>
  mutate(.id = paste("Forecasts of", .id + 2019))

# Solution 3 -------------------------------------------------------------------

tsol3 <- austa |>
  stretch_tsibble(.step = 12, .init = 240) |>
  mutate(
    Visitors = if_else(Month >= yearmonth("2020 Mar") &
      Month <= yearmonth("2022 Oct"),
    NA_real_, Visitors
    )
  ) |>
  model(
    arima = ARIMA(log(Visitors))
  ) |>
  forecast(h = 12) |>
  mutate(.id = paste("Forecasts of", .id + 2019))

# Solution 4 -------------------------------------------------------------------

# Replace Mar 2020 - Nov 2022 with average of last three years
ave_3y <- austa |>
  filter(
    Month >= yearmonth("2020 March") - 3 * 12,
    Month <= yearmonth("2020 Feb")
  ) |>
  mutate(month = month(Month)) |>
  as_tibble() |>
  summarise(ave = mean(Visitors), .by = "month")
austa <- austa |>
  mutate(month = month(Month)) |>
  left_join(ave_3y, by = "month") |>
  mutate(
    Visitors = if_else(Month < yearmonth("2022 Nov") &
      Month > yearmonth("2020 Feb"),
    ave, Visitors
    ),
  )
tsol4 <- austa |>
  stretch_tsibble(.step = 12, .init = 240) |>
  model(
    ets = ETS(log(Visitors)),
    arima = ARIMA(log(Visitors))
  ) |>
  forecast(h = 12) |>
  mutate(.id = paste("Forecasts of", .id + 2019))


# Create ensemble from list of fable objects -----------------------------------

# Assumes the key structures are identical other than .model
tensemble <- list(tsol1, tsol2, tsol3, tsol4) |>
  lapply(function(x) {
    x |>
      as_tibble() |>
      mutate(.model = NULL)
  }) |>
  bind_rows() |>
  mutate(Visitors = generate(Visitors, 2000)) |>
  group_by(.id, Month) |>
  summarise(Visitors = distributional::dist_sample(list(c(unlist(Visitors))))) |>
  ungroup() |>
  as_fable(
    index = Month, key = .id,
    response = "Visitors", distribution = Visitors
  ) |>
  suppressWarnings()


# Forecast plots ---------------------------------------------------------------

tourism_history <- geom_line(
  data = austa |> filter(Month >= yearmonth("2016 Jan")),
  aes(y = Visitors)
)
tourism_labels <- labs(
  x = "Month",
  y = "Thousands of visitors",
  title = "Total short-term visitors to Australia"
)
autoplot(tsol1, level = 90) + tourism_history + tourism_labels
autoplot(tsol2, level = 90) + tourism_history + tourism_labels
autoplot(tsol3, level = 90) + tourism_history + tourism_labels
autoplot(tsol4, level = 90) + tourism_history + tourism_labels
autoplot(tensemble, level = 90) + tourism_history + tourism_labels
