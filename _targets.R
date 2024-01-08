# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("fpp3")
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# List of targets
list(
  # Tourism data ---------------------------------------------------------------
  tar_target(tourism_csv, here::here("data/340101.xlsx"), format = "file"),
  tar_target(austa, read_tourism(tourism_csv)),
  # Tourism time plot
  tar_target(tourism_labels, labs(
    x = "Month",
    y = "Thousands of visitors",
    title = "Total short-term visitors to Australia"
  )),
  tar_target(tourism_plot1, tourism_plot(austa) + tourism_labels),
  # Forecasts
  tar_target(tsol1, tourism_sol1(austa)),
  tar_target(tsol2, tourism_sol2(austa)),
  tar_target(tsol3, tourism_sol3(austa)),
  tar_target(tsol4, tourism_sol4(austa)),
  tar_target(tensemble, tourism_ensemble(list(tsol1,tsol2,tsol3,tsol4))),
  
  # Forecast plots
  tar_target(
    tourism_history,
    geom_line(
      data = austa |> filter(Month >= yearmonth("2016 Jan")),
      aes(y = Visitors)
    )
  ),
  tar_target(
    tsol1_plot,
    autoplot(tsol1, level = 90) + tourism_history + tourism_labels
  ),
  tar_target(
    tsol2_plot,
    autoplot(tsol2, level = 90) + tourism_history + tourism_labels
  ),
  tar_target(
    tsol3_plot,
    autoplot(tsol3, level = 90) + tourism_history + tourism_labels
  ),
  tar_target(
    tsol4_plot,
    autoplot(tsol4, level = 90) + tourism_history + tourism_labels
  ),
  tar_target(
    tensemble_plot,
    autoplot(tensemble, level = NULL) + tourism_history + tourism_labels
  ),
  
  # Pedestrians
  tar_target(raw_walkers, fetch_walkers(start = "2019-01-01", end = "2021-12-31")),
  tar_target(lockdowns, lockdown_table()),
  tar_target(walkers2, clean_walkers(raw_walkers)),
  tar_target(walkers3, ave_walkers(walkers2)),
  tar_target(walkers, add_lockdowns(walkers3)),
  tar_target(walkers_plot, pedestrian_plot(walkers)),
  tar_target(init, 365),
  tar_target(step, 7),
  tar_target(h, 21),
  tar_target(walkers_sol1, pedestrian_sol1(walkers, step = step, init = init, h = h)),
  tar_target(walkers_sol2, pedestrian_sol2(walkers, step = step, init = init, h = h)),
  tar_target(walkers_sol3, pedestrian_sol3(walkers, step = step, init = init, h = h)),
  tar_target(walkers_sol4, pedestrian_sol4(walkers, step = step, init = init, h = h)),
  tar_target(wensemble, pedestrian_ensemble(list(walkers_sol1, walkers_sol2))),
  tar_target(walkers_plot1, pedestrian_fc_plot(walkers_sol1, walkers)),
  tar_target(walkers_plot2, pedestrian_fc_plot(walkers_sol2, walkers)),
  tar_target(walkers_plot3, pedestrian_fc_plot(walkers_sol3, walkers)),
  tar_target(walkers_plot4, pedestrian_fc_plot(walkers_sol4, walkers)),
  tar_target(walkers_ensemble_plot, pedestrian_fc_plot(wensemble, walkers)),
  
  # Attended incidents example
  tar_target(attendant_csv, here::here("data/attendent_incident.csv"), format = "file"),
  tar_target(verified_csv, here::here("data/verified_incident.csv"), format = "file"),
  tar_target(attendant, readr::read_csv(attendant_csv)),
  tar_target(verified, readr::read_csv(verified_csv)),
  tar_target(df, create_tsibble(attendant, verified)),
  tar_target(attended_plot, autoplot(df, Attended)),
  tar_target(
    stl, # STL Decomposition
    df |>
      filter(Area == "BC") |>
      model(
        stl = STL(Attended ~ trend(window = 81) + season(period = 7, window = Inf)),
      )
  ),
  tar_target(
    stl_plot,
    stl |>
      components() |>
      autoplot()
  ),
  tar_target(
    stl_model,
    decomposition_model(
      STL(Attended ~ trend(window = 81) + season(period = 7, window = Inf)),
      ETS(season_adjust ~ season("N"))
    )
  ),
  tar_target(
    fit,
    df |>
      model(
        ets = ETS(Attended),
        arima = ARIMA(Attended),
        stl_ets = stl_model
      )
  ),
  tar_target(fc, forecast(fit, h = 365)),
  tar_quarto(paper,
    "fits.qmd",
    extra_files = "references.bib"
  )
)
