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
  tar_target(df, create_tsibble()),
  tar_target(attended_plot, autoplot(df, Attended)),
  tar_target(stl, # STL Decomposition
    df |>
      filter(Area == "BC") |>
      model(
        stl = STL(Attended ~ trend(window = 81) + season(period = 7, window = Inf)),
      )
  ),
  tar_target(stl_plot,
    stl |>
    components() |>
    autoplot()
  ),
  tar_target(stl_model,
    decomposition_model(
      STL(Attended ~ trend(window = 81) + season(period = 7, window = Inf)),
      ETS(season_adjust ~ season("N"))
    )
  ),
  tar_target(fit,
    df |>
      model(
        ets = ETS(Attended),
        arima = ARIMA(Attended),
        stl_ets = stl_model
      )
    ),
  tar_target(fc, forecast(fit, h=365)),
  tar_quarto(paper,
    "fits.qmd", extra_files = "references.bib")
)
