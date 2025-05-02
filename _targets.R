# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("dplyr"), # Packages that your targets need for their tasks.
  format = "qs", # Optionally set the default storage format. qs is fast.
  controller = crew::crew_controller_local(
    workers = max(1, floor(getOption("Ncpus") / 2)),
    seconds_idle = 60
  ),
  error = "trim"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

list(
  tar_target(
    name = atp,
    command = atp_get_episodes(cache = FALSE),
    packages = "poddr"
  ),
  tar_target(
    name = relay_shows,
    command = relay_get_shows(cache = FALSE),
    packages = "poddr"
  ),
  tar_target(
    name = relay_episodes,
    command = relay_get_episodes(relay_shows, cache = FALSE),
    packages = "poddr"
  ),
  tar_target(
    name = incomparable_shows,
    command = incomparable_get_shows(cache = FALSE),
    packages = "poddr"
  ),
  tar_target(
    name = incomparable_episodes,
    command = incomparable_get_episodes(incomparable_shows, cache = FALSE),
    packages = "poddr"
  ),
  tar_target(
    name = mothership_wide,
    command = subset(
      incomparable_episodes,
      show == "The Incomparable Mothership"
    )
  ),
  tar_target(
    name = mothership_long,
    command = gather_people(mothership_wide),
    packages = "poddr"
  ),
  tar_target(
    name = podcasts,
    command = combine_data(incomparable_episodes, relay_episodes, atp),
    packages = "dplyr"
  ),
  tar_quarto(
    name = site,
    path = ".",
    quiet = FALSE
  )
)
