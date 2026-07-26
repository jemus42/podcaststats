# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("dplyr"),
  format = "qs",
  error = "trim"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

list(
  # tar_age() re-runs a target once its stored copy is older than `age`,
  # so a weekly cron re-scrapes but local re-renders within the week don't.
  # Downstream targets invalidate only when the scraped data actually changes.
  tar_age(
    name = atp,
    command = atp_get_episodes(cache = FALSE),
    age = as.difftime(1, units = "weeks"),
    packages = "poddr"
  ),
  tar_age(
    name = relay_shows,
    command = relay_get_shows(cache = FALSE),
    age = as.difftime(1, units = "weeks"),
    packages = "poddr"
  ),
  tar_age(
    name = relay_episodes,
    command = relay_get_episodes(relay_shows, cache = FALSE),
    age = as.difftime(1, units = "weeks"),
    packages = "poddr"
  ),
  tar_age(
    name = incomparable_shows,
    command = incomparable_get_shows(cache = FALSE),
    age = as.difftime(1, units = "weeks"),
    packages = "poddr"
  ),
  tar_age(
    name = incomparable_episodes,
    command = incomparable_get_episodes(incomparable_shows, cache = FALSE),
    age = as.difftime(1, units = "weeks"),
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
  tar_target(
    name = data_files,
    command = export_datasets(atp, relay_episodes, incomparable_episodes),
    format = "file",
    packages = c("fs", "here")
  ),
  tar_quarto(
    name = site,
    path = ".",
    quiet = FALSE
  )
)
