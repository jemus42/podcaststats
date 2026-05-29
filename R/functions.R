combine_data <- function(incomparable_episodes, relay_episodes, atp) {
  incomparable_episodes |>
    bind_rows(relay_episodes) |>
    bind_rows(
      atp |>
        mutate(
          network = "ATP",
          show = "ATP"
        )
    )
}

# For yearly review things, use the current(ish) year.
current_year <- lubridate::year(lubridate::today() - lubridate::dmonths(3))

caption <- paste0(
  "podcasts.jemu.name\n",
  format(lubridate::now("UTC"), format = "%F %H:%M %Z")
)
