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

# For yearly review things, use the current(ish) year
current_year <- lubridate::year(lubridate::today() - lubridate::dmonths(3))

network_colors <- c(
  "The Incomparable" = "#252D6D",
  "relay.fm" = "#36667E",
  "ATP" = "#BAC2CC"
)

#### Plotting presets ####
caption <- paste0(
  "podcasts.jemu.name\n",
  format(lubridate::now("UTC"), format = "%F %H:%M %Z")
)


theme_podcasts <- function() {
  hrbrthemes::theme_ipsum(
    base_size = 14,
    plot_margin = ggplot2::margin(4, 4, 4, 4),
    axis_text_size = 12
  ) +
    ggplot2::theme(
      plot.title.position = "plot",
      panel.spacing.x = ggplot2::unit(2, "mm"),
      legend.position = "top",
      plot.caption.position = "plot"
    )
}
