library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(purrr)
library(epoxy)
library(poddr)

library(ggplot2)
library(scales)
library(ggrepel)
library(hrbrthemes)
library(ggbeeswarm)
#library(plotly)
library(DT)
library(reactable)

# Dummy to make renv pick up ragg and Hmisc
if (FALSE) {
  library(ragg)
  library(Hmisc)
}

incomparable_shows <- readRDS("data/incomparable_shows.rds")
incomparable_episodes <- readRDS("data/incomparable_episodes.rds")

mothership_wide <- incomparable_episodes |>
  filter(show == "The Incomparable Mothership")

mothership_long <- gather_people(mothership_wide)

relay_shows <- readRDS("data/relay_shows.rds")
relay_episodes <- readRDS("data/relay_episodes.rds")

atp <- readRDS("data/atp.rds") |>
  filter(!is.na(duration))


# For yearly review things, use the current(ish) year
current_year <- year(today() - dmonths(3))

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

duration_mins <- "Duration (H:M:S)"

# Set default theme
# hrbrthemes::import_public_sans()

# theme_set(
#   theme_ipsum(
#     base_size = 14,
#     plot_margin = margin(4, 4, 4, 4),
#     axis_text_size = 12
#   ) +
#     theme(
#       plot.title.position = "plot",
#       plot.caption.position = "plot",
#       panel.spacing.x = unit(2, "mm"),
#       legend.position = "top"
#     )
# )

theme_set(
  theme_minimal(
    base_size = 14
  ) +
    theme(
      axis.text = element_text(size = 12),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.spacing.x = unit(2, "mm"),
      legend.position = "top"
    )
)
