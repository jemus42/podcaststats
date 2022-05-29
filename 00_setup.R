#### Setup stuff for each sub-analysis ####
# Should be source'd on top of each Rmd
#### Loading packages ####
# library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(hms)
library(purrr)
library(epoxy)
library(poddr)

# library(knitr)
library(ggplot2)
library(scales)
library(ggrepel)
library(hrbrthemes)
library(ggbeeswarm)
library(plotly)
library(DT)

library(broom)

# Dummy to make renv pick up ragg and Hmisc
require(ragg)
if (!requireNamespace("Hmisc")) install.packages("Hmisc")

#### Knitr chunk options ####
knitr::opts_chunk$set(
  fig.path = "assets/plots/",
  fig.align = "center",
  fig.retina = 2,
  echo = FALSE,
  prompt = FALSE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  fig.retina = 2,
  cache = FALSE,
  dev = "ragg_png"
)

# For yearly review things, use the current(ish) year
current_year <- year(today() - dmonths(3))

network_colors <- c(
  "The Incomparable" = "#252D6D",
  "relay.fm" = "#36667E",
  "ATP" = "#BAC2CC"
)

#### Plotting presets ####
caption <- paste0(
  "podcasts.jemu.name – @jemus42\n",
  format(lubridate::now("UTC"), format = "%F %H:%M %Z")
)

# Set default theme
# hrbrthemes::import_public_sans()

theme_set(
  theme_ipsum_pub(
    plot_margin = margin(4, 4, 4, 4)
  ) +
    theme(
      plot.title.position = "plot",
      panel.spacing.x = unit(2, "mm"),
      legend.position = "top"
    )
)
