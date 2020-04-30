#### Setup stuff for each sub-analysis ####
# Should be source'd on top of each Rmd
#### Loading packages ####
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(rvest)
library(readr)

library(knitr)
library(ggplot2)
library(scales)
library(ggrepel)
library(hrbrthemes)
library(ggbeeswarm)
library(plotly)
library(DT)

library(broom)

# Dummy to make renv pick up ragg
require(ragg)

#### Knitr chunk options ####
knitr::opts_chunk$set(
  fig.path = "assets/plots/",
  fig.align = "center",
  fig.retina = 2,
  echo    = FALSE,
  prompt  = FALSE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  fig.retina = 2,
  dev = "ragg_png"
)

#### Plotting presets ####
caption <- paste0("podcasts.jemu.name – @jemus42\n",
                  format(lubridate::now("UTC"), format = "%F %H:%M %Z"))

# Set default theme
theme_set(
  theme_ipsum_rc(
    plot_margin = margin(6, 6, 6, 6)
  ) +
    theme(
      plot.title.position = "plot",
      panel.spacing.x = unit(5, "mm")
    )
)

# Convenience function to display N
label_n <- function(data, brackets = FALSE) {
  if (is.data.frame(data)) {
    n <- nrow(data)
  } else if (is.numeric(data) & length(data) == 1) {
    n <- data
  } else {
    stop("'data' must be a data.frame or a numeric vector of length 1")
  }
  ret <- paste0("N = ", n)
  if (brackets) {
    ret <- paste0("(", ret, ")")
  }
  return(ret)
}

#### Caching ####
cache_podcast_data <- function(x, dir = "data", filename = NULL, csv = TRUE) {
  if (is.null(filename)) {
    filename <- deparse(substitute(x))
  }

  path_rds <- paste0(file.path(dir, filename), ".rds")

  cliapp::cli_alert_success("Saving {filename} to {path_rds}")
  saveRDS(x, path_rds)

  if (csv) {
    path_csv <- paste0(file.path(dir, filename), ".csv")
    cliapp::cli_alert_success("Saving {filename} to {path_csv}")
    write_delim(x, path_csv, delim = ";")
  }

}

