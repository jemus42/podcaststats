#### Setup stuff for each sub-analysis ####
# Should be source'd on top of each Rmd
#### Loading packages ####
library(dplyr)
library(knitr)
library(ggplot2)
library(DT)
library(readr)
library(rvest)
library(magrittr)
library(dplyr)
library(broom)
library(tidyr)
library(scales)
library(RColorBrewer)
library(stringr)
library(ggbeeswarm)
library(plotly)
library(scales)
library(viridis)
library(lubridate)
library(ggrepel)
library(hrbrthemes)

#### Knitr chunk options ####
knitr::opts_chunk$set(
  fig.path = "assets/plots/",
  fig.align = "center",
  fig.retina = 2,
  echo    = FALSE,
  cache   = FALSE,
  prompt  = FALSE,
  tidy    = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  fig.retina = 2,
  cache.path = "assets/cache/", cache = F
)

opts_knit$set(width = 75)


#### Plotting presets ####
caption <- paste0("podcasts.jemu.name – @jemus42\n",
                  format(lubridate::now("UTC"), format = "%F %H:%M %Z"))

# Set default theme
theme_set(theme_ipsum())

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
cache_podcast_data <- function(x, dir = "data", filename = NULL) {
  if (is.null(filename)) {
    filename <- deparse(substitute(x))
  }
  path <- paste0(file.path(dir, filename), ".rds")
  message("Saving ", filename, " to ", path)
  saveRDS(x, path)
}

