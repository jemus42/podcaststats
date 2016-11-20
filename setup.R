#### Setup stuff for each sub-analysis ####
# Should be source'd on top of each Rmd
#### Loading packages ####
library(sjPlot)
library(dplyr)
library(knitr)
library(ggplot2)
library(DT)
library(tadaatoolbox)
library(readr)
library(rvest)
library(magrittr)
library(dplyr)
library(broom)
library(tidyr)
library(scales)
library(ggthemes)
library(RColorBrewer)
library(stringr)
library(ggbeeswarm)
library(plotly)
library(scales)
library(viridis)
library(lubridate)
library(ggrepel)

#### Knitr chunk options ####
knitr::opts_chunk$set(fig.path = "assets/plots/", fig.align = "center",
                      fig.retina = 2, echo = T, warning = F, message = F,
                      cache.path = "assets/cache/", cache = F)

#### Reading the prepared dataset ####


#### Plotting presets ####

# Set default theme
theme_set(theme_readthedown())

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
  if (is.null(filename)){
    filename <- deparse(substitute(x))
  }
  path <- paste0(file.path(dir, filename), ".rds")
  message("Saving ", filename, " to ", path)
  saveRDS(x, path)
}

