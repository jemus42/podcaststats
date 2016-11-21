#! /usr/bin/env Rscript
#### Acquiring the data and chaching it

## Relay.fm
source("podcast_parsers.R")
source("setup.R")

relay_shows <- read_html("https://www.relay.fm/shows") %>%
  html_nodes("h4 a") %>%
  html_attr("href") %>%
  str_c("https://www.relay.fm", ., "/feed")

relay <- get_relay_shows(relay_shows)
cache_podcast_data(relay)

## Incomparable
source("get_incomparable.R")
