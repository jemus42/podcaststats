#! /usr/bin/env Rscript

source("00_setup.R")
source("00_podcast_parsers.R")

#### Relay.fm ####
relay_shows <- read_html("https://www.relay.fm/shows") %>%
  html_nodes(".broadcast__name a") %>%
  html_attr("href") %>%
  str_c("https://www.relay.fm", ., "/feed")

relay <- get_relay_shows(relay_shows)

retired_shows <- read_html("https://www.relay.fm/shows") %>%
  html_nodes(".subheader~ .entry .broadcast__name a") %>%
  html_text()

relay %<>%
  mutate(show_status = ifelse(podcast %in% retired_shows, "Retired", "Active"),
         month    = month(date, abbr = F, label = T),
         weekday  = wday(date, label = T, abbr = F))

cache_podcast_data(relay)
