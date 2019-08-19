#! /usr/bin/env Rscript
#### Acquiring the data and chaching it

source("setup.R", echo = F)
source("podcast_parsers.R")

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
write_delim(relay, "data/relay.csv", delim = ";")


#### The Incomparable ####
source("incomparable_helpers.R")

incomparable_shows <- read_html("https://www.theincomparable.com/shows/") %>%
  html_nodes("h3 a") %>%
  html_text()

incomparable_show_partials <- read_html("https://www.theincomparable.com/shows/") %>%
  html_nodes("h3 a") %>%
  html_attr("href") %>%
  str_replace_all("\\/", "")

incomparable_shows <- tibble(partial = incomparable_show_partials,
                             show = incomparable_shows)

incomparable_master <- tibble()

for (i in seq_len(nrow(incomparable_shows))) {
  show                <- get_podcast_stats(incomparable_shows$partial[i],
                                           show_title = incomparable_shows$show[i])
  incomparable_master <- bind_rows(incomparable_master, show)
}

incomparable_segments <- get_podcast_segment_episodes()

# Spreading guests
incomparable_master_wide <- incomparable_master %>%
  widen_people() %>%
  full_join(incomparable_segments,
            by = c("number" = "number", "podcast" = "podcast"))

# Appending the segments to the long dataset
incomparable_master %<>%
  full_join(incomparable_segments,
            by = c("number" = "number", "podcast" = "podcast"))

cache_podcast_data(incomparable_master_wide)
cache_podcast_data(incomparable_master)

#### ATP ####
atp <- parse_atp_feed()
cache_podcast_data(atp)
