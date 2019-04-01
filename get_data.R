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
}; rm(i, incomparable_show_partials, incomparable_shows, show)

# Spreading guests
incomparable_master_wide <- incomparable_master %>%
  widen_people() %>%
  full_join(get_podcast_segment_episodes(),
            by = c("number" = "number", "podcast" = "podcast"))

# Appending the segments to the long dataset
incomparable_master %<>%
  full_join(get_podcast_segment_episodes(),
            by = c("number" = "number", "podcast" = "podcast"))

#### ATP ####
atp <- parse_atp_feed()
# Manual fix
# atp$date[atp$number == 254] <- ymd("2017-12-27")
# atp$month[atp$number == 254] <- "December"
# atp$duration[atp$number == 254] <- 144.87

cache_podcast_data(atp)

#### Write to disk ####
cache_podcast_data(incomparable_master_wide)
cache_podcast_data(incomparable_master)

# Write master set as CSV with ; as separator because Numbers likes that more than ,
# incomparable_master_wide %>%
#   mutate(summary = str_replace_all(summary, '\\s"', ' “'),
#          summary = str_replace_all(summary, '"(\\s)*', '” '),
#          summary = str_trim(summary, "right")) %>%
#   arrange(desc(date)) %>%
#   write.table(., "data/incomparable_master_wide.csv", sep = ";", row.names = F)
#
# incomparable_master %>%
#   mutate(summary = str_replace_all(summary, '\\s"', ' “'),
#          summary = str_replace_all(summary, '"(\\s)*', '” '),
#          summary = str_trim(summary, "right")) %>%
#   arrange(desc(date)) %>%
#   write.table(., "data/incomparable_master.csv", sep = ";", row.names = F)
