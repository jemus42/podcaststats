#! /usr/bin/env Rscript
library(poddr)

cli::cli_h1("Getting data...")

cli::cli_ul()

cli::cli_li("Getting ATP")

atp <- atp_get_episodes()
cache_podcast_data(atp, csv = FALSE)

cli::cli_li("Getting relay.fm")

relay_shows <- relay_get_shows()
relay_episodes <- relay_get_episodes(relay_shows)

cache_podcast_data(relay_shows)
cache_podcast_data(relay_episodes)

cli::cli_li("Getting The Incomparable")

incomparable_shows <- incomparable_get_shows()
incomparable_episodes <- incomparable_get_episodes(incomparable_shows)

cache_podcast_data(incomparable_shows)
cache_podcast_data(incomparable_episodes)

cli::cli_end()

cli::cli_inform("Finished!")
