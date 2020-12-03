#! /usr/bin/env Rscript

library(poddr)

relay_shows <- relay_get_shows()
relay_episodes <- relay_get_episodes(relay_shows)

cache_podcast_data(relay_shows)
cache_podcast_data(relay_episodes)
