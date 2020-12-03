#! /usr/bin/env Rscript

library(poddr)

incomparable_shows <- incomparable_get_shows()
incomparable_episodes <- incomparable_get_episodes(incomparable_shows)

cache_podcast_data(incomparable_shows)
cache_podcast_data(incomparable_episodes)
