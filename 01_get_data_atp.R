#! /usr/bin/env Rscript

library(poddr)

atp <- atp_get_episodes()
cache_podcast_data(atp, csv = FALSE)
