#! /usr/bin/env Rscript

source("00_setup.R")
source("00_podcast_parsers.R")

#### ATP ####
atp <- parse_atp_feed()
cache_podcast_data(atp)
