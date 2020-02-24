
#! /usr/bin/env Rscript
#### Acquiring the data and chaching it

source("setup.R")
source("podcast_parsers.R")

#### ATP ####
atp <- parse_atp_feed()
cache_podcast_data(atp)
