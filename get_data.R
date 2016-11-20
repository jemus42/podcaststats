#! /usr/bin/env Rscript
#### Acquiring the data and chaching it

## Relay.fm
source("podcast_parsers.R")
source("setup.R")

relay <- get_relay_shows()
cache_podcast_data(relay)

## Incomparable
source("get_incomparable.R")
