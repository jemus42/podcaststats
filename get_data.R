#### Acquiring the data and chaching it

source("podcast_parsers.R")
source("setup.R")

relay <- get_relay_shows()
cache_podcast_data(relay)
