#! /usr/bin/env Rscript
#### Acquiring the data and chaching it

source("00_setup.R")
source("00_podcast_parsers.R")

#### The Incomparable ####
source("00_incomparable_helpers.R")

incomparable_shows <- get_incomparable_shows("https://www.theincomparable.com/shows/")

incomparable_master <- purrr::pmap_df(incomparable_shows, ~ {
  get_podcast_stats(.x, .y)
})

incomparable_segments <- get_podcast_segment_episodes()

# Spreading guests
incomparable_master_wide <- incomparable_master %>%
  widen_people() %>%
  full_join(
    incomparable_segments,
    by = c("number" = "number", "podcast" = "podcast")
  )

# Appending the segments to the long dataset
incomparable_master <- incomparable_master %>%
  full_join(
    incomparable_segments,
    by = c("number" = "number", "podcast" = "podcast")
  )

cache_podcast_data(incomparable_master_wide)
cache_podcast_data(incomparable_master)
