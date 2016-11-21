#### Functions to get the show data #####

#### RelayFM #####
parse_relay_feed <- function(url = "https://www.relay.fm/master/feed"){
  feed <- read_html(url)

  titles <- feed %>% html_nodes("title") %>%
    html_text() %>%
    magrittr::extract(-1)

  number <- titles %>%
    str_extract("\\d+:") %>%
    str_replace(":", "") %>%
    as.numeric()

  show <- titles %>%
    str_extract("^.*\\d") %>%
    str_replace_all("\\s\\d+.*$", "")

  durations <- feed %>%
    html_nodes("duration") %>%
    html_text() %>%
    as.numeric() %>%
    divide_by(60)

  pubdate <- feed %>% html_nodes("pubdate") %>%
    html_text() %>%
    str_replace("^.{5}", "") %>%
    lubridate::parse_date_time("%d %b %Y %H:%M:%S", tz = "GMT")
  pubdate <- pubdate[-1]

  people <- feed %>%
    html_nodes("author") %>%
    html_text() %>%
    str_replace_all(" and ", ", ") %>%
    magrittr::extract(-1)

  df <- data_frame(number = number, podcast = show,
                   title = titles, duration = durations, date = pubdate, people = people) %>%
    mutate(month = month(date, label = T, abbr = T),
           year  = year(date))
  return(df)
}

get_relay_shows <- function() {
  relay_urls <-
    c("https://www.relay.fm/analogue/feed",
      "https://www.relay.fm/almanac/feed",
      "https://www.relay.fm/bonanza/feed",
      "https://www.relay.fm/b-sides/feed",
      "https://www.relay.fm/canvas/feed",
      "https://www.relay.fm/clockwise/feed",
      "https://www.relay.fm/connected/feed",
      "https://www.relay.fm/cortex/feed",
      "https://www.relay.fm/disruption/feed",
      "https://www.relay.fm/freeagents/feed",
      "https://www.relay.fm/ltoe/feed",
      "https://www.relay.fm/isometric/feed",
      "https://www.relay.fm/liftoff/feed",
      "https://www.relay.fm/mpu/feed",
      "https://www.relay.fm/material/feed",
      "https://www.relay.fm/mixedfeelings/feed",
      "https://www.relay.fm/rd/feed",
      "https://www.relay.fm/remaster/feed",
      "https://www.relay.fm/rocket/feed",
      "https://www.relay.fm/penaddict/feed",
      "https://www.relay.fm/presentable/feed",
      "https://www.relay.fm/tc/feed",
      "https://www.relay.fm/topfour/feed",
      "https://www.relay.fm/radar/feed",
      "https://www.relay.fm/upgrade/feed",
      "https://www.relay.fm/ungeniused/feed",
      "https://www.relay.fm/inquisitive/feed",
      "https://www.relay.fm/virtual/feed")

  relay <- plyr::ldply(relay_urls, function(x){
    parse_relay_feed(x)
  })

  return(relay)
}
