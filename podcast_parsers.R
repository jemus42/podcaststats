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

  df <- tibble(number = number, podcast = show,
                   title = titles, duration = durations, date = pubdate, people = people) %>%
    mutate(month = month(date, label = T, abbr = F),
           year  = year(date))
  return(df)
}

get_relay_shows <- function(urls) {
  relay <- plyr::ldply(urls, function(x){
    parse_relay_feed(x)
  })

  return(relay)
}

#### ATP ####

parse_atp_feed <- function(url = "http://atp.fm/episodes?format=rss") {
  atpfeed <- read_html(url)

  titles    <- atpfeed %>% html_nodes("title") %>%
    html_text()

  number <- str_extract(titles, "^\\d+")

  durations <- atpfeed %>% html_nodes("duration") %>%
    html_text() %>%
    parse_duration()

  pubdate   <- atpfeed %>% html_nodes("pubdate") %>%
    html_text() %>%
    str_replace("^.{5}", "") %>%
    lubridate::parse_date_time("%d %b %Y %H:%M:%S %z")

  atp <- tibble(number = number[-1], title = titles[-1],
                duration = durations, date = pubdate) %>%
            mutate(month = month(date, label = T, abbr = F),
                   year  = as.factor(year(date)))

  return(atp)
}
