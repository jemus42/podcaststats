#### Functions to get the show data #####

#### Utility functions ####
#### Converting HH:MM:SS or MM:SS to a numeric vector of minutes
parse_duration <- function(x){
  require(stringr)
  mins <- sapply(x, function(x){
    if (str_count(x, ":") == 2) {
      xx      <- as.numeric(unlist(str_split(x, ":")))
      minutes <- xx[1] * 60 + xx[2] + xx[3] / 60
    } else if (str_count(x, ":") == 1) {
      xx      <- as.numeric(unlist(str_split(x, ":")))
      minutes <- xx[1] + xx[2] / 60
    } else {
      stop("Unexpected input format ", x)
    }
    return(unname(minutes))
  })
  mins <- unname(mins)
  return(mins)
}

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
# Thanks, Marco
# https://twitter.com/marcoarment/status/801550714568900608

parse_atp_feed <- function(url = "https://overcast.fm/itunes617416468/accidental-tech-podcast") {
  raw    <- read_html(url)
  titles <- html_nodes(raw, ".title") %>%
              html_text() %>%
              str_replace_all("\\t", "")
  nums   <- as.numeric(str_extract(titles, "^\\d+"))

  meta   <- html_nodes(raw, ".caption2.singleline") %>% html_text()
  pub    <- str_extract(meta, "^\\n\\s+.*\\u2022") %>%
              str_replace_all("^\\n\\s+", "") %>%
              str_replace("\\s\\u2022$", "")
  yr     <- as.numeric(str_extract(pub, "\\d{4}$"))
  yr     <- ifelse(is.na(yr), as.numeric(format(now(), "%Y")), yr)

  pub    <- str_replace(pub, ",\\s\\d+$", "")
  pub    <- mdy(paste0(pub, " ", yr))

  drt    <- str_extract(meta, "\\u2022.*$") %>%
              str_replace_all("\\u2022\\s", "") %>%
              str_trim() %>%
              str_extract(pattern = "\\d+") %>% # Should be minutes
              as.numeric()

  summ  <- html_nodes(raw, ".lighttext.margintop05") %>% html_text() %>%
            str_replace_all("^\\n", "") %>%
            str_trim("both")

  tibble(number = nums, title = titles, date = pub, month = month(pub, label = T, abbr = F),
         year = yr, duration = drt, summary = summ)
}
