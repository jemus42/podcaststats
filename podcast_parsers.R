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

  show <- feed %>%
    html_node("channel") %>%
    html_node("title") %>%
    html_text()

  titles <- feed %>%
    html_nodes("item") %>%
    html_node("title") %>%
    html_text()

  number <- titles %>%
    str_extract("\\d+:") %>%
    str_replace(":", "") %>%
    as.numeric()


  durations <- feed %>%
    html_nodes("duration") %>%
    html_text() %>%
    as.numeric() %>%
    divide_by(60)

  pubdate <- feed %>% html_nodes("pubdate") %>%
    html_text() %>%
    str_replace("^.{5}", "") %>%
    lubridate::parse_date_time("%d %b %Y %H:%M:%S", tz = "GMT") %>%
    magrittr::extract(-1)

  people <- feed %>%
    html_nodes("author") %>%
    html_text() %>%
    str_replace_all(" and ", ", ") %>%
    magrittr::extract(-1)

  tibble(number  = number, podcast = show,
         title   = titles, duration = durations,
         date    = pubdate, people = people,
         network = "Relay.fm") %>%
  mutate(month = month(date, label = T, abbr = F),
         year  = year(date),
         date  = as_date(date))
}

get_relay_shows <- function(urls) {
  purrr::map_df(urls, parse_relay_feed)
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

  pub_monthday <- meta %>%
    str_trim() %>%
    str_extract("^\\w{3}\\s\\d{1,2}")

  pub_year <- meta %>%
    str_trim() %>%
    str_extract("20\\d{2}") %>%
    ifelse(is.na(.), year(today()), .)

  pub <- mdy(paste(pub_monthday, pub_year))

  drt    <- str_extract(meta, "\\u2022.*$") %>%
              str_replace_all("\\u2022\\s", "") %>%
              str_trim() %>%
              str_extract(pattern = "\\d+") %>% # Should be minutes
              as.numeric()

  summ  <- html_nodes(raw, ".lighttext.margintop05") %>% html_text() %>%
            str_replace_all("^\\n", "") %>%
            str_trim("both")

  tibble(number = nums, title = titles, date = pub,
         month = month(pub, label = T, abbr = F),
         year = pub_year, duration = drt, summary = summ,
         network = "ATP", podcast = "ATP")
}
