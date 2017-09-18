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
              str_extract(pattern = "\\d+") # Should be minutes

  summ  <- html_nodes(raw, ".lighttext.margintop05") %>% html_text() %>%
            str_replace_all("^\\n", "") %>%
            str_trim("both")

  tibble(number = nums, title = titles, date = pub, month = month(pub, label = T, abbr = F),
         year = yr, duration = drt, summary = summ)
}

# parse_atp_feed <- function(url = "http://atp.fm/episodes?format=rss") {
#   atpfeed <- read_html(url)
#
#   titles    <- atpfeed %>% html_nodes("title") %>%
#     html_text() %>%
#     str_replace_all("\\t", "")
#
#   number <- str_extract(titles, "^\\d+")
#
#   durations <- atpfeed %>% html_nodes("duration") %>%
#     html_text() %>%
#     parse_duration()
#
#   pubdate   <- atpfeed %>% html_nodes("pubdate") %>%
#     html_text() %>%
#     str_replace("^.{5}", "") %>%
#     lubridate::parse_date_time("%d %b %Y %H:%M:%S %z")
#
#   atp <- tibble(number = number[-1], title = titles[-1],
#                 duration = durations, date = pubdate) %>%
#             mutate(month = month(date, label = T, abbr = F),
#                    year  = as.factor(year(date)))
#
#   return(atp)
# }
#
# latest_atp_num <- function() {
#   read_html("http://atp.fm/") %>%
#     html_nodes(".entry-title a") %>%
#     html_text() %>%
#     extract2(1) %>%
#     str_extract("^\\d+") %>%
#     as.numeric()
# }
#
# download_atp_episodes <- function() {
#   plyr::l_ply(seq_len(latest_atp_num()), function(num) {
#
#     file <- paste0("data/atp/atp", num, ".mp3")
#
#     if (!file.exists(file)) {
#       url <- paste0("http://traffic.libsyn.com/atpfm/atp", num, ".mp3")
#       download.file(url, file)
#     }
#   })
# }
#
# get_atp_data_from_files <- function(dir = "data/atp/") {
#   files <- list.files(dir, pattern = "atp\\d+.mp3", full.names = T)
#
#   nums      <- as.numeric(str_extract(files, "\\d+"))
#   durations <- get_mp3_duration(files)
#   titles    <- get_mp3_title(files)
#
#   ret <- tibble(number = nums, title = titles, duration = durations) %>%
#            arrange(desc(number))
# }
#
# #### Parsing generic mp3 files as a fallback ####
# get_mp3_duration <- function(file, unit = "min") {
#
#   if (length(file) > 1) {
#     ret <- sapply(file, get_mp3_duration, unit = unit, USE.NAMES = F)
#     return(ret)
#   }
#
#   cmd <- "mediainfo --Inform='General;%Duration%'"
#   cmd <- paste(cmd, file)
#
#   ret <- as.numeric(system(cmd, intern = T))
#   ret <- ret/1000
#
#   if (unit == "sec") {
#    return(ret)
#   } else if (unit == "min") {
#     ret <- ret/60
#     return(ret)
#   } else {
#     stop("Invalid unit specification")
#   }
# }
#
# get_mp3_title <- function(file) {
#
#   if (length(file) > 1) {
#     ret <- sapply(file, get_mp3_title, USE.NAMES = F)
#     return(ret)
#   }
#
#   cmd <- "mediainfo --Inform='General;%Title%'"
#   cmd <- paste(cmd, file)
#
#   ret <- system(cmd, intern = T)
#   return(ret)
# }
