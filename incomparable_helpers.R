#### Helper functions ####

#### Parse date and times
enhance_datetimes <- function(showstats) {
  require(lubridate)
  showstats %<>%
    filter(!is.na(number)) %>%
    mutate(duration = parse_duration(duration),
           date     = as.POSIXct(dmy(date)),
           year     = as.factor(year(date)),
           month    = month(date, abbr = F, label = T),
           weekday  = wday(date, label = T, abbr = F)) %>%
    select(podcast, number, date, year, month, weekday, everything())
  return(showstats)
}

#### Spread long to wide format
# Only applicable to master dataset
widen_people <- function(master) {
  require(stringr)
  master %<>%
    group_by(podcast, number, role) %>%
    mutate(person_number = paste0("person_", seq_along(person))) %>%
    spread(person_number, person) %>%
    unite(persons, starts_with("person_"), sep = ", ") %>%
    mutate(persons = str_replace_all(persons, ", NA", "")) %>%
    spread(key = role, value = persons) %>%
    ungroup %>%
    select(podcast, number, date, year, month, weekday,
           duration, title, host, guest, category, topic, everything())
  return(master)
}

#### Handling the stats.txt ####
#### Initial collection of the stats file
# Thanks a lot, @l3viathan https://twitter.com/L3viathan2142/status/701009923841400833
get_initial_stats <- function(urlpartial = "theincomparable", show_title = "The Incomparable") {
  require(readr)
  require(magrittr)

  stats_url <- paste0("https://www.theincomparable.com/", urlpartial, "/stats.txt")
  message(stats_url)
  showstats <- read_lines(stats_url) %>%
    str_c(., ";") %>%
    paste0(collapse = "\n") %>%
    read_delim(file = ., delim = ";", quote = "",
                          col_names = F, col_types = cols(X1 = col_character(),
                                                          X3 = col_character()))

  if (ncol(showstats) == 5) {
    names(showstats) <- c("number", "date", "duration", "title", "host")
    showstats$guest <- "None"
  } else {
    showstats        <- showstats[1:6]
    names(showstats) <- c("number", "date", "duration", "title", "host", "guest")
  }
  showstats$podcast <- show_title
  showstats %<>% select(podcast, everything())

  return(showstats)
}

#### Preparations after initital collection of stats.txt ####
#### Extract host
extract_show_hosts <- function(showstats) {

  comma_count <- max(str_count(showstats$host, ","), na.rm = T)

  showstats %<>%
    separate(host, into = paste0("host_", 1:(comma_count+1)), sep = "\\,\\s") %>%
    mutate(host_1 = str_trim(host_1, "both"))

  return(showstats)
}

#### Further guest management
extract_show_guests <- function(showstats){

  if (all(is.na(showstats$guest))) return(showstats)

  comma_count <- max(str_count(showstats$guest, ","), na.rm = T)

  showstats %<>% separate(guest, paste0("guest_", 1:(comma_count+1)), sep = ",")

  showstats %<>%
    gather(position, guest, contains("guest")) %>%
    mutate(guest = str_trim(guest, side = "both")) %>%
    select(-position) %>%
    arrange(desc(date))
  return(showstats)
}

#### Collapsing the people
collapse_show_people <- function(showstats) {
  showstats %<>%
    gather(host_position, host, contains("host")) %>%
    gather(role, person, host, guest) %>%
    mutate(person = str_trim(person, "both")) %>%
    select(-host_position) %>%
    distinct() %>%
    filter(!is.na(person))
  return(showstats)
}

#### Compilation of the above functions in one ####
handle_people <- function(showstats) {
  showstats %<>% extract_show_hosts()
    if ("guest" %in% names(showstats)){
      showstats %<>% extract_show_guests()
    }
  showstats %<>% collapse_show_people() %>%
    filter(person != "None")
  return(showstats)
}

get_podcast_stats <- function(urlpartial = "theincomparable", show_title = "The Incomparable"){
    get_initial_stats(urlpartial, show_title) %>%
    enhance_datetimes() %>%
    handle_people() %>%
    select(-title) %>%
    full_join(y = get_podcast_metadata(urlpartial),
              by = c("number" = "number")) %>%
    filter(!is.na(podcast)) %>%
    arrange(desc(date)) %>%
    select(podcast, number, date, year, month, weekday,
           duration, title, person, role, category, topic, summary)
}

#### Parsing the archive pages ####
#### Getting summaries, topics and categories
get_podcast_metadata <- function(urlpartial = "theincomparable"){
  require(rvest)
  require(dplyr)
  require(stringr)

  cat("getting", urlpartial, "\n")

  url     <- paste0("https://www.theincomparable.com/", urlpartial, "/archive/")

  archive_parsed <- read_html(url)

  entries <- archive_parsed %>%
    html_nodes(css = "#entry") %>%
    html_text()

  epnums <- archive_parsed %>%
    html_nodes(css = ".episode-number") %>%
    html_text() %>%
    as.character()

  summaries <- archive_parsed %>%
    html_nodes(css = ".episode-description") %>%
    html_text() %>%
    str_replace_all("^(\\W)*", "") %>%
    str_replace_all("\\W*$", "")

  titles <- archive_parsed %>%
    html_nodes(css = ".entry-title a") %>%
    html_text()

   categories <- archive_parsed %>%
     html_nodes("#entry img") %>%
     html_attr("alt")

   if (identical(categories, character(0))) {
     categories <- NA
   }

   topics <- archive_parsed %>%
     html_nodes(".postdate+ .postdate") %>%
     html_text() %>%
     str_extract("•.*") %>%
     str_replace_all("•", "")  %>%
     str_trim("both")

   if (length(topics) != length(titles)) {
     topics <- rep(NA, length(titles))
   }
   if (length(categories) != length(titles)) {
     categories <- rep(NA, length(titles))
   }

  result <- tibble(number = epnums,
                   title = titles,
                   summary = summaries,
                   category = categories,
                   topic = topics)
  return(result)
}

get_podcast_segment_episodes <- function() {
  require(rvest)
  require(purrr)

  inc_raw <- read_html("https://www.theincomparable.com/incomparable/") %>%
    html_nodes("#nav:nth-child(3) a")
  inc_partials <- inc_raw %>%
    html_attr("href") %>%
    str_remove("theincomparable") %>%
    str_remove_all("/") %>%
    unique()
  inc_titles <- inc_raw %>%
    html_text() %>%
    unique()
  segments_incomparable <- tibble(partial = inc_partials, name = inc_titles)

  inc <-  map2_df(segments_incomparable$partial, segments_incomparable$name,
                  function(partial, name) {
          url <- paste("https://www.theincomparable.com/theincomparable",
                       partial, "archive", sep = "/")
          entry  <- read_html(url) %>% html_nodes(".entry-title a")
          title  <- entry %>% html_text()
          epnums <- entry %>% html_attr("href") %>% str_extract("\\d+")
          tibble(number = epnums, segment = name, podcast = "The Incomparable")
        })

  # Get gameshow ----
  gs_raw <- read_html("https://www.theincomparable.com/gameshow/") %>%
    html_nodes("#nav:nth-child(3) a")
  gs_partials <- gs_raw %>%
    html_attr("href") %>%
    str_remove("/gameshow") %>%
    str_remove_all("/") %>%
    unique()
  gs_titles <- gs_raw %>%
    html_text() %>%
    unique()
  segments_gameshow <- tibble(partial = gs_partials, name = gs_titles)

  gs <-  map2_df(segments_gameshow$partial, segments_gameshow$name,
                 function(partial, name) {
            url <- paste("https://www.theincomparable.com/gameshow",
                         partial, "archive", sep = "/")
            entry  <- read_html(url) %>% html_nodes(".entry-title a")
            title  <- entry %>% html_text()
            epnums <- entry %>% html_attr("href") %>% str_extract("\\d+")
            tibble(number = epnums, segment = name, podcast = "Game Show")
          })

  # Get teevee ----
  teevee_raw <- read_html("https://www.theincomparable.com/teevee/") %>%
    html_nodes("#nav:nth-child(3) a")
  teevee_partials <- teevee_raw %>%
    html_attr("href") %>%
    str_remove("/teevee") %>%
    str_remove_all("/")
  teevee_titles <- teevee_raw %>%
    html_text()
  segments_teevee <- tibble(partial = teevee_partials, name = teevee_titles)

  teevee <-  map2_df(segments_teevee$partial, segments_teevee$name,
                            function(partial, name) {
                url <- paste("https://www.theincomparable.com/teevee",
                             partial, "archive", sep = "/")
                entry  <- read_html(url) %>% html_nodes(".entry-title a")
                title  <- entry %>% html_text()
                epnums <- entry %>% html_attr("href") %>% str_extract("\\d+")
                tibble(number = epnums, segment = name, podcast = "TeeVee")
              })

  bind_rows(inc, gs, teevee)
}
