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

  # categories <- entries %>%
  #   str_replace_all("(\\n|\\s)*$", "") %>%
  #   str_extract("\\s\\w+$") %>%
  #   str_trim("both") %>%
  #   str_replace_all("^(seconds|minute|minutes|hour)$", "Not provided")

  categories <- archive_parsed %>%
    html_nodes("#entry img") %>%
    html_attr("alt")

  # topics <- entries %>%
  #   str_extract(".* •") %>%
  #   str_replace_all("•", "") %>%
  #   str_replace_all("^\\s$", "Not provided") %>%
  #   str_trim("both")

  topics <- archive_parsed %>%
    html_nodes(".postdate+ .postdate") %>%
    html_text() %>%
    str_extract("•.*") %>%
    str_replace_all("•", "")  %>%
    str_trim("both")

  result <- data_frame(number = epnums, title = titles, summary = summaries,
                       category = categories, topic = topics)
  return(result)
}

get_podcast_segment_episodes <- function(){
  require(rvest)

  segments_incomparable <- list(list(partial = "oldmovieclub", name = "Old Movie Club"),
                           list(partial = "rocketsurgery", name = "Rocket Surgery"),
                           list(partial = "comicbookclub", name = "Comic Book Club"),
                           list(partial = "bookclub", name = "Book Club"))

  segments_gameshow <- list(list(partial = "counterclockwise", name = "Counterclockwise"),
                       list(partial = "gamenight", name = "Game Night"),
                       list(partial = "inconceivable", name = "Inconceivable!"),
                       list(partial = "lowdef", name = "Low Definition"),
                       list(partial = "turnsout", name = "Turns Out"),
                       list(partial = "pundit", name = "Pundit Showdown"))

  segments_teevee <- list(list(partial = "arrow", name = "Arrow"),
                          list(partial = "daredevil", name = "Daredevil"),
                          list(partial = "doctorwho", name = "Doctor Who!"),
                          list(partial = "gameofthrones", name = "Game of Thrones"),
                          list(partial = "legends", name = "Legends of Tomorrow"),
                          list(partial = "jessicajones", name = "Jessica Jones"),
                          list(partial = "sonsofanarchy", name = "Sons of Anarchy"),
                          list(partial = "expanse", name = "The Expanse"),
                          list(partial = "flash", name = "The Flash"),
                          list(partial = "truedetective", name = "True Detective"))

  inc <-  plyr::ldply(segments_incomparable, function(segment){
          url <- paste("https://www.theincomparable.com/theincomparable", segment$partial, "archive", sep = "/")
          entry  <- read_html(url) %>% html_nodes(".entry-title a")
          title  <- entry %>% html_text()
          epnums <- entry %>% html_attr("href") %>% str_extract("\\d+")
          data.frame(number = epnums, segment = segment$name, podcast = "The Incomparable")
        })

  gs <-  plyr::ldply(segments_gameshow, function(segment){
            url <- paste("https://www.theincomparable.com/gameshow", segment$partial, "archive", sep = "/")
            entry  <- read_html(url) %>% html_nodes(".entry-title a")
            title  <- entry %>% html_text()
            epnums <- entry %>% html_attr("href") %>% str_extract("\\d+")
            data.frame(number = epnums, segment = segment$name, podcast = "Game Show")
          })

  teevee <-  plyr::ldply(segments_teevee, function(segment){
                url <- paste("https://www.theincomparable.com/teevee", segment$partial, "archive", sep = "/")
                entry  <- read_html(url) %>% html_nodes(".entry-title a")
                title  <- entry %>% html_text()
                epnums <- entry %>% html_attr("href") %>% str_extract("\\d+")
                data.frame(number = epnums, segment = segment$name, podcast = "TeeVee")
              })

  ret <- bind_rows(inc, gs, teevee)
  return(ret)
}
