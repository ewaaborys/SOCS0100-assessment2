## R/helpers.R -----------------------------------------------------------------
# Utility functions for MusicBrainz API collection  
#
# Design goals:
#  - Keep scraping logic reusable (functions used repeatedly in 01_collect_data.R)
#  - Keep outputs predictable (so downstream wrangling + Shiny are smooth)
#  - Be polite to the API (rate limiting + clear user agent)
#
# Note:
#  - You CAN load packages inside helpers, but a cleaner pattern is:
#      pacman::p_load(...) in each script + source("R/helpers.R")
#  - Below keeps dependencies minimal and explicit.

# ------------------------------------------------------------------------------
# 0) SETUP 
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
})

# ------------------------------------------------------------------------------
# 1) CORE API FUNCTION
# ------------------------------------------------------------------------------

# mb_get():
#  - Sends a GET request to MusicBrainz WS/2
#  - Sleeps first to respect rate limiting (important for public APIs)
#  - Parses JSON into an R list/data.frame structure
#
# Reuse:
#  - Called many times in 01_collect_data.R: once for count, then per page.
mb_get <- function(url, query = list(), pause_sec = 1.1) {
  Sys.sleep(pause_sec)
  
  resp <- httr::GET(
    url = url,
    query = query,
    httr::user_agent("SOCS0100-UCL-Student/1.0 (Track Length Analysis)")
  )
  
  httr::stop_for_status(resp)
  
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
}

# ------------------------------------------------------------------------------
# 2) SAFE YEAR EXTRACTION
# ------------------------------------------------------------------------------

# safe_year():
#  - MusicBrainz dates can be "YYYY", "YYYY-MM", or "YYYY-MM-DD"
#  - For time-trend analysis we only need the year
#  - Returns integer year or NA
#
# Reuse:
#  - Applied to every fetched recording in the collection script.
safe_year <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  suppressWarnings(as.integer(substr(x, 1, 4)))
}

# ------------------------------------------------------------------------------
# 3) ARTIST NAME EXTRACTION (robust to nested structures)
# ------------------------------------------------------------------------------

# get_first_artist_name():
#  - MusicBrainz returns "artist-credit" as a nested structure
#  - For this project we extract the first credited artist as a single label
#  - Handles common shapes: list, data.frame, atomic vectors
#
# Reuse:
#  - Applied to every recording we keep.
get_first_artist_name <- function(ac) {
  if (is.null(ac)) return(NA_character_)
  if (is.atomic(ac)) return(as.character(ac[1]))
  
  # jsonlite often turns nested lists into data.frames
  if (is.data.frame(ac)) {
    if ("name" %in% names(ac) && !is.na(ac$name[1])) return(as.character(ac$name[1]))
    if ("artist.name" %in% names(ac) && !is.na(ac$`artist.name`[1])) {
      return(as.character(ac$`artist.name`[1]))
    }
    return(NA_character_)
  }
  
  # list case
  if (is.list(ac) && length(ac) > 0) {
    first <- ac[[1]]
    
    if (is.atomic(first)) return(as.character(first[1]))
    
    if (is.list(first)) {
      if (!is.null(first$name)) return(as.character(first$name))
      
      # sometimes nested under $artist$name
      if (!is.null(first$artist) && is.list(first$artist) && !is.null(first$artist$name)) {
        return(as.character(first$artist$name))
      }
    }
  }
  
  NA_character_
}

# ------------------------------------------------------------------------------
# 4) ERA CLASSIFICATION (3-era sociotechnical framing)
# ------------------------------------------------------------------------------

# classify_era():
#  - Takes integer year
#  - Returns an era label used for RQ2 comparisons
#  - This is an *analytical framing* (not a claim of causality)
#
# Reuse:
#  - Used in collection output AND later wrangling, so Shiny + Quarto agree.
classify_era <- function(year) {
  dplyr::case_when(
    is.na(year) ~ NA_character_,
    year >= 2000 & year <= 2012 ~ "Pre-Convergence (2000–2012)",
    year >= 2013 & year <= 2016 ~ "Platform Emergence (2013–2016)",
    year >= 2017 ~ "Dual Algorithmization (2017+)",
    TRUE ~ NA_character_
  )
}








