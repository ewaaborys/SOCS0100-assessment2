## R/01_collect_data.R ----------------------------------------------------------
#Automated Data Collection ####################
#MusicBrainz API (WS/2) #################################
# Goal:
#   Build a Shiny-friendly dataset to analyse:
#   - RQ1: track length over time
#   - RQ2: differences across platform eras (derived from year)
#   - RQ3: differences across genre clusters (operationalised via query tags)
#
# Output:
#   data/raw/musicbrainz_raw.csv
#
# Notes (reproducibility):
#   - Uses relative paths (assumes project root as working directory).
#   - Respects MusicBrainz rate limiting via pause_sec in mb_get().
#   - Uses set.seed() so the random sampling offsets are reproducible.
################################################################################

# ------------------------------------------------------------------------------
# 0) SETUP 
# ------------------------------------------------------------------------------

rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  dplyr, purrr, stringr, readr, tibble,
  httr, jsonlite
)

# Helper functions expected in R/helpers.R:
#   mb_get(), safe_year(), get_first_artist_name(), classify_era()
source("R/helpers.R")

# ------------------------------------------------------------------------------
# 1) PARAMETERS
# ------------------------------------------------------------------------------

# Genre-tag "universe" (used for sampling).
tags_mainstream <- c(
  "pop", "dance-pop", "edm", "electronic", "dance", "hip hop", "trap",
  "hyperpop", "cloud rap", "plugg", "lo-fi hip hop", "bedroom pop",
  "synthwave", "vaporwave", "chillwave", "emo rap"
)

tags_underground <- c(
  "experimental", "noise", "black metal", "industrial", "post-punk",
  "punk rock", "hardcore punk", "power electronics", "glitch", "idm",
  "dark ambient", "drone", "avant-garde", "noisegrind", "breakcore",
  "harsh noise", "experimental rock", "anarcho-punk", "crust punk"
)

tags_structured <- c(
  "classical", "jazz", "jazz fusion", "ambient", "folk", "acoustic",
  "progressive rock", "progressive metal", "art rock", "chamber",
  "symphony", "orchestral", "classical crossover", "contemporary classical"
)

# Map each tag to an analytical cluster label (used in plots + Shiny filters).
tag_map <- tibble::tibble(
  genre_tag = c(tags_mainstream, tags_underground, tags_structured),
  genre_cluster = c(
    rep("Mainstream/Commercial", length(tags_mainstream)),
    rep("Underground/Alternative", length(tags_underground)),
    rep("Structured/Classical", length(tags_structured)) #renamed later 
  )
)

# Time window (eras are derived later from release_year using classify_era()).
start_date <- "2000-01-01"
end_date   <- "2025-12-31"

# MusicBrainz recordings endpoint URL. 
base_url <- "https://musicbrainz.org/ws/2/recording/"

# API paging.
limit <- 100
pages_per_tag <- 4    # 4 pages x 100 limit ≈ up to 400 rows per tag (before filters)
pause_sec <- 1.1      # be polite to MB; 1s+ is a safe default

# Reproducible random paging to reduce "top of search" bias.
# (If FALSE, it always pulls the first pages, which can bias toward popular / well-tagged items.)
set.seed(122)
use_random_offsets <- TRUE

# ------------------------------------------------------------------------------
# 2) REUSABLE FUNCTIONS (Functional programming, used repeatedly)
# ------------------------------------------------------------------------------

# Build a Lucene query string for recordings:
# - tag:"X" focuses on a tag universe
# - firstreleasedate:[start TO end] restricts by time window
build_query <- function(tag, start_date, end_date) {
  sprintf('tag:"%s" AND firstreleasedate:[%s TO %s]', tag, start_date, end_date)
}

# Retrieve the total hit count for a query so we can sample offsets from the
# full result set (not just the beginning).
get_count <- function(q, base_url, pause_sec) {
  res <- mb_get(
    url = base_url,
    query = list(query = q, fmt = "json", limit = 1, offset = 0),
    pause_sec = pause_sec
  )
  if (!is.null(res$count)) return(as.integer(res$count))
  0L
}

# Choose offsets either randomly (recommended) or sequentially.
# This is a reusable “sampling strategy” function.
choose_offsets <- function(n_hits, limit, pages_per_tag, random = TRUE) {
  offsets <- seq(0, max(0, n_hits - 1), by = limit)
  if (length(offsets) == 0) return(integer(0))
  if (random) {
    offsets <- sample(offsets, size = min(length(offsets), pages_per_tag))
    offsets <- sort(offsets)
  } else {
    offsets <- offsets[seq_len(min(length(offsets), pages_per_tag))]
  }
  offsets
}

# Extract a Shiny-friendly tibble from the API response.
# (Keeps your loop clean and makes the extraction logic reproducible.)
extract_recordings <- function(rec, tag) {
  tibble::tibble(
    recording_id       = rec$id,
    title              = rec$title,
    length_ms          = rec$length,  # MusicBrainz duration is in milliseconds
    artist_name        = purrr::map_chr(rec$`artist-credit`, get_first_artist_name),
    first_release_date = rec$`first-release-date`,
    release_year       = safe_year(rec$`first-release-date`),
    genre_tag          = tag
  ) %>%
    mutate(
      title       = stringr::str_squish(title),
      artist_name = dplyr::if_else(is.na(artist_name) | artist_name == "", "Unknown", artist_name),
      length_sec  = length_ms / 1000
    ) %>%
    # Light “at-collection” cleaning so the dataset is immediately usable in Shiny.
    # (You still do fuller wrangling in Part I-B.)
    filter(
      !is.na(recording_id),
      !is.na(length_ms),
      length_ms > 0,
      !is.na(release_year),
      release_year >= 2000,
      release_year <= 2025,
      length_sec >= 30,     # drop ultra-short fragments
      length_sec <= 1800    # drop extreme outliers (30 min cap)
    ) %>%
    distinct(recording_id, genre_tag, .keep_all = TRUE)
}

# ------------------------------------------------------------------------------
# 3) COLLECTION LOOP (tag × sampled offsets)
# ------------------------------------------------------------------------------

rows <- list()

for (tag in tag_map$genre_tag) {
  
  q <- build_query(tag, start_date, end_date)
  
  message("====================================================")
  message("Tag: ", tag)
  message("Query: ", q)
  
  n_hits <- get_count(q, base_url = base_url, pause_sec = pause_sec)
  
  if (n_hits == 0) {
    message("No results found. Skipping.")
    next
  }
  
  offsets <- choose_offsets(
    n_hits = n_hits,
    limit = limit,
    pages_per_tag = pages_per_tag,
    random = use_random_offsets
  )
  
  if (length(offsets) == 0) {
    message("No offsets available. Skipping.")
    next
  }
  
  for (offset in offsets) {
    message("  Fetching offset: ", offset)
    
    res <- mb_get(
      url = base_url,
      query = list(query = q, fmt = "json", limit = limit, offset = offset),
      pause_sec = pause_sec
    )
    
    if (is.null(res$recordings) || length(res$recordings) == 0) next
    
    rec <- res$recordings
    
    # Convert response to clean rows
    df <- extract_recordings(rec, tag)
    
    rows[[length(rows) + 1]] <- df
    message("    Rows kept: ", nrow(df))
  }
}

# ------------------------------------------------------------------------------
# 4) COMBINE + LABEL (cluster + era) + SAVE
# ------------------------------------------------------------------------------

raw <- bind_rows(rows) %>%
  left_join(tag_map, by = "genre_tag") %>%
  mutate(
    genre_cluster = if_else(is.na(genre_cluster), "Other", genre_cluster),
    era = classify_era(release_year)
  ) %>%
  filter(!is.na(era)) %>%
  distinct(recording_id, genre_tag, .keep_all = TRUE)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
out_path <- "data/raw/musicbrainz_raw.csv"
readr::write_csv(raw, out_path)

# ------------------------------------------------------------------------------
# 5) QUICK CHECKS (printed to console; useful for debugging + report descriptives)
# ------------------------------------------------------------------------------

message("\n✓ Saved: ", out_path)
message("Rows: ", nrow(raw), " | Unique recordings: ", dplyr::n_distinct(raw$recording_id))

message("\nEra distribution:")
print(table(raw$era))

message("\nCluster distribution:")
print(table(raw$genre_cluster))

message("\nLength/year bounds:")
print(
  raw %>%
    summarise(
      min_year = min(release_year, na.rm = TRUE),
      max_year = max(release_year, na.rm = TRUE),
      min_len  = min(length_sec, na.rm = TRUE),
      max_len  = max(length_sec, na.rm = TRUE),
      median_len = median(length_sec, na.rm = TRUE)
    )
)

message("\nMissing checks (should be 0):")
print(sum(is.na(raw$release_year)))
print(sum(is.na(raw$length_sec)))