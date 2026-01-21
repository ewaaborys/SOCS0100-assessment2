## R/02_clean_data.R ------------------------------------------------------------
#Wrangling & Context #################
# Purpose (what this script produces):
#  1) Cleans the raw API output into analysis-ready data
#  2) Creates two “units of analysis”:
#       - clean_unique: one row per recording_id (best for RQ1/RQ2 trends)
#       - clean_tagged: one row per recording_id x genre_tag (best for RQ3 comparisons)
#  3) Produces fast, aggregated tables for Shiny (yearly medians/means)
#  4) Saves everything to data/final/ so app.R can load it with relative paths
################################################################################

rm(list = ls())

# ------------------------------------------------------------------------------
# 0) Setup
# ------------------------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  dplyr, readr, stringr, tibble, purrr, lubridate,
  forcats, skimr, knitr
)

# Import helper functions (classify_era() etc.)
source(file.path("R", "helpers.R"))

# ------------------------------------------------------------------------------
# 1) Read raw data (output of 01_collect_data.R)
# ------------------------------------------------------------------------------
in_path <- "data/raw/musicbrainz_raw.csv"

if (!file.exists(in_path)) {
  stop(
    "Raw data file not found at: ", in_path, "\n",
    "Run R/01_collect_data.R first (it creates data/raw/musicbrainz_raw.csv)."
  )
}

raw <- readr::read_csv(in_path, show_col_types = FALSE)

message("✓ Raw dataset loaded: ", in_path)
message("Rows: ", nrow(raw), " | Cols: ", ncol(raw))
message("Unique recordings: ", dplyr::n_distinct(raw$recording_id))

# Quick overview (useful for report narrative)
skimr::skim(raw)

# ------------------------------------------------------------------------------
# 2) Variable dictionary (for “exhaustive overview of dataset”)
# ------------------------------------------------------------------------------
var_dict <- tibble::tibble(
  variable = c(
    "recording_id", "title", "artist_name",
    "length_ms", "length_sec",
    "first_release_date", "release_year",
    "genre_tag", "genre_cluster", "era"
  ),
  type = c(
    "character", "character", "character",
    "numeric", "numeric",
    "character", "integer",
    "character", "factor", "ordered factor"
  ),
  description = c(
    "MusicBrainz recording identifier (track-level ID).",
    "Track title from the API (cleaned with str_squish).",
    "First credited artist name (extracted from artist-credit).",
    "Track duration in milliseconds (MusicBrainz length field).",
    "Track duration in seconds (length_ms / 1000).",
    "First release date returned for the recording (string).",
    "Year extracted from first_release_date (used for trends).",
    "Genre tag used as the search query (sampling label).",
    "Cluster mapping of genre_tag (Mainstream/Commercial, Underground/Alternative, Classical/Structured).",
    "3-era sociotechnical grouping derived from release_year."
  )
)

message("\n--- Variable dictionary (for report Part I-B) ---")
print(knitr::kable(var_dict))

# ------------------------------------------------------------------------------
# 3) Core cleaning (make data Shiny-friendly + analysis-consistent)
# ------------------------------------------------------------------------------

# Stable labels for plotting + Shiny dropdowns
era_levels <- c(
  "Pre-Convergence (2000–2012)",
  "Platform Emergence (2013–2016)",
  "Dual Algorithmization (2017+)"
)

cluster_levels <- c(
  "Mainstream/Commercial",
  "Underground/Alternative",
  "Classical/Structured",
  "Other"
)

# Standardise cluster names BEFORE making them a factor.
# This prevents Mainstream/Commercial being turned into NA (and then filtered out).
standardise_cluster <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x <- dplyr::if_else(is.na(x) | x == "", "Other", x)
  
  dplyr::recode(
    x,
    "Commercial/Mainstream" = "Mainstream/Commercial",  # old label -> new label
    "Structured/Classical"  = "Classical/Structured",   # just in case old naming appears
    .default = x
  )
}

clean_tagged <- raw %>%
  mutate(
    # ---- Text cleaning ----
    title       = stringr::str_squish(title),
    artist_name = stringr::str_squish(artist_name),
    
    # ---- Type enforcement ----
    release_year = as.integer(release_year),
    length_ms    = as.numeric(length_ms),
    length_sec   = as.numeric(length_sec),
    
    # ---- Recompute era for consistency ----
    era = classify_era(release_year),
    era = factor(era, levels = era_levels, ordered = TRUE),
    
    # ---- Fix + order cluster labels (prevents mainstream dropping) ----
    genre_cluster = standardise_cluster(genre_cluster),
    genre_cluster = factor(genre_cluster, levels = cluster_levels)
  ) %>%
  # ---- Defensive filtering: keep only valid analytic cases ----
filter(
  !is.na(recording_id),
  !is.na(title) & title != "",
  !is.na(release_year),
  between(release_year, 2000, 2025),
  !is.na(length_sec),
  between(length_sec, 30, 1800),
  !is.na(genre_tag) & genre_tag != "",
  !is.na(era),
  !is.na(genre_cluster)     # safe now, because mainstream is no longer turned into NA
) %>%
  distinct(recording_id, genre_tag, .keep_all = TRUE)

message("\n✓ Tagged unit ready (recording_id x genre_tag).")
message("Rows: ", nrow(clean_tagged), " | Unique recordings: ", n_distinct(clean_tagged$recording_id))

# ------------------------------------------------------------------------------
# 4) Unique unit for RQ1/RQ2 (avoid double-counting tracks across tags)
# ------------------------------------------------------------------------------
clean_unique <- clean_tagged %>%
  arrange(recording_id, genre_tag) %>%  # stable selection rule
  group_by(recording_id) %>%
  slice(1) %>%
  ungroup()

message("\n✓ Unique unit ready (one row per recording_id).")
message("Rows: ", nrow(clean_unique), " | Unique recordings: ", n_distinct(clean_unique$recording_id))

# ------------------------------------------------------------------------------
# 5) Descriptive summaries (use in Part I-B “contextualisation”)
# ------------------------------------------------------------------------------
summary_overall <- clean_unique %>%
  summarise(
    n_tracks   = n(),
    min_year   = min(release_year),
    max_year   = max(release_year),
    mean_len   = mean(length_sec),
    median_len = median(length_sec),
    p25_len    = quantile(length_sec, 0.25),
    p75_len    = quantile(length_sec, 0.75)
  )

summary_era <- clean_unique %>%
  group_by(era) %>%
  summarise(
    n          = n(),
    mean_len   = mean(length_sec),
    median_len = median(length_sec),
    .groups = "drop"
  )

summary_cluster <- clean_tagged %>%
  group_by(genre_cluster) %>%
  summarise(
    n          = n(),
    mean_len   = mean(length_sec),
    median_len = median(length_sec),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

message("\n--- Overall descriptives (unique unit) ---")
print(summary_overall)

message("\n--- Summary by era (unique unit) ---")
print(summary_era)

message("\n--- Summary by genre cluster (tagged unit) ---")
print(summary_cluster)

# ------------------------------------------------------------------------------
# 6) Aggregations for Shiny (fast plotting tables)
# ------------------------------------------------------------------------------
yearly_overall <- clean_unique %>%
  group_by(release_year) %>%
  summarise(
    n          = n(),
    mean_len   = mean(length_sec),
    median_len = median(length_sec),
    .groups = "drop"
  )

yearly_by_era <- clean_unique %>%
  group_by(release_year, era) %>%
  summarise(
    n          = n(),
    mean_len   = mean(length_sec),
    median_len = median(length_sec),
    .groups = "drop"
  )

yearly_by_cluster <- clean_tagged %>%
  group_by(release_year, genre_cluster) %>%
  summarise(
    n          = n(),
    mean_len   = mean(length_sec),
    median_len = median(length_sec),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 7) Save cleaned + aggregated datasets
# ------------------------------------------------------------------------------
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)

readr::write_csv(clean_tagged,      "data/final/musicbrainz_clean_tagged.csv")
readr::write_csv(clean_unique,      "data/final/musicbrainz_clean_unique.csv")
readr::write_csv(yearly_overall,    "data/final/musicbrainz_yearly_overall.csv")
readr::write_csv(yearly_by_era,     "data/final/musicbrainz_yearly_by_era.csv")
readr::write_csv(yearly_by_cluster, "data/final/musicbrainz_yearly_by_cluster.csv")
readr::write_csv(summary_era,       "data/final/musicbrainz_summary_era.csv")
readr::write_csv(summary_cluster,   "data/final/musicbrainz_summary_cluster.csv")

message("\n✓ Saved cleaned + aggregated datasets to data/final/")
message("  - musicbrainz_clean_tagged.csv (RQ3 unit: recording_id x genre_tag)")
message("  - musicbrainz_clean_unique.csv (RQ1/RQ2 unit: one row per recording)")
message("  - musicbrainz_yearly_overall.csv")
message("  - musicbrainz_yearly_by_era.csv")
message("  - musicbrainz_yearly_by_cluster.csv")
message("  - musicbrainz_summary_era.csv")
message("  - musicbrainz_summary_cluster.csv")

