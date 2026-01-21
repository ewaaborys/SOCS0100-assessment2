# SOCS0100-assessment2
Are Songs Really Getting Shorter in Recent Decades? Computational Analysis of Music Metadata Using APIs and Shiny

This repository contains a **computational social science** project that tests whether recorded music tracks have become shorter between **2000–2025**, using **MusicBrainz** metadata collected via its public API. 

It includes:
- a reproducible **API collection pipeline** (R scripts),
- a **cleaning/wrangling pipeline** that produces analysis-ready datasets,
- an **interactive R Shiny dashboard** (`app.R`),
- a **Quarto report** (`SOCS0100-report.qmd`) and its rendered HTML output.


## Project structure
├─ README.md
├─ SOCS0100-assessment2.Rproj
├─ SOCS0100-report.qmd
├─ SOCS0100-report.html
├─ SOCS0100_report_files/          # Quarto HTML assets (auto-generated)
├─ app.R                           # Shiny dashboard (runApp)
├─ R/
│  ├─ 01_collect_data.R            # API data collection (MusicBrainz WS/2)
│  ├─ 02_clean_data.R              # cleaning + units of analysis + Shiny aggregates
│  └─ helpers.R                    # helper functions (mb_get, classify_era, etc.)
└─ data/
├─ raw/                         # created by 01_collect_data.R
│  └─ musicbrainz_raw.csv
└─ final/                       # created by 02_clean_data.R (inputs for Shiny)
├─ musicbrainz_clean_unique.csv
├─ musicbrainz_clean_tagged.csv
├─ musicbrainz_yearly_overall.csv
├─ musicbrainz_yearly_by_era.csv
├─ musicbrainz_yearly_by_cluster.csv
├─ musicbrainz_summary_era.csv
└─ musicbrainz_summary_cluster.csv


## Reproducibility notes
- Run from project root (via the .Rproj) so relative paths resolve.
- MusicBrainz rate-limiting: the collection script uses pauses to avoid excessive requests.
- Sampling reproducibility: offset sampling is fixed with set.seed(122).
- If you re-run the collection script, results can change if MusicBrainz content/tags change over time (expected for live community databases).


---

## Research questions 

- **RQ1:** How has track length changed over time (2000–2025)?
- **RQ2:** Does track duration vary across three sociotechnical eras?
- **RQ3:** Do different genres (clusters / tags) exhibit distinct track-length trends?

## Key Findings

- **RQ1:** Track length declines over time between 2000 and 2025, with a pronounced drop after the mid-2010s.
- **RQ2:** Track durations remain stable before 2017 but shorten sharply in the post-2017 “Dual Algorithmization” era.
- **RQ3:** Shortening is most pronounced in Mainstream/Commercial genres, while Underground/Alternative and Classical/Structured genres show weaker or inconsistent trends.

---

## Data source

- **MusicBrainz Web Service (WS/2)** — public REST-style API.
- The collection script queries the **recording** endpoint and requests **JSON** responses (e.g., `fmt=json`), then extracts track metadata including duration and first release date.

Sampling design highlights (see `R/01_collect_data.R`):
- tag-based sampling across three analytical clusters (Mainstream/Commercial, Underground/Alternative, Structured/Classical),
- time window restricted to **2000–2025**,
- **reproducible offset sampling** (`set.seed(122)`) to reduce “top-of-search” bias,
- “polite” API use (rate limiting via `Sys.sleep`, identifiable user agent).

---

## Quick start (recommended order)

> **Note:** The report is written to render quickly, so the API collection script is not run inside the Quarto document. Data collection and cleaning are executed via the standalone scripts in `/R`.

### 1) Open the project
Open `SOCS0100-assessment2.Rproj` so relative paths work correctly.

### 2) Install packages
All scripts use `pacman::p_load(...)` to install/load required packages automatically.  
If `pacman` is not installed, scripts install it first.

### 3) Collect raw data (API)
Run:
- `R/01_collect_data.R`

Expected output:
- `data/raw/musicbrainz_raw.csv`

This script:
- builds MusicBrainz search queries (Lucene syntax),
- sends requests via `mb_get()` (defined in `R/helpers.R`),
- extracts key fields,
- applies light bounds checks  
- saves a raw CSV checkpoint.

### 4) Clean + create analysis datasets
Run:
- `R/02_clean_data.R`

Expected outputs (saved to `data/final/`):
- `musicbrainz_clean_unique.csv` (one row per `recording_id`; used for RQ1/RQ2)
- `musicbrainz_clean_tagged.csv` (one row per `recording_id × genre_tag`; used for RQ3)
- aggregated tables for fast Shiny plotting (yearly + summaries)

**Important implementation detail:**  
Factors (era/cluster ordering) are defined during cleaning for consistent labels. Because outputs are saved as CSV (which drops factor classes), the Shiny app re-applies factor levels after loading to ensure stable ordering and legends.

### 5) Run the Shiny dashboard
From the project root, run in R:
```r
shiny::runApp("app.R")
```
Interactive dashboard:
- loads data/final/ outputs,
- re-applies factor ordering (CSV does not store factor classes),
- provides global filters (year range, cluster, tag, median/mean),
- renders three interactive visualisations aligned to RQ1–RQ3.


### 6) Rendering the report (Quarto)
```r
quarto::quarto_render("SOCS0100-report.qmd")
```

## Analytical Limitations

Two analytical limitations should be acknowledged:

- **Historical catalogue bias:** Earlier years contain more recordings due to longer accumulation periods. This motivates the use of medians, year-level aggregation, and era-based comparisons rather than raw counts.

- **Genre labels as sampling handles:** MusicBrainz genre tags are community-generated and not objective classifications, the same with my own cluster calssification. For this reason, cluster-level patterns are emphasised, and tag-level results are treated as exploratory.


## **Overall assessment on Copilot engagement**

Copilot was most useful for scaffolding and debugging, particularly when resolving inconsistent naming and Shiny reactivity issues. 
However, its suggestions were often syntactically plausible but analytically risky, requiring careful review. 
The key learning outcome was developing a habit of auditing Copilot’s outputs against research design, reproducibility, and data integrity constraints. 
Overall, Copilot accelerated development but reinforced the necessity of human methodological judgement in computational social science workflows.



# References (used in the report)
Prey, R. (2020). Locating power in platformization: Music streaming playlists and curatorial power. Social Media + Society, 6(3). https://doi.org/10.1177/2056305120933291
Sonmez, B. (2025). SOCS0100 lecture materials
GitHub Copilot was used as a coding assistance tool for scaffolding and debugging, with all outputs critically evaluated and modified by the author.



## Sources & Inspiration

The overall project structure and Shiny application design were informed by SOCS0100 lecture materials on reproducibility and interactivity (Weeks 5–6), particularly the emphasis on separating data collection, wrangling, and visualisation into distinct, reproducible stages.

The Shiny dashboard implementation was guided by official Posit Shiny tutorials and reference documentation, especially regarding reactive expressions, tabbed layouts, and performance considerations. These sources informed design patterns but no external code was copied verbatim; all scripts were written specifically for this project.

Automated data collection was informed by the MusicBrainz Web Service (WS/2) documentation:
https://musicbrainz.org/doc/MusicBrainz_API








