# app.R ----------------------------------------------------------------
# Shiny App: Track length over time (MusicBrainz)
# Reads cleaned + aggregated datasets created in R/02_clean_data.R

rm(list = ls())

# ---- Setup ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  shiny, ggplot2, dplyr, readr, stringr, forcats, scales
)

# ---- Defensive: check required files exist ----
req_files <- c(
  "data/final/musicbrainz_clean_unique.csv",
  "data/final/musicbrainz_clean_tagged.csv",
  "data/final/musicbrainz_yearly_by_cluster.csv"
)

missing_files <- req_files[!file.exists(req_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required file(s):\n- ",
    paste(missing_files, collapse = "\n- "),
    "\n\nRun R/02_clean_data.R to generate the data/final/ outputs."
  )
}

# ---- Load data (relative paths) ----
clean_unique <- readr::read_csv("data/final/musicbrainz_clean_unique.csv", show_col_types = FALSE)
clean_tagged <- readr::read_csv("data/final/musicbrainz_clean_tagged.csv", show_col_types = FALSE)
yearly_by_cluster <- readr::read_csv("data/final/musicbrainz_yearly_by_cluster.csv", show_col_types = FALSE)

# ---- Defensive validation for Shiny (UI safety) ----
# Data are already cleaned in R/02_clean_data.R;
# this check prevents reactive failures if filters or inputs change.
clean_unique <- clean_unique %>%
  filter(!is.na(release_year), !is.na(length_sec), length_sec > 0)

clean_tagged <- clean_tagged %>%
  filter(!is.na(release_year), !is.na(length_sec), length_sec > 0)

min_year <- min(clean_unique$release_year, na.rm = TRUE)
max_year <- max(clean_unique$release_year, na.rm = TRUE)

# ---- Standardise genre_cluster labels (solve a small mistake: inconsistent naming) ----
standardise_cluster <- function(x) {
  x <- stringr::str_squish(as.character(x))
  
  # Map old/alternate naming -> final naming used everywhere in the app
  x <- dplyr::recode(
    x,
    "Commercial/Mainstream" = "Mainstream/Commercial",
    .default = x
  )
  
  x
}

clean_unique <- clean_unique %>%
  mutate(genre_cluster = standardise_cluster(genre_cluster))

clean_tagged <- clean_tagged %>%
  mutate(genre_cluster = standardise_cluster(genre_cluster))

yearly_by_cluster <- yearly_by_cluster %>%
  mutate(genre_cluster = standardise_cluster(genre_cluster))

# ---- Factor ordering (stable legends + consistent plots) ----
# CSV drops factor classes; reapply factor levels here for stable ordering/legends

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

cluster_palette <- c(
  "Mainstream/Commercial"    = "#1F77B4",
  "Underground/Alternative"  = "#2CA02C",
  "Classical/Structured"     = "#FF7F0E",
  "Other"                    = "grey50"
)

clean_unique <- clean_unique %>%
  mutate(
    era = factor(era, levels = era_levels, ordered = TRUE),
    genre_cluster = factor(genre_cluster, levels = cluster_levels)
  )

clean_tagged <- clean_tagged %>%
  mutate(
    era = factor(era, levels = era_levels, ordered = TRUE),
    genre_cluster = factor(genre_cluster, levels = cluster_levels)
  )

yearly_by_cluster <- yearly_by_cluster %>%
  mutate(genre_cluster = factor(genre_cluster, levels = cluster_levels))

# ---- UI ----
ui <- fluidPage(
  titlePanel("Are Tracks Getting Shorter? MusicBrainz Track Length Explorer (2000–2025)"),
  
  sidebarLayout(
    sidebarPanel(
      tags$p(
        style = "color:#666;",
        "Filters update all plots. Use Tag to drill down within a cluster."
      ),
      
      sliderInput(
        inputId = "year_range",
        label   = "Year range",
        min     = min_year,
        max     = max_year,
        value   = c(min_year, max_year),
        step    = 1,
        sep     = ""
      ),
      
      selectInput(
        inputId = "cluster",
        label   = "Genre cluster",
        choices = c("All", cluster_levels),
        selected = "All"
      ),
      
      selectInput(
        inputId = "tag",
        label   = "Specific genre tag (optional drill-down)",
        choices = c("All", sort(unique(clean_tagged$genre_tag))),
        selected = "All"
      ),
      
      radioButtons(
        inputId = "stat",
        label   = "Summary statistic",
        choices = c("Median" = "median_len", "Mean" = "mean_len"),
        selected = "median_len",
        inline = TRUE
      ),
      
      checkboxInput(
        inputId = "show_points",
        label   = "Show yearly points on trend lines",
        value   = TRUE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "RQ1: Trend over time",
          br(),
          tags$div(
            style = "color:#666; font-size: 0.95em; margin-bottom: 10px;",
            "Shows how typical track length changes by year. ",
            "This plot respects the current filters (year range + cluster + tag)."
          ),
          plotOutput("plot_overall", height = 380),
          br(),
          tags$h5("Summary of current filter"),
          tableOutput("tbl_summary_overall")
        ),
        
        tabPanel(
          "RQ2: Era comparison",
          br(),
          tags$div(
            style = "color:#666; font-size: 0.95em; margin-bottom: 10px;",
            "Compares track-length distributions across sociotechnical eras. ",
            "Filtering by cluster helps check whether patterns differ by musical context."
          ),
          plotOutput("plot_era", height = 420)
        ),
        
        tabPanel(
          "RQ3: Genre clusters / tags",
          br(),
          tags$div(
            style = "color:#666; font-size: 0.95em; margin-bottom: 10px;",
            "Compares trends across clusters. Select a tag to drill down and see if a specific scene shortens faster."
          ),
          plotOutput("plot_cluster_trend", height = 440)
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Update tag choices dynamically (based on cluster)
  observeEvent(input$cluster, {
    tags_available <- clean_tagged %>%
      {
        if (input$cluster == "All") .
        else filter(., genre_cluster == input$cluster)
      } %>%
      pull(genre_tag) %>%
      unique() %>%
      sort()
    
    updateSelectInput(
      session,
      "tag",
      choices = c("All", tags_available),
      selected = "All"
    )
  }, ignoreInit = TRUE)
  
  # Filter: unique unit (best for RQ1/RQ2)
  data_unique_f <- reactive({
    df <- clean_unique %>%
      filter(
        release_year >= input$year_range[1],
        release_year <= input$year_range[2]
      )
    
    if (input$cluster != "All") {
      df <- df %>% filter(genre_cluster == input$cluster)
    }
    
    # Tag drill-down reflected in unique unit (via recording_id membership)
    if (input$tag != "All") {
      ids <- clean_tagged %>%
        filter(
          release_year >= input$year_range[1],
          release_year <= input$year_range[2],
          genre_tag == input$tag
        ) %>%
        pull(recording_id) %>%
        unique()
      
      df <- df %>% filter(recording_id %in% ids)
    }
    
    df
  })
  
  # Filter: tagged unit (best for RQ3 and tag drill-down)
  data_tagged_f <- reactive({
    df <- clean_tagged %>%
      filter(
        release_year >= input$year_range[1],
        release_year <= input$year_range[2]
      )
    
    if (input$cluster != "All") {
      df <- df %>% filter(genre_cluster == input$cluster)
    }
    
    if (input$tag != "All") {
      df <- df %>% filter(genre_tag == input$tag)
    }
    
    df
  })
  
  # Small summary table (matches current filters)
  output$tbl_summary_overall <- renderTable({
    df <- data_unique_f()
    
    data.frame(
      n_tracks   = nrow(df),
      min_year   = ifelse(nrow(df) == 0, NA, min(df$release_year, na.rm = TRUE)),
      max_year   = ifelse(nrow(df) == 0, NA, max(df$release_year, na.rm = TRUE)),
      mean_len_s = ifelse(nrow(df) == 0, NA, round(mean(df$length_sec, na.rm = TRUE), 1)),
      median_s   = ifelse(nrow(df) == 0, NA, round(median(df$length_sec, na.rm = TRUE), 1))
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  
  # ----------------------------
  # Plot 1: Overall trend (RQ1) — recomputed from filtered data
  # ----------------------------
  output$plot_overall <- renderPlot({
    stat_col <- input$stat
    
    df <- data_unique_f() %>%
      group_by(release_year) %>%
      summarise(
        n = n(),
        median_len = median(length_sec, na.rm = TRUE),
        mean_len   = mean(length_sec, na.rm = TRUE),
        .groups = "drop"
      )
    
    validate(need(nrow(df) > 1, "Not enough data in the current filter to plot a trend."))
    
    p <- ggplot(df, aes(x = release_year, y = .data[[stat_col]])) +
      geom_line(linewidth = 1.1, colour = "#1F4E79") +
      labs(
        x = NULL,
        y = ifelse(stat_col == "median_len", "Median length (seconds)", "Mean length (seconds)")
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      theme_minimal(base_size = 12)
    
    if (isTRUE(input$show_points)) {
      p <- p + geom_point(size = 1.9, colour = "#1F4E79")
    }
    
    p
  })
  
  # -------------------------------------
  # Plot 2: Era distributions (RQ2)
  # -------------------------------------
  output$plot_era <- renderPlot({
    df <- data_unique_f()
    validate(need(nrow(df) > 10, "Not enough data in the current filter to plot distributions."))
    
    ylims <- quantile(df$length_sec, probs = c(0.02, 0.98), na.rm = TRUE)
    
    ggplot(df, aes(x = era, y = length_sec, fill = era)) +
      geom_boxplot(outlier.alpha = 0.15, width = 0.6) +
      coord_cartesian(ylim = ylims) +
      scale_fill_manual(values = c("#A7C7E7", "#FFD6A5", "#FFADAD")) +
      labs(x = NULL, y = "Track length (seconds)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })
  
  # -------------------------------------------------
  # Plot 3: Cluster trend + optional tag drill-down
  # -------------------------------------------------
  output$plot_cluster_trend <- renderPlot({
    stat_col <- input$stat
    
    # Tag drill-down: one line
    if (input$tag != "All") {
      df <- data_tagged_f() %>%
        group_by(release_year) %>%
        summarise(
          median_len = median(length_sec, na.rm = TRUE),
          mean_len   = mean(length_sec, na.rm = TRUE),
          n          = n(),
          .groups = "drop"
        )
      
      validate(need(nrow(df) > 1, "Not enough data for this tag in the current year range."))
      
      p <- ggplot(df, aes(x = release_year, y = .data[[stat_col]])) +
        geom_line(linewidth = 1.1, colour = "#6A1B9A") +
        labs(
          x = NULL,
          y = ifelse(stat_col == "median_len", "Median length (seconds)", "Mean length (seconds)"),
          subtitle = paste0('Tag: "', input$tag, '" | total n = ', sum(df$n))
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks(8)) +
        theme_minimal(base_size = 12)
      
      if (isTRUE(input$show_points)) {
        p <- p + geom_point(size = 1.8, colour = "#6A1B9A")
      }
      
      return(p)
    }
    
    # Compare clusters (use the CSV, but now labels are standardised)
    df <- yearly_by_cluster %>%
      filter(
        release_year >= input$year_range[1],
        release_year <= input$year_range[2]
      )
    
    if (input$cluster != "All") {
      df <- df %>% filter(genre_cluster == input$cluster)
    }
    
    validate(need(nrow(df) > 1, "Not enough data in the current filter to plot cluster trends."))
    
    ggplot(df, aes(x = release_year, y = .data[[stat_col]], colour = genre_cluster)) +
      geom_line(linewidth = 1.1) +
      labs(
        x = NULL,
        y = ifelse(stat_col == "median_len", "Median length (seconds)", "Mean length (seconds)"),
        colour = "Genre cluster"
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_colour_manual(values = cluster_palette, drop = TRUE) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom") +
      { if (isTRUE(input$show_points)) geom_point(size = 1.7) else NULL }
  })
}

shinyApp(ui, server)



