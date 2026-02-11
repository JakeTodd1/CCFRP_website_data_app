library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tidyr)

# ---- Region Lookup Table ----
# Maps each Area to its Region
region_lookup <- data.frame(
  Area = c(
    # South Coast (6 areas)
    "Swamis", "South La Jolla", "Point Conception", "Laguna Beach",
    "Carrington Point", "Anacapa Island",
    # Central Coast (4 areas)
    "Ano Nuevo", "Piedras Blancas", "Point Buchon", "Point Lobos",
    # North Coast (6 areas)
    "Bodega Head", "Southeast Farallon Islands", "South Cape Mendocino",
    "Stewarts Point", "Ten Mile", "Trinidad"
  ),
  Region = c(
    rep("South Coast", 6),
    rep("Central Coast", 4),
    rep("North Coast", 6)
  ),
  stringsAsFactors = FALSE
)

# ---- Load Data ----
# Load CPUE/BPUE data (each row = one species per grid cell per trip)
if (file.exists("data/2007-2024_CCFRP_derived_effort_table.csv")) {
  cpue_bpue_raw <- read.csv("data/2007-2024_CCFRP_derived_effort_table.csv", stringsAsFactors = FALSE)
} else {
  # Sample data for development/testing
  # Simulates the real data structure: each row = one species on one cell-trip
  set.seed(42)
  areas <- region_lookup$Area
  years <- 2007:2024
  mpa_status <- c("MPA", "REF")
  grid_cells <- paste0("GC", 1:5)
  trips <- paste0("T", 1:3)
  species <- c("Blue Rockfish", "Gopher Rockfish", "Black Rockfish",
               "Vermilion Rockfish", "Copper Rockfish")

  cpue_bpue_raw <- expand.grid(
    Area = areas,
    Year = years,
    MPA_Status = mpa_status,
    Grid_Cell_ID = grid_cells,
    Trip = trips,
    Common_Name = species,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      ID_Cell_per_Trip = paste(Area, Year, MPA_Status, Grid_Cell_ID, Trip, sep = "_"),
      CPUE_catch_per_angler_hour = round(runif(n(), 0.0, 2.0), 3),
      BPUE_biomass.kg._per_angler_hour = round(runif(n(), 0.0, 1.5), 3)
    ) %>%
    # Randomly remove some rows so not every species appears on every cell-trip
    slice_sample(prop = 0.6)
}

# Merge region info onto data
cpue_bpue_raw <- cpue_bpue_raw %>%
  left_join(region_lookup, by = "Area")

# Get unique values for filters
all_areas <- sort(unique(cpue_bpue_raw$Area))
all_regions <- sort(unique(region_lookup$Region))
all_species <- sort(unique(cpue_bpue_raw$Common_Name))

# ---- Load Length Data ----
# Each row = one individual fish measured
if (file.exists("data/2007-2024_CCFRP_derived_length_table.csv")) {
  length_raw <- read.csv("data/2007-2024_CCFRP_derived_length_table.csv", stringsAsFactors = FALSE)
} else {
  # Sample length data for development/testing
  # Simulates real structure: each row = one fish, with ID_Cell_per_Trip grouping
  set.seed(123)
  areas <- region_lookup$Area
  years <- 2007:2024
  mpa_status <- c("MPA", "REF")
  grid_cells <- paste0("GC", 1:5)
  trips <- paste0("T", 1:3)
  species <- c("Blue Rockfish", "Gopher Rockfish", "Black Rockfish",
               "Vermilion Rockfish", "Copper Rockfish")

  length_raw <- expand.grid(
    Area = areas,
    Year = years,
    MPA_Status = mpa_status,
    Grid_Cell_ID = grid_cells,
    Trip = trips,
    Common_Name = species,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      ID_Cell_per_Trip = paste(Area, Year, MPA_Status, Grid_Cell_ID, Trip, sep = "_")
    ) %>%
    # Each species-cell-trip combo produces a few fish
    slice(rep(1:n(), each = sample(1:5, n(), replace = TRUE))) %>%
    mutate(
      Length_cm = round(rnorm(n(), mean = 30, sd = 8), 1)
    )
}

length_raw <- length_raw %>%
  left_join(region_lookup, by = "Area")

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  titlePanel(
    div(
      h1("CCFRP Data Visualization Dashboard"),
      h4("California Collaborative Fisheries Research Program")
    )
  ),

  hr(),

  # ---- Filters ----
  fluidRow(
    column(3,
      selectInput("view_level", "View Level:",
                  choices = c("By Area" = "area",
                              "By Region" = "region",
                              "All Areas Combined" = "all"),
                  selected = "area")
    ),
    column(3,
      conditionalPanel(
        condition = "input.view_level == 'area'",
        selectInput("area_select", "Select Area:",
                    choices = all_areas,
                    selected = all_areas[1])
      ),
      conditionalPanel(
        condition = "input.view_level == 'region'",
        selectInput("region_select", "Select Region:",
                    choices = all_regions,
                    selected = all_regions[1])
      )
    ),
    column(3,
      # Species filter for CPUE/BPUE tabs (includes "All Species")
      conditionalPanel(
        condition = "input.main_tabs !== 'Fish Length'",
        selectInput("species_select", "Select Species:",
                    choices = c("All Species", all_species),
                    selected = "All Species")
      ),
      # Species filter for Length tab (NO "All Species" option)
      conditionalPanel(
        condition = "input.main_tabs === 'Fish Length'",
        selectInput("species_select_length", "Select Species:",
                    choices = all_species,
                    selected = all_species[1])
      )
    ),
    column(3,
      sliderInput("year_range", "Year Range:",
                  min = min(cpue_bpue_raw$Year),
                  max = max(cpue_bpue_raw$Year),
                  value = c(min(cpue_bpue_raw$Year), max(cpue_bpue_raw$Year)),
                  step = 1, sep = "")
    )
  ),

  hr(),

  # ---- Tabs for CPUE, BPUE, Length ----
  tabsetPanel(
    id = "main_tabs",

    # CPUE Tab
    tabPanel("CPUE",
      br(),
      h3("Catch Per Unit Effort (CPUE) Over Time"),
      p("Mean total CPUE per grid cell visit (ID_Cell_per_Trip), averaged by year and MPA status."),
      plotlyOutput("cpue_plot", height = "550px"),
      hr(),
      h4("CPUE Summary Table"),
      tableOutput("cpue_table")
    ),

    # BPUE Tab
    tabPanel("BPUE",
      br(),
      h3("Biomass Per Unit Effort (BPUE) Over Time"),
      p("Mean total BPUE per grid cell visit (ID_Cell_per_Trip), averaged by year and MPA status."),
      plotlyOutput("bpue_plot", height = "550px"),
      hr(),
      h4("BPUE Summary Table"),
      tableOutput("bpue_table")
    ),

    # Length Tab — now has TWO sub-tabs: Mean Length and Length Frequency
    tabPanel("Fish Length",
      br(),
      tabsetPanel(
        id = "length_subtabs",

        # Sub-tab 1: Mean Length Over Time (two-step aggregation)
        tabPanel("Mean Length Over Time",
          br(),
          h3("Mean Fish Length Over Time"),
          p("Mean fish length (cm) per grid cell visit, averaged by year and MPA status. Select a species to view."),
          plotlyOutput("length_plot", height = "550px"),
          hr(),
          h4("Mean Length Summary Table"),
          tableOutput("length_table")
        ),

        # Sub-tab 2: Length Frequency Distribution
        tabPanel("Length Frequency Distribution",
          br(),
          h3("Length Frequency Distribution"),
          p("Distribution of individual fish lengths (cm) by MPA status for the selected species and filters. ",
            "Histograms are shown side-by-side for MPA vs Reference sites."),
          fluidRow(
            column(4,
              selectInput("length_year_select", "Select Year (or All Years):",
                          choices = c("All Years", sort(unique(length_raw$Year))),
                          selected = "All Years")
            ),
            column(4,
              sliderInput("bin_width", "Bin Width (cm):",
                          min = 1, max = 10, value = 2, step = 1)
            )
          ),
          plotlyOutput("length_freq_plot", height = "550px"),
          hr(),
          h4("Length Frequency Summary"),
          tableOutput("length_freq_table")
        )
      )
    )
  ),

  hr(),
  tags$footer(
    p("CCFRP Data Visualization App | Built with R Shiny",
      style = "text-align: center; color: gray; font-size: 12px;")
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ===========================================================
  # CPUE / BPUE AGGREGATION
  # ===========================================================

  # ---- Reactive: filtered CPUE/BPUE data ----
  filtered_cpue_bpue <- reactive({
    data <- cpue_bpue_raw %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])

    if (input$species_select != "All Species") {
      data <- data %>% filter(Common_Name == input$species_select)
    }

    if (input$view_level == "area") {
      data <- data %>% filter(Area == input$area_select)
    } else if (input$view_level == "region") {
      data <- data %>% filter(Region == input$region_select)
    }

    return(data)
  })

  # ---- Reactive: summarized CPUE/BPUE data (two-step aggregation) ----
  summarized_cpue_bpue <- reactive({
    data <- filtered_cpue_bpue()

    # Step 1: Sum CPUE/BPUE across species within each cell-trip
    cell_trip_totals <- data %>%
      group_by(ID_Cell_per_Trip, Year, MPA_Status, Area, Region) %>%
      summarise(
        total_CPUE = sum(CPUE_catch_per_angler_hour, na.rm = TRUE),
        total_BPUE = sum(BPUE_biomass.kg._per_angler_hour, na.rm = TRUE),
        .groups = "drop"
      )

    # Step 2: Average the cell-trip totals by Year + MPA_Status
    cell_trip_totals %>%
      group_by(Year, MPA_Status) %>%
      summarise(
        mean_CPUE = mean(total_CPUE, na.rm = TRUE),
        se_CPUE   = sd(total_CPUE, na.rm = TRUE) / sqrt(n()),
        mean_BPUE = mean(total_BPUE, na.rm = TRUE),
        se_BPUE   = sd(total_BPUE, na.rm = TRUE) / sqrt(n()),
        n_cell_trips = n(),
        .groups = "drop"
      )
  })

  # ===========================================================
  # LENGTH AGGREGATION
  # ===========================================================

  # ---- Reactive: filtered length data (uses separate species selector) ----
  filtered_length <- reactive({
    data <- length_raw %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])

    # Length tab ALWAYS filters by a single species
    data <- data %>% filter(Common_Name == input$species_select_length)

    if (input$view_level == "area") {
      data <- data %>% filter(Area == input$area_select)
    } else if (input$view_level == "region") {
      data <- data %>% filter(Region == input$region_select)
    }

    return(data)
  })

  # ---- Reactive: summarized length data (two-step aggregation) ----
  summarized_length <- reactive({
    data <- filtered_length()

    # Step 1: Mean length per cell-trip
    cell_trip_means <- data %>%
      group_by(ID_Cell_per_Trip, Year, MPA_Status, Area, Region) %>%
      summarise(
        mean_length_cell_trip = mean(Length_cm, na.rm = TRUE),
        n_fish_cell_trip = n(),
        .groups = "drop"
      )

    # Step 2: Average cell-trip means by Year + MPA_Status
    cell_trip_means %>%
      group_by(Year, MPA_Status) %>%
      summarise(
        mean_length    = mean(mean_length_cell_trip, na.rm = TRUE),
        se_length      = sd(mean_length_cell_trip, na.rm = TRUE) / sqrt(n()),
        n_cell_trips   = n(),
        total_fish     = sum(n_fish_cell_trip),
        .groups = "drop"
      )
  })

  # ---- Reactive: filtered length data for frequency distribution ----
  # Same species/area/region filters as mean length, but with optional single-year filter
  filtered_length_freq <- reactive({
    data <- filtered_length()

    # Additional year filter for the frequency plot
    if (input$length_year_select != "All Years") {
      data <- data %>% filter(Year == as.integer(input$length_year_select))
    }

    return(data)
  })

  # ---- Plot: CPUE ----
  output$cpue_plot <- renderPlotly({
    data <- summarized_cpue_bpue()

    title_text <- switch(input$view_level,
      "area" = paste("CPUE at", input$area_select),
      "region" = paste("CPUE in", input$region_select),
      "all" = "CPUE Across All Areas"
    )

    p <- ggplot(data, aes(x = Year, y = mean_CPUE, color = MPA_Status)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_errorbar(aes(ymin = mean_CPUE - se_CPUE, ymax = mean_CPUE + se_CPUE),
                    width = 0.2) +
      scale_color_manual(values = c("MPA" = "#2196F3", "REF" = "#FF5722"),
                         labels = c("MPA" = "MPA", "REF" = "Reference")) +
      labs(title = title_text,
           x = "Year",
           y = "Mean CPUE (catch per angler hour)",
           color = "Site Status") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.15))
  })

  # ---- Plot: BPUE ----
  output$bpue_plot <- renderPlotly({
    data <- summarized_cpue_bpue()

    title_text <- switch(input$view_level,
      "area" = paste("BPUE at", input$area_select),
      "region" = paste("BPUE in", input$region_select),
      "all" = "BPUE Across All Areas"
    )

    p <- ggplot(data, aes(x = Year, y = mean_BPUE, color = MPA_Status)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_errorbar(aes(ymin = mean_BPUE - se_BPUE, ymax = mean_BPUE + se_BPUE),
                    width = 0.2) +
      scale_color_manual(values = c("MPA" = "#2196F3", "REF" = "#FF5722"),
                         labels = c("MPA" = "MPA", "REF" = "Reference")) +
      labs(title = title_text,
           x = "Year",
           y = "Mean BPUE (kg per angler hour)",
           color = "Site Status") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.15))
  })

  # ---- Plot: Mean Length Over Time ----
  output$length_plot <- renderPlotly({
    data <- summarized_length()

    species_name <- input$species_select_length;

    title_text <- switch(input$view_level,
      "area" = paste(species_name, "- Mean Length at", input$area_select),
      "region" = paste(species_name, "- Mean Length in", input$region_select),
      "all" = paste(species_name, "- Mean Length Across All Areas")
    )

    p <- ggplot(data, aes(x = Year, y = mean_length, color = MPA_Status)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_errorbar(aes(ymin = mean_length - se_length, ymax = mean_length + se_length),
                    width = 0.2) +
      scale_color_manual(values = c("MPA" = "#2196F3", "REF" = "#FF5722"),
                         labels = c("MPA" = "MPA", "REF" = "Reference")) +
      labs(title = title_text,
           x = "Year",
           y = "Mean Length (cm)",
           color = "Site Status") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.15))
  })

  # ---- Plot: Length Frequency Distribution ----
  output$length_freq_plot <- renderPlotly({
    data <- filtered_length_freq()

    species_name <- input$species_select_length
    year_label <- if (input$length_year_select == "All Years") {
      paste0(input$year_range[1], "-", input$year_range[2])
    } else {
      input$length_year_select
    }

    title_text <- switch(input$view_level,
      "area" = paste(species_name, "- Length Frequency at", input$area_select, "(", year_label, ")"),
      "region" = paste(species_name, "- Length Frequency in", input$region_select, "(", year_label, ")"),
      "all" = paste(species_name, "- Length Frequency Across All Areas", "(", year_label, ")")
    )

    # Relabel MPA_Status for display
    data <- data %>%
      mutate(Site_Status = ifelse(MPA_Status == "MPA", "MPA", "Reference"))

    p <- ggplot(data, aes(x = Length_cm, fill = Site_Status)) +
      geom_histogram(binwidth = input$bin_width, position = "dodge",
                     alpha = 0.7, color = "white") +
      scale_fill_manual(values = c("MPA" = "#2196F3", "Reference" = "#FF5722")) +
      labs(title = title_text,
           x = "Length (cm)",
           y = "Number of Fish",
           fill = "Site Status") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.15))
  })

  # ---- Tables ----
  output$cpue_table <- renderTable({
    summarized_cpue_bpue() %>%
      select(Year, MPA_Status, mean_CPUE, se_CPUE, n_cell_trips) %>%
      rename(
        "MPA Status" = MPA_Status,
        "Mean CPUE" = mean_CPUE,
        "SE" = se_CPUE,
        "N Cell-Trips" = n_cell_trips
      )
  })

  output$bpue_table <- renderTable({
    summarized_cpue_bpue() %>%
      select(Year, MPA_Status, mean_BPUE, se_BPUE, n_cell_trips) %>%
      rename(
        "MPA Status" = MPA_Status,
        "Mean BPUE" = mean_BPUE,
        "SE" = se_BPUE,
        "N Cell-Trips" = n_cell_trips
      )
  })

  output$length_table <- renderTable({
    summarized_length() %>%
      rename(
        "MPA Status" = MPA_Status,
        "Mean Length (cm)" = mean_length,
        "SE" = se_length,
        "N Cell-Trips" = n_cell_trips,
        "Total Fish" = total_fish
      )
  })

  # ---- Length Frequency Summary Table ----
  output$length_freq_table <- renderTable({
    data <- filtered_length_freq()

    data %>%
      group_by(MPA_Status) %>%
      summarise(
        `N Fish` = n(),
        `Mean Length (cm)` = round(mean(Length_cm, na.rm = TRUE), 1),
        `SD Length (cm)` = round(sd(Length_cm, na.rm = TRUE), 1),
        `Min Length (cm)` = round(min(Length_cm, na.rm = TRUE), 1),
        `Max Length (cm)` = round(max(Length_cm, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      rename("MPA Status" = MPA_Status)
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)