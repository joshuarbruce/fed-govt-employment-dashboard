library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(reactable)

source("R/bq_queries.R")
source("R/plots.R")
source("R/maps.R")

# --- Data loading (once at startup) ---
con         <- bq_connect()
agency_data <- get_agency_monthly(con)
stem_data   <- get_stem_monthly(con)
geo_data    <- get_geo_monthly(con)
dbDisconnect(con)

# Exclude non-STEM catch-all values present in the source data
STEM_TYPES <- c("SCIENCE OCCUPATIONS", "TECHNOLOGY OCCUPATIONS",
                "ENGINEERING OCCUPATIONS", "MATHEMATICS OCCUPATIONS",
                "HEALTH OCCUPATIONS")
stem_data <- stem_data %>% filter(stem_occupation_type %in% STEM_TYPES)

# --- Derived values used across tabs ---
most_recent_period <- agency_data %>%
  slice_max(order_by = file_year * 100 + file_month, n = 1) %>%
  slice(1)

current_headcount <- agency_data %>%
  filter(file_year == most_recent_period$file_year,
         file_month == most_recent_period$file_month) %>%
  summarise(n = sum(headcount)) %>%
  pull(n)

prior_headcount <- agency_data %>%
  filter(file_year == most_recent_period$file_year - 1,
         file_month == most_recent_period$file_month) %>%
  summarise(n = sum(headcount)) %>%
  pull(n)

yoy_change_pct <- if (length(prior_headcount) == 0 || prior_headcount == 0) {
  NA
} else {
  round((current_headcount - prior_headcount) / prior_headcount * 100, 1)
}

current_stem <- stem_data %>%
  filter(file_year == most_recent_period$file_year,
         file_month == most_recent_period$file_month) %>%
  summarise(n = sum(headcount)) %>%
  pull(n)

stem_share_pct <- round(current_stem / current_headcount * 100, 1)

prior_stem <- stem_data %>%
  filter(file_year == most_recent_period$file_year - 1,
         file_month == most_recent_period$file_month) %>%
  summarise(n = sum(headcount)) %>%
  pull(n)

stem_yoy_change_pct <- if (length(prior_stem) == 0 || prior_stem == 0) {
  NA
} else {
  round((current_stem - prior_stem) / prior_stem * 100, 1)
}

agency_choices <- sort(unique(agency_data$agency))
stem_type_choices <- sort(unique(stem_data$stem_occupation_type))

# Pre-aggregated slices used by plot functions — collapse unused dimensions now
# so render functions only filter, never re-aggregate the full 2M-row table.
agency_by_date <- agency_data %>%
  summarise(headcount = sum(headcount),
            .by = c(file_year, file_month, agency, agency_subelement))

total_headcount_ts <- agency_data %>%
  mutate(date = as.Date(paste(file_year, file_month, "01", sep = "-"))) %>%
  summarise(headcount = sum(headcount), .by = date) %>%
  arrange(date)

stem_by_date <- stem_data %>%
  summarise(headcount = sum(headcount),
            .by = c(file_year, file_month, agency, agency_subelement, stem_occupation_type))

# --- UI ---
ui <- page_navbar(
  title = "Federal Government Employment",
  theme = bs_theme(
    base_font   = font_google("Inter"),
    primary     = "#1397ab",
    success     = "#7ec9ae",
    danger      = "#d5332f",
    secondary   = "#6b7280",
    "body-bg"   = "#fefefe",
    "body-color" = "#282622",
    "navbar-bg" = "#282622",
    "navbar-fg" = "#fefefe"
  ),
  includeCSS("www/custom.css"),

  # ── Overview ──────────────────────────────────────────────────────────────
  nav_panel("Overview",
    br(),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Current Federal Employees",
        value = scales::comma(current_headcount),
        showcase = bsicons::bs_icon("people-fill"),
        theme  = "primary"
      ),
      value_box(
        title = "Year-over-Year Change",
        value = if (is.na(yoy_change_pct)) "N/A" else paste0(ifelse(yoy_change_pct >= 0, "+", ""), yoy_change_pct, "%"),
        showcase = bsicons::bs_icon(if (is.na(yoy_change_pct) || yoy_change_pct >= 0) "arrow-up" else "arrow-down"),
        theme  = if (is.na(yoy_change_pct)) "secondary" else ifelse(yoy_change_pct >= 0, "success", "danger")
      ),
      value_box(
        title = "STEM Share of Workforce",
        value = paste0(stem_share_pct, "%"),
        showcase = bsicons::bs_icon("graph-up"),
        theme  = "info"
      ),
      value_box(
        title = "STEM Headcount YoY Change",
        value = if (is.na(stem_yoy_change_pct)) "N/A" else paste0(ifelse(stem_yoy_change_pct >= 0, "+", ""), stem_yoy_change_pct, "%"),
        showcase = bsicons::bs_icon(if (is.na(stem_yoy_change_pct) || stem_yoy_change_pct >= 0) "arrow-up" else "arrow-down"),
        theme  = if (is.na(stem_yoy_change_pct)) "secondary" else ifelse(stem_yoy_change_pct >= 0, "success", "danger")
      )
    ),
    br(),
    card(
      card_header("Total Federal Headcount Over Time"),
      plotlyOutput("plot_total_headcount", height = "400px")
    )
  ),

  # ── By Agency ─────────────────────────────────────────────────────────────
  nav_panel("By Agency",
    br(),
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput("agency_select", "Agency",
                       choices  = agency_choices,
                       selected = "DEPARTMENT OF HEALTH AND HUMAN SERVICES",
                       multiple = TRUE,
                       options  = list(placeholder = "Select one or more agencies...")),
        input_switch("agency_split", "Show each agency separately", value = FALSE),
        uiOutput("subelement_ui")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Headcount Over Time"),
          plotlyOutput("plot_agency_trend", height = "400px")
        ),
        card(
          card_header(textOutput("top_subelements_header")),
          reactableOutput("plot_top_subelements")
        )
      )
    )
  ),

  # ── Scientists & STEM ─────────────────────────────────────────────────────
  nav_panel("Scientists & STEM",
    br(),
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("stem_types", "STEM Category",
                           choiceNames  = unname(STEM_LABELS[stem_type_choices]),
                           choiceValues = stem_type_choices,
                           selected     = stem_type_choices),
        hr(),
        selectInput("stem_agency", "Filter to Agency",
                    choices  = c("All", agency_choices),
                    selected = "All")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("STEM Workforce Over Time"),
          plotlyOutput("plot_stem_trend", height = "400px")
        ),
        card(
          card_header("Top Agencies by STEM Headcount (Most Recent Month)"),
          reactableOutput("plot_top_stem_agencies")
        )
      )
    )
  ),

  # ── Geography ─────────────────────────────────────────────────────────────
  nav_panel("Geography",
    br(),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("geo_year", "Year",
                    min = 2005, max = max(geo_data$file_year), value = max(geo_data$file_year), step = 1, sep = ""),
        hr(),
        radioButtons("geo_metric", "Metric",
                     choices  = c("Headcount" = "headcount", "YoY Change (count)" = "yoy_count", "YoY Change (%)" = "yoy"),
                     selected = "headcount"),
        hr(),
        checkboxInput("geo_stem_only", "STEM occupations only", value = FALSE),
        selectInput("geo_agency", "Filter to Agency",
                    choices  = c("All", agency_choices),
                    selected = "All")
      ),
      card(
        card_header("Federal Employment by State"),
        leafletOutput("geo_map", height = "480px")
      ),
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  # ── Overview ──────────────────────────────────────────────────────────────
  output$plot_total_headcount <- renderPlotly({
    plot_total_headcount(total_headcount_ts)
  })

  # ── By Agency ─────────────────────────────────────────────────────────────
  subelements_for_agency <- reactive({
    c("All", sort(unique(agency_by_date$agency_subelement[agency_by_date$agency %in% input$agency_select])))
  })

  output$subelement_ui <- renderUI({
    selectInput("subelement_select", "Sub-agency",
                choices  = subelements_for_agency(),
                selected = "All")
  })

  output$plot_agency_trend <- renderPlotly({
    req(input$agency_select)
    plot_agency_trend(agency_by_date, input$agency_select, input$subelement_select, input$agency_split)
  }) %>% bindCache(input$agency_select, input$subelement_select, input$agency_split)

  output$top_subelements_header <- renderText({
    if (length(input$agency_select) == 1) {
      paste("Top Sub-agencies —", input$agency_select, "(Most Recent Month)")
    } else {
      "Top Sub-agencies — Selected Agencies (Most Recent Month)"
    }
  })

  output$plot_top_subelements <- renderReactable({
    req(input$agency_select)
    plot_top_subelements(agency_by_date, input$agency_select)
  }) %>% bindCache(input$agency_select)

  # ── Scientists & STEM ─────────────────────────────────────────────────────
  output$plot_stem_trend <- renderPlotly({
    req(input$stem_types)
    stem_filtered <- stem_by_date %>% filter(stem_occupation_type %in% input$stem_types)
    plot_stem_trend(stem_filtered, input$stem_agency)
  }) %>% bindCache(input$stem_types, input$stem_agency)

  output$plot_top_stem_agencies <- renderReactable({
    req(input$stem_types)
    plot_top_stem_agencies(stem_by_date, input$stem_types)
  }) %>% bindCache(input$stem_types)

  # ── Geography ─────────────────────────────────────────────────────────────
  output$geo_map <- renderLeaflet({
    plot_geo_map(geo_data, input$geo_year,
                 stem_only       = input$geo_stem_only,
                 selected_agency = input$geo_agency,
                 metric          = input$geo_metric)
  }) %>% bindCache(input$geo_year, input$geo_stem_only, input$geo_agency, input$geo_metric)

}

shinyApp(ui, server)
