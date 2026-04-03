library(plotly)
library(dplyr)
library(scales)
library(reactable)

# Short display labels for STEM occupation types (raw value → label)
STEM_LABELS <- c(
  "SCIENCE OCCUPATIONS"     = "Science",
  "TECHNOLOGY OCCUPATIONS"  = "Technology",
  "ENGINEERING OCCUPATIONS" = "Engineering",
  "MATHEMATICS OCCUPATIONS" = "Mathematics",
  "HEALTH OCCUPATIONS"      = "Health"
)

# Palette derived from brand colors — 5 distinct tones for STEM categories
STEM_COLORS <- c(
  "Science"     = "#1397ab",
  "Technology"  = "#7ec9ae",
  "Engineering" = "#282622",
  "Mathematics" = "#e8a020",
  "Health"      = "#d5332f"
)

# Helper: build a date column from file_year + file_month
make_date <- function(df) {
  df %>% mutate(date = as.Date(paste(file_year, file_month, "01", sep = "-")))
}

# --- Overview tab ---

# Total federal headcount over time — accepts pre-aggregated time series
plot_total_headcount <- function(total_headcount_ts) {
  plot_ly(total_headcount_ts, x = ~date, y = ~headcount, type = "scatter", mode = "lines",
          line = list(color = "#1397ab", width = 2),
          hovertemplate = "%{x|%b %Y}<br>%{y:,.0f} employees<extra></extra>") %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "Total Employees", tickformat = ",.0f", gridcolor = "#f3f4f6"),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin = list(t = 10)
    )
}

# --- By Agency tab ---

# Headcount over time — combined or split by agency
plot_agency_trend <- function(agency_data, selected_agencies, selected_subelement = NULL, split = FALSE) {
  df <- agency_data %>%
    filter(agency %in% selected_agencies)

  if (!is.null(selected_subelement) && selected_subelement != "All") {
    df <- df %>% filter(agency_subelement == selected_subelement)
  }

  if (split && length(selected_agencies) > 1) {
    df <- df %>%
      make_date() %>%
      summarise(headcount = sum(headcount), .by = c(date, agency)) %>%
      arrange(date)

    p <- plot_ly(df, x = ~date, y = ~headcount, color = ~agency,
                 type = "scatter", mode = "lines",
                 line = list(width = 2),
                 hovertemplate = "%{x|%b %Y}<br>%{y:,.0f} employees<extra>%{fullData.name}</extra>") %>%
      layout(legend = list(orientation = "h", y = -0.15), showlegend = TRUE)
  } else {
    df <- df %>%
      make_date() %>%
      summarise(headcount = sum(headcount), .by = date) %>%
      arrange(date)

    p <- plot_ly(df, x = ~date, y = ~headcount, type = "scatter", mode = "lines",
                 line = list(color = "#1397ab", width = 2),
                 hovertemplate = "%{x|%b %Y}<br>%{y:,.0f} employees<extra></extra>") %>%
      layout(showlegend = FALSE)
  }

  p %>% layout(
    xaxis = list(title = "", showgrid = FALSE),
    yaxis = list(title = "Employees", tickformat = ",.0f", gridcolor = "#f3f4f6"),
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    margin = list(t = 10)
  )
}

# Top subelements by headcount for a selected agency (most recent month)
plot_top_subelements <- function(agency_data, selected_agencies, n = 15) {
  df <- agency_data %>%
    filter(agency %in% selected_agencies) %>%
    slice_max(order_by = file_year * 100 + file_month, n = 1, with_ties = TRUE) %>%
    summarise(headcount = sum(headcount), .by = agency_subelement) %>%
    slice_max(order_by = headcount, n = n) %>%
    arrange(desc(headcount))

  max_val <- max(df$headcount)

  reactable(
    df,
    columns = list(
      agency_subelement = colDef(name = "Sub-agency", minWidth = 160),
      headcount = colDef(
        name = "Employees",
        align = "left",
        minWidth = 120,
        cell = function(value) {
          bar_width <- paste0(round(value / max_val * 100), "%")
          htmltools::div(
            style = "display:flex; align-items:center; gap:6px;",
            htmltools::div(
              style = "flex:1; min-width:0;",
              htmltools::div(style = paste0("background:#1397ab; width:", bar_width,
                                            "; height:8px; border-radius:2px;"))
            ),
            htmltools::span(comma(value), style = "font-size:0.8rem; color:#282622; white-space:nowrap; width:55px; text-align:right;")
          )
        }
      )
    ),
    striped = TRUE, highlight = TRUE, compact = TRUE,
    pagination = FALSE,
    theme = reactableTheme(
      stripedColor     = "#f9fafb",
      highlightColor   = "#f0faf8",
      borderColor      = "#e5e7eb",
      headerStyle      = list(fontSize = "0.75rem", textTransform = "uppercase",
                              letterSpacing = "0.05em", color = "#6b7280")
    )
  )
}

# --- Scientists & STEM tab ---

# Stacked area: STEM type breakdown over time (optionally filtered to an agency)
plot_stem_trend <- function(stem_data, selected_agency = "All") {
  df <- stem_data

  if (!is.null(selected_agency) && selected_agency != "All") {
    df <- df %>%filter(agency == selected_agency)
  }

  df <- df |>
    make_date() |>
    summarise(headcount = sum(headcount), .by = c(date, stem_occupation_type)) |>
    mutate(stem_label = STEM_LABELS[stem_occupation_type]) |>
    arrange(date)

  plot_ly(df, x = ~date, y = ~headcount, color = ~stem_label,
          colors = STEM_COLORS,
          type = "scatter", mode = "lines", stackgroup = "one",
          hovertemplate = "%{x|%b %Y}<br>%{y:,.0f}<extra>%{fullData.name}</extra>") %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "STEM Employees", tickformat = ",.0f", gridcolor = "#f3f4f6"),
      legend = list(orientation = "h", y = -0.15),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin = list(t = 10)
    )
}

# Top agencies by STEM headcount (most recent month)
plot_top_stem_agencies <- function(stem_data, stem_types = NULL, n = 15) {
  df <- stem_data

  if (!is.null(stem_types) && length(stem_types) > 0) {
    df <- df %>% filter(stem_occupation_type %in% stem_types)
  }

  df <- df %>%
    slice_max(order_by = file_year * 100 + file_month, n = 1, with_ties = TRUE) %>%
    summarise(headcount = sum(headcount), .by = agency) %>%
    slice_max(order_by = headcount, n = n) %>%
    arrange(desc(headcount))

  max_val <- max(df$headcount)

  reactable(
    df,
    columns = list(
      agency = colDef(name = "Agency", minWidth = 160),
      headcount = colDef(
        name = "STEM Employees",
        align = "left",
        minWidth = 120,
        cell = function(value) {
          bar_width <- paste0(round(value / max_val * 100), "%")
          htmltools::div(
            style = "display:flex; align-items:center; gap:6px;",
            htmltools::div(
              style = "flex:1; min-width:0;",
              htmltools::div(style = paste0("background:#1397ab; width:", bar_width,
                                            "; height:8px; border-radius:2px;"))
            ),
            htmltools::span(comma(value), style = "font-size:0.8rem; color:#282622; white-space:nowrap; width:55px; text-align:right;")
          )
        }
      )
    ),
    striped = TRUE, highlight = TRUE, compact = TRUE,
    pagination = FALSE,
    theme = reactableTheme(
      stripedColor     = "#f9fafb",
      highlightColor   = "#f0faf8",
      borderColor      = "#e5e7eb",
      headerStyle      = list(fontSize = "0.75rem", textTransform = "uppercase",
                              letterSpacing = "0.05em", color = "#6b7280")
    )
  )
}
