library(leaflet)
library(dplyr)
library(sf)
library(scales)
library(tigris)

# Cache tigris downloads so subsequent app starts don't re-download
options(tigris_use_cache = TRUE)

# Build state boundary sf object once at source time (all 50 states + DC)
.build_states_sf <- function() {
  sf::sf_use_s2(FALSE)
  tigris::states(cb = TRUE, resolution = "20m", year = 2023) %>%
    filter(!STUSPS %in% c("PR", "GU", "VI", "MP", "AS")) %>%
    select(abb = STUSPS, geometry)
}

STATES_SF <- .build_states_sf()

# --- Geography tab ---

# Choropleth map of federal employment by state.
# metric: "headcount" = absolute count, "yoy" = YoY % change
plot_geo_map <- function(geo_data, selected_year,
                         stem_only = FALSE, selected_agency = "All",
                         metric = "headcount") {
  df <- geo_data

  if (stem_only) {
    df <- df %>% filter(!is.na(stem_occupation_type), stem_occupation_type != "")
  }
  if (selected_agency != "All") {
    df <- df %>% filter(agency == selected_agency)
  }

  # Use the most recent available month for the selected year
  latest_month <- df %>%
    filter(file_year == selected_year) %>%
    summarise(m = max(file_month, na.rm = TRUE)) %>%
    pull(m)

  if (length(latest_month) == 0 || is.na(latest_month)) {
    return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron))
  }

  current <- df %>%
    filter(file_year == selected_year, file_month == latest_month) %>%
    summarise(headcount = sum(headcount), .by = duty_station_state_abbreviation)

  if (metric %in% c("yoy", "yoy_count")) {
    prior <- df %>%
      filter(file_year == selected_year - 1, file_month == latest_month) %>%
      summarise(headcount = sum(headcount), .by = duty_station_state_abbreviation)

    current <- current %>%
      left_join(prior, by = "duty_station_state_abbreviation", suffix = c("", "_prior"))

    if (metric == "yoy") {
      current <- current %>%
        mutate(value = round((headcount - headcount_prior) / headcount_prior * 100, 1)) %>%
        select(duty_station_state_abbreviation, value)
      legend_title <- "YoY Change (%)"
      label_fmt    <- function(abb, v) paste0(abb, ": ", ifelse(is.na(v), "N/A", paste0(v, "%")))
    } else {
      current <- current %>%
        mutate(value = headcount - headcount_prior) %>%
        select(duty_station_state_abbreviation, value)
      legend_title <- "YoY Change (n)"
      label_fmt    <- function(abb, v) paste0(abb, ": ", ifelse(is.na(v), "N/A", paste0(ifelse(v >= 0, "+", ""), comma(v))))
    }

    max_abs <- max(abs(current$value), na.rm = TRUE)
    pal     <- colorNumeric(c("#d5332f", "#f5f5f5", "#1397ab"),
                            domain = c(-max_abs, max_abs), na.color = "#e5e7eb")
  } else {
    current <- current %>% rename(value = headcount)

    pal         <- colorNumeric(c("#f0faf8", "#7ec9ae", "#1397ab"),
                                domain = current$value, na.color = "#e5e7eb")
    legend_title <- "Employees"
    label_fmt    <- function(abb, v) paste0(abb, ": ", ifelse(is.na(v), "N/A", comma(v)))
  }

  map_data <- STATES_SF %>%
    left_join(current, by = c("abb" = "duty_station_state_abbreviation"))

  leaflet(map_data) %>%
    setView(lng = -96, lat = 38, zoom = 4) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor   = ~pal(value),
      fillOpacity = 0.8,
      color       = "#ffffff",
      weight      = 1,
      highlightOptions = highlightOptions(weight = 2, color = "#282622", bringToFront = TRUE),
      label = ~mapply(label_fmt, abb, value, SIMPLIFY = FALSE)
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = legend_title, opacity = 0.9)
}

# Top N states bar chart — reuses the same filters as the map
top_states_data <- function(geo_data, selected_year,
                            stem_only = FALSE, selected_agency = "All", n = 10) {
  df <- geo_data

  if (stem_only) {
    df <- df %>% filter(!is.na(stem_occupation_type), stem_occupation_type != "")
  }
  if (selected_agency != "All") {
    df <- df %>% filter(agency == selected_agency)
  }

  latest_month <- df %>%
    filter(file_year == selected_year) %>%
    summarise(m = max(file_month, na.rm = TRUE)) %>%
    pull(m)

  df %>%
    filter(file_year == selected_year, file_month == latest_month) %>%
    summarise(headcount = sum(headcount), .by = duty_station_state_abbreviation) %>%
    slice_max(order_by = headcount, n = n) %>%
    arrange(headcount)
}
