# =============================================================================
# SINGLE MONTH EVENT SUMMARY TABLE
# How many 1-month drought events detected per config per country
# annually on average
# =============================================================================



all_drought_events <- readRDS( "./Data/all_drought_events_allconfigs.rds")%>%
  filter(!(index %in% c("spi3", "spi6") & method == "cumulative"))

n_years <- n_distinct(year(all_drought_events$start_date))

single_month_summary <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  filter(!is.na(country)) %>%
  mutate(
    event_year     = year(start_date),
    is_single_month = duration_months == 1
  ) %>%
  
  # ── Stage 1 — pixel level ──────────────────────────────────────────────────
  group_by(
    config_name, index, method,
    start_thr, min_duration,
    country, event_year, x, y
  ) %>%
  summarise(
    pixel_total_events        = n(),
    pixel_single_month_events = sum(is_single_month, na.rm = TRUE),
    pixel_pct_single_month    = round(
      100 * sum(is_single_month) / n(), 1),
    .groups = "drop"
  ) %>%
  
  # ── Stage 2 — country level per year ──────────────────────────────────────
  group_by(
    config_name, index, method,
    start_thr, min_duration,
    country, event_year
  ) %>%
  summarise(
    mean_total_events        = round(mean(pixel_total_events,        na.rm = TRUE), 2),
    mean_single_month_events = round(mean(pixel_single_month_events, na.rm = TRUE), 2),
    mean_pct_single_month    = round(mean(pixel_pct_single_month,    na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  
  # ── Stage 3 — average across all years ────────────────────────────────────
  group_by(
    config_name, index, method,
    start_thr, min_duration,
    country
  ) %>%
  summarise(
    annual_total_events        = round(mean(mean_total_events,        na.rm = TRUE), 2),
    annual_single_month_events = round(mean(mean_single_month_events, na.rm = TRUE), 2),
    annual_pct_single_month    = round(mean(mean_pct_single_month,    na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(country, index, method, start_thr, min_duration)

cat("=== SINGLE MONTH EVENT SUMMARY ===\n\n")
print(single_month_summary, n = Inf, width = Inf)

# =============================================================================
# FORMATTED TABLE
# =============================================================================

library(knitr)
library(kableExtra)
library(tibble)

table_data <- single_month_summary %>%
  mutate(
    Index  = toupper(index),
    index_order = case_when(
      Index == "SPI1"  ~ 1, Index == "SPI3"  ~ 2, Index == "SPI6"  ~ 3,
      Index == "SPEI1" ~ 4, Index == "SPEI3" ~ 5, Index == "SPEI6" ~ 6,
      TRUE             ~ 99
    ),
    method_order = case_when(
      method == "threshold_entry" ~ 1,
      method == "threshold_peak"  ~ 2,
      method == "cumulative"      ~ 3,
      TRUE                        ~ 99
    ),
    Method    = case_when(
      method == "threshold_entry" ~ "Thr entry",
      method == "threshold_peak"  ~ "Thr peak",
      method == "cumulative"      ~ "Cumulative",
      TRUE                        ~ method
    ),
    Threshold = sprintf("%.1f", start_thr)
  ) %>%
  # ── Arrange BEFORE dropping order columns ──────────────────────────────────
  arrange(country, index_order, method_order, Threshold) %>%
  # ── Drop helper columns BEFORE renaming ────────────────────────────────────
  select(-index_order, -method_order) %>%
  select(
    Country                   = country,
    Index,
    Method,
    Threshold,
    `Annual events`           = annual_total_events,
    `Annual 1-month events`   = annual_single_month_events,
    `% single month`          = annual_pct_single_month
  ) %>%
  mutate(
    Country = ifelse(duplicated(paste(Country, Index)), "", Country),
    Index   = ifelse(duplicated(paste(
      cumsum(!duplicated(paste(single_month_summary$country,
                               single_month_summary$index))),
      Index)), "", Index)
  )

group_labels <- single_month_summary %>%
  mutate(
    Index = toupper(index),
    group = paste(country, toupper(index), sep = " — ")
  ) %>%
  count(group) %>%
  deframe()

html_table <- table_data %>%
  kable(
    format    = "html",
    caption   = "Annual single-month drought events by configuration",
    align     = c("l","l","l","r","r","r","r"),
    col.names = c("Country", "Index", "Method", "Threshold",
                  "Annual events/pixel",
                  "Annual 1-month events/pixel",
                  "% single month")
  ) %>%
  kable_styling(
    bootstrap_options = c("hover", "condensed"),
    full_width        = FALSE,
    font_size         = 12
  ) %>%
  pack_rows(
    index         = group_labels,
    label_row_css = "background-color: #f0f0f0; font-weight: bold;"
  ) %>%
  row_spec(0, bold = TRUE, background = "#f5f5f5") %>%
  # Highlight high % single month in red
 # column_spec(7,
  #            color = ifelse(
  #              single_month_summary$annual_pct_single_month > 50,
  #              "#A32D2D", "black"),
  #            bold = ifelse(
  #              single_month_summary$annual_pct_single_month > 50,
  #              TRUE, FALSE)
 # ) %>%
  column_spec(1, bold = TRUE, width = "8em") %>%
  column_spec(2, bold = TRUE, width = "5em") %>%
  column_spec(3, width = "7em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "8em") %>%
  column_spec(6, width = "10em") %>%
  column_spec(7, width = "8em")

# ── Save as HTML ──────────────────────────────────────────────────────────────
save_kable(
  html_table,
  file           = "./Figures/single_month_events_table.html",
  self_contained = TRUE
)

message("Saved: single_month_events_table.html")