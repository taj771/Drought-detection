# =============================================================================
# CLEAN SUMMARY TABLE — using drought_annual_summary Annual -->  summary -- what does a typical YEAR
# look like under this definition"
# =============================================================================

library(dplyr)
library(knitr)
library(kableExtra)
library(tibble)


drought_annual_summary <- read.csv("./Data/drought_annual_summary.csv")%>%
  filter(!(index %in% c("spi3", "spi6") & method == "cumulative"))



summary_table <- drought_annual_summary %>%
  select(
    Country              = country,
    Index                = index,
    Method               = method,
    Threshold            = start_thr,
    `Annual freq`        = annual_freq_events,
    `% pixels affected`  = annual_pct_pixels,
    `Mean duration (mo)` = annual_mean_duration,
    `Mean peak SPI`      = annual_mean_peak_spi,
    `Mean severity`      = annual_mean_severity
  ) %>%
  mutate(
    Index = toupper(Index),
    index_order = case_when(
      Index == "SPI1"  ~ 1, Index == "SPI3"  ~ 2, Index == "SPI6"  ~ 3,
      Index == "SPEI1" ~ 4, Index == "SPEI3" ~ 5, Index == "SPEI6" ~ 6,
      TRUE             ~ 99
    ),
    method_order = case_when(
      Method == "threshold_entry" ~ 1,
      Method == "threshold_peak"  ~ 2,
      Method == "cumulative"      ~ 3,
      TRUE                        ~ 99
    ),
    Method = case_when(
      Method == "threshold_entry" ~ "Thr entry",
      Method == "threshold_peak"  ~ "Thr peak",
      Method == "cumulative"      ~ "Cumulative",
      TRUE                        ~ Method
    ),
    Threshold = sprintf("%.1f", Threshold)
  ) %>%
  arrange(Country, index_order, method_order, Threshold) %>%
  select(-index_order, -method_order)

# ── Build grouping structure ───────────────────────────────────────────────────
group_labels <- summary_table %>%
  mutate(group = paste(Country, Index, sep = " — ")) %>%
  count(group) %>%
  deframe()

# ── Remove repeated Country and Index values ──────────────────────────────────
table_display <- summary_table %>%
  mutate(
    Country = ifelse(duplicated(paste(Country, Index)), "", Country),
    Index   = ifelse(duplicated(paste(
      cumsum(!duplicated(paste(summary_table$Country,
                               summary_table$Index))),
      Index)), "", Index)
  )

# ── Render table ──────────────────────────────────────────────────────────────
table_display %>%
  kable(
    format    = "html",
    caption   = "Annual drought characteristics by country, index and detection configuration",
    align     = c("l", "l", "l", "r", "r", "r", "r", "r", "r"),
    digits    = 2,
    col.names = c("Country", "Index", "Method", "Threshold",
                  "Annual freq", "% pixels affected",
                  "Mean duration (mo)", "Mean peak SPI",
                  "Mean severity")
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
  column_spec(1, bold = TRUE, width = "8em") %>%
  column_spec(2, bold = TRUE, width = "5em") %>%
  column_spec(3, width = "7em") %>%
  column_spec(4, width = "5em") %>%
  column_spec(5, width = "7em") %>%
  column_spec(6, width = "8em") %>%
  column_spec(7, width = "8em") %>%
  column_spec(8, width = "8em") %>%
  column_spec(9, width = "8em")





library(knitr)
library(kableExtra)

# Build your table
html_table <- summary_table %>%
  kable(
    format    = "html",
    caption   = "Drought characteristics by country and configuration",
    col.names = c("Country", "Index", "Method", "Threshold",
                  "Annual freq", "% pixels affected",
                  "Mean duration (mo)", "Mean peak SPI",
                  "Mean severity")
  ) %>%
  kable_styling(
    bootstrap_options = c("hover", "condensed"),
    full_width        = FALSE,
    font_size         = 12
  ) %>%
  pack_rows(
    index = group_labels
  )

# Save as standalone HTML
save_kable(html_table,
           file      = "./Figures/drought_table.html",
           self_contained = TRUE)   # ← includes all CSS inline
#   so it looks right when opened






library(knitr)
library(kableExtra)

# Save as self-contained HTML first
# then convert to PDF

table_display %>%
  kable(format = "html") %>%
  kable_styling() %>%
  save_kable(
    file = "./Figures/drought_workflow_table.pdf",
    zoom = 1.5   # ← increase for higher resolution
  )