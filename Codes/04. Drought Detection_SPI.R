###############################################################################
# DROUGHT DETECTION — BASED ON SPI - FULL PIPELINE
# Indices: SPI1, SPI3, SPI6, SPI12
# Methods: threshold | cumulative
###############################################################################

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(SPEI)


# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: Load data
# ─────────────────────────────────────────────────────────────────────────────

spi_results <- readRDS("./Data/spi_results.rds")


# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: Build country average across all pixels
# ─────────────────────────────────────────────────────────────────────────────

country_avg <- spi_results %>%
  group_by(year, month) %>%
  summarise(
    spi1  = mean(spi1,  na.rm = TRUE),
    spi3  = mean(spi3,  na.rm = TRUE),
    spi6  = mean(spi6,  na.rm = TRUE),
    spi12 = mean(spi12, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, month) %>%
  mutate(
    month = as.integer(month),
    year  = as.integer(year),
    date  = as.Date(paste(year, month, "01", sep = "-"))
  )%>%
  filter(between(year, 2000, 2025)) # filter year to mathc the required time span 




# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: Configure drought detection — ONLY change this block
#
# method:
#   "threshold"  → drought ends when SPI rises back above end_threshold
#   "cumulative" → drought ends when cumulative SPI sum is repaid to >= 0
#                  (allows drought to continue through above-normal months)
#
# start_threshold:
#   0    = any negative SPI triggers (most sensitive)
#  -0.5  = mild dryness onset
#  -1.0  = WMO moderate drought
#
# end_threshold:
#   used only for method = "threshold"
#   e.g. -0.5 means SPI must recover to >= -0.5 to end the drought
#
# min_duration:
#   minimum number of CONSECUTIVE months below start_threshold
#   required before a drought is declared
#   e.g. 2 = need two back-to-back negative months to confirm drought start
# ─────────────────────────────────────────────────────────────────────────────

drought_config <- list(
  method          = "threshold",   # "threshold" or "cumulative"
  start_threshold = -1,              # drought starts on any negative SPI
  end_threshold   = -0.9999999999999999,              # only used for threshold method
  min_duration    = 2               # need 2 consecutive negative months to start
)


# ─────────────────────────────────────────────────────────────────────────────
# STEP 5: Drought detection function
#
# Two-question logic:
#   Q1: Are there min_duration consecutive months all below start_threshold?
#       If NO  → not a drought start, move on
#       If YES → confirmed drought start, proceed to Q2
#   Q2: When does the drought end?
#       cumulative → when cumulative SPI sum repays back to >= 0
#       threshold  → when SPI rises back above end_threshold
# ─────────────────────────────────────────────────────────────────────────────

detect_droughts <- function(spi_vec, dates, config) {
  
  if (is.null(dates))                   stop("'dates' is NULL — check column name")
  if (length(dates) != length(spi_vec)) stop("'dates' and 'spi_vec' must be same length")
  dates     <- as.Date(dates)
  method    <- config$method
  start_thr <- config$start_threshold
  end_thr   <- config$end_threshold
  min_dur   <- config$min_duration
  n         <- length(spi_vec)
  
  event_id   <- rep(NA_integer_, n)
  cum_track  <- rep(NA_real_,    n)
  drought_id <- 0
  i          <- 1
  
  while (i <= n) {
    
    val <- spi_vec[i]
    if (is.na(val) || is.nan(val)) { i <- i + 1; next }
    
    # ── Q1: min_dur consecutive months below start_thr ───────────────────────
    window       <- spi_vec[i:min(i + min_dur - 1, n)]
    window       <- window[!is.na(window) & !is.nan(window)]
    all_negative <- (length(window) == min_dur) && all(window < start_thr)
    
    if (!all_negative) { i <- i + 1; next }
    
    # ── Q1 PASSED ─────────────────────────────────────────────────────────────
    drought_id  <- drought_id + 1
    cum_deficit <- 0
    
    for (k in i:n) {
      val_k <- spi_vec[k]
      if (is.na(val_k) || is.nan(val_k)) next
      
      if (method == "threshold") {
        # ── CHECK END CONDITION FIRST before tagging this month ───────────────
        # If this month is above end_threshold it is the recovery month
        # It should NOT be part of the drought — exit without tagging it
        if (val_k >= end_thr) {
          i <- k + 1   # next search starts from this recovery month
          break
        }
        # Still below threshold — tag this month as drought
        cum_deficit  <- cum_deficit + val_k
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit
        
      } else if (method == "cumulative") {
        # Cumulative: always tag first, check end after
        # (positive months are part of the drought in cumulative method)
        cum_deficit  <- cum_deficit + val_k
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit
        
        if (cum_deficit >= 0) {
          i <- k + 1
          break
        }
      }
      
      if (k == n) {
        message("Note: drought event #", drought_id, " is ongoing at end of record")
        i <- n + 1
      }
    }
  }
  
  if (drought_id == 0) { message("No drought events detected."); return(tibble()) }
  
  monthly_track <- tibble(
    event_date  = dates,
    spi         = spi_vec,
    event_id    = event_id,
    cum_deficit = cum_track
  ) %>% filter(!is.na(event_id))
  
  events <- monthly_track %>%
    group_by(drought_id = event_id) %>%
    summarise(
      start_date       = min(event_date),
      end_date         = max(event_date),
      duration_months  = n(),
      peak_spi         = min(spi,          na.rm = TRUE),
      peak_date        = event_date[which.min(spi)],
      sum_positive_spi = sum(spi[spi > 0], na.rm = TRUE),
      total_severity   = sum(spi,          na.rm = TRUE),
      min_cum_deficit  = min(cum_deficit,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      severity_class = case_when(
        peak_spi >= -1.0                    ~ "Mild",
        peak_spi >= -1.5 & peak_spi < -1.0 ~ "Moderate",
        peak_spi >= -2.0 & peak_spi < -1.5 ~ "Severe",
        peak_spi <  -2.0                    ~ "Extreme"
      )
    )
  
  return(events)
}

# ─────────────────────────────────────────────────────────────────────────────
# STEP 6: Tracking function — identical logic to detect_droughts
# Adds per-month drought ID and cumulative deficit back onto country_avg
# Used for plotting the deficit through time
# ─────────────────────────────────────────────────────────────────────────────

add_cumulative_track <- function(spi_vec, dates, config) {
  
  dates     <- as.Date(dates)
  method    <- config$method
  start_thr <- config$start_threshold
  end_thr   <- config$end_threshold
  min_dur   <- config$min_duration
  n         <- length(spi_vec)
  
  event_id   <- rep(NA_integer_, n)
  cum_track  <- rep(0,           n)
  drought_id <- 0
  i          <- 1
  
  while (i <= n) {
    
    val <- spi_vec[i]
    if (is.na(val) || is.nan(val)) { i <- i + 1; next }
    
    window       <- spi_vec[i:min(i + min_dur - 1, n)]
    window       <- window[!is.na(window) & !is.nan(window)]
    all_negative <- (length(window) == min_dur) && all(window < start_thr)
    
    if (!all_negative) { i <- i + 1; next }
    
    drought_id  <- drought_id + 1
    cum_deficit <- 0
    
    for (k in i:n) {
      val_k <- spi_vec[k]
      if (is.na(val_k) || is.nan(val_k)) next
      
      if (method == "threshold") {
        # ── Check end BEFORE tagging ──────────────────────────────────────────
        if (val_k >= end_thr) { i <- k + 1; break }
        cum_deficit  <- cum_deficit + val_k
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit
        
      } else if (method == "cumulative") {
        cum_deficit  <- cum_deficit + val_k
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit
        if (cum_deficit >= 0) { i <- k + 1; break }
      }
      
      if (k == n) i <- n + 1
    }
  }
  
  return(list(event_id = event_id, cum_deficit = cum_track))
}


# ─────────────────────────────────────────────────────────────────────────────
# STEP 7: Run detection
# ─────────────────────────────────────────────────────────────────────────────

message("Running drought detection | method: ", drought_config$method,
        " | start: ",        drought_config$start_threshold,
        " | end: ",          drought_config$end_threshold,
        " | min duration: ", drought_config$min_duration, " months")

drought_spi1  <- detect_droughts(country_avg$spi1,  country_avg$date, drought_config)
drought_spi3  <- detect_droughts(country_avg$spi3,  country_avg$date, drought_config)
drought_spi6  <- detect_droughts(country_avg$spi6,  country_avg$date, drought_config)
drought_spi12 <- detect_droughts(country_avg$spi12, country_avg$date, drought_config)

message("Done! Events detected — SPI1: ",  nrow(drought_spi1),
        " | SPI3: ",  nrow(drought_spi3),
        " | SPI6: ",  nrow(drought_spi6),
        " | SPI12: ", nrow(drought_spi12))


# ─────────────────────────────────────────────────────────────────────────────
# STEP 8: Attach tracking columns to country_avg
# ─────────────────────────────────────────────────────────────────────────────

track1  <- add_cumulative_track(country_avg$spi1,  country_avg$date, drought_config)
track3  <- add_cumulative_track(country_avg$spi3,  country_avg$date, drought_config)
track6  <- add_cumulative_track(country_avg$spi6,  country_avg$date, drought_config)
track12 <- add_cumulative_track(country_avg$spi12, country_avg$date, drought_config)

country_avg <- country_avg %>%
  mutate(
    drought_id_spi1   = track1$event_id,   cum_deficit_spi1  = track1$cum_deficit,
    drought_id_spi3   = track3$event_id,   cum_deficit_spi3  = track3$cum_deficit,
    drought_id_spi6   = track6$event_id,   cum_deficit_spi6  = track6$cum_deficit,
    drought_id_spi12  = track12$event_id,  cum_deficit_spi12 = track12$cum_deficit
  )


# ─────────────────────────────────────────────────────────────────────────────
# STEP 9: Print summaries
# ─────────────────────────────────────────────────────────────────────────────

for (scale in c("spi1", "spi3", "spi6", "spi12")) {
  d <- get(paste0("drought_", scale))
  cat("\n──", toupper(scale), "Drought Summary ──\n")
  cat("Total events     :", nrow(d), "\n")
  cat("Avg duration     :", round(mean(d$duration_months), 1), "months\n")
  cat("Longest event    :", max(d$duration_months), "months  |",
      format(d$start_date[which.max(d$duration_months)], "%b %Y"), "→",
      format(d$end_date[which.max(d$duration_months)],   "%b %Y"), "\n")
  cat("Most severe event:", round(min(d$total_severity), 2), "(total SPI deficit)  |",
      format(d$start_date[which.min(d$total_severity)], "%b %Y"), "\n")
}


# ─────────────────────────────────────────────────────────────────────────────
# STEP 10: Plot drought events
# ─────────────────────────────────────────────────────────────────────────────

# Auto-generate subtitle from config so plots are always self-documenting
drought_method_label <- if (drought_config$method == "threshold") {
  paste0("Simple threshold | Start: SPI < ",  drought_config$start_threshold,
         "  |  End: SPI ≥ ",                  drought_config$end_threshold,
         "  |  Min consecutive months: ",     drought_config$min_duration)
} else {
  paste0("Cumulative run theory | Start: SPI < ", drought_config$start_threshold,
         "  |  End: cumulative SPI ≥ 0",
         "  |  Min consecutive months: ",         drought_config$min_duration)
}

plot_spi_droughts <- function(country_avg, drought_events, spi_col, title_label, config) {
  
  plot_data <- country_avg %>%
    select(date, spi = all_of(spi_col)) %>%
    filter(!is.na(spi))
  
  drought_bands <- if (nrow(drought_events) > 0) {
    drought_events %>% mutate(xmin = start_date, xmax = end_date + days(15))
  } else { NULL }
  
  severity_colors <- c(
    "Mild"     = "#FFF9C4",
    "Moderate" = "#FFCC80",
    "Severe"   = "#EF9A9A",
    "Extreme"  = "#CE93D8"
  )
  
  ggplot(plot_data, aes(x = date, y = spi)) +
    
    # Zero reference line
    geom_hline(yintercept = 0, color = "#888888", linewidth = 0.4, linetype = "solid") +
    
    # Drought threshold line — matches start_threshold from config
    geom_hline(yintercept = config$start_threshold,
               color = "#FF5722", linewidth = 0.4, linetype = "dashed") +
    
    # Drought period shading coloured by severity
    {if (!is.null(drought_bands))
      geom_rect(data = drought_bands,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                    fill = severity_class),
                inherit.aes = FALSE, alpha = 0.25)
    } +
    scale_fill_manual(values = severity_colors, name = "Drought severity") +
    
    # Full SPI line in blue
    geom_line(color = "#5C9BD6", linewidth = 0.55, alpha = 0.9) +
    
    # Red overlay where SPI is below start_threshold
    geom_line(
      data = plot_data %>%
        mutate(spi_drought = ifelse(spi < config$start_threshold, spi, NA)),
      aes(y = spi_drought), color = "#E53935", linewidth = 0.7
    ) +
    
    # Red vertical line = drought START
    {if (!is.null(drought_bands))
      geom_vline(data = drought_bands, aes(xintercept = as.numeric(xmin)),
                 color = "#E53935", linewidth = 0.7, linetype = "solid",
                 inherit.aes = FALSE)
    } +
    
    # Green vertical line = drought END
    {if (!is.null(drought_bands))
      geom_vline(data = drought_bands, aes(xintercept = as.numeric(end_date)),
                 color = "#43A047", linewidth = 0.7, linetype = "solid",
                 inherit.aes = FALSE)
    } +
    
    # Event ID label centred on each drought band
    {if (!is.null(drought_bands))
      geom_label(data = drought_bands,
                 aes(x     = xmin + (end_date - xmin) / 2,
                     y     = Inf,
                     label = paste0("D", drought_id)),
                 inherit.aes = FALSE, vjust = 1.3, size = 2.8,
                 color = "#333333", fill = "white",
                 label.size = 0.2, fontface = "bold")
    } +
    
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 expand = c(0.01, 0)) +
    scale_y_continuous(breaks = seq(-3, 3, by = 1)) +
    
    labs(
      title    = paste(title_label, "— Drought Events |",
                       nrow(drought_events), "detected"),
      subtitle = drought_method_label,
      x        = NULL,
      y        = paste(title_label, "value"),
      caption  = "Shading = severity class (Mild / Moderate / Severe / Extreme)"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 9,  color = "grey40"),
      plot.caption     = element_text(size = 8,  color = "grey50"),
      panel.grid.major = element_line(color = "grey92"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 9),
      legend.text      = element_text(size = 9),
      axis.text        = element_text(size = 9)
    )
}

p_spi1  <- plot_spi_droughts(country_avg, drought_spi1,  "spi1",  "SPI-1",  drought_config)
p_spi3  <- plot_spi_droughts(country_avg, drought_spi3,  "spi3",  "SPI-3",  drought_config)
p_spi6  <- plot_spi_droughts(country_avg, drought_spi6,  "spi6",  "SPI-6",  drought_config)
p_spi12 <- plot_spi_droughts(country_avg, drought_spi12, "spi12", "SPI-12", drought_config)

print(p_spi1)
print(p_spi3)
print(p_spi6)
print(p_spi12)

p_spi1 / p_spi3 / p_spi6 / p_spi12 +
  plot_annotation(
    title    = "Multi-scale SPI Drought Detection",
    subtitle = drought_method_label,
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave("./Figures/drought_events_spi_def3.png", width = 14, height = 18, dpi = 300)



# ─────────────────────────────────────────────────────────────────────────────
# Drought summary across all SPI scales
# Filter Inf rows first — these occur when the last event is still ongoing
# (open-ended drought has no end date, producing Inf in date arithmetic)
# ─────────────────────────────────────────────────────────────────────────────

spi_drought_summary <- list(
  SPI1  = drought_spi1,
  SPI3  = drought_spi3,
  SPI6  = drought_spi6,
  SPI12 = drought_spi12
) %>%
  purrr::imap_dfr(function(df, scale_name) {
    
    # ── If no events detected return a row of zeros ───────────────────────────
    if (nrow(df) == 0) {
      return(tibble(
        SPI                   = scale_name,
        n_events              = 0L,
        mean_duration_months  = 0,
        mean_peak_spi         = 0,
        mean_sum_positive_spi = 0,
        mean_total_severity   = 0,
        mean_min_cum_deficit  = 0
      ))
    }
    
    df %>%
      filter(!if_any(everything(), is.infinite)) %>%
      # ── After filtering Inf rows it is possible df becomes empty too ─────────
      { if (nrow(.) == 0) {
        tibble(
          SPI                   = scale_name,
          n_events              = 0L,
          mean_duration_months  = 0,
          mean_peak_spi         = 0,
          mean_sum_positive_spi = 0,
          mean_total_severity   = 0,
          mean_min_cum_deficit  = 0
        )
      } else {
        summarise(.,
                  n_events              = n(),
                  mean_duration_months  = mean(duration_months,  na.rm = TRUE),
                  mean_peak_spi         = mean(peak_spi,         na.rm = TRUE),
                  mean_sum_positive_spi = mean(sum_positive_spi, na.rm = TRUE),
                  mean_total_severity   = mean(total_severity,   na.rm = TRUE),
                  mean_min_cum_deficit  = mean(min_cum_deficit,  na.rm = TRUE)
        ) %>%
          mutate(SPI = scale_name)
      }
      }
  }) %>%
  relocate(SPI, n_events)

print(spi_drought_summary)


write.csv(spi_drought_summary, "./Data/spi_drought_summary_def3.csv")


