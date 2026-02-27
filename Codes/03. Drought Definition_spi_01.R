###############################################################################
# No need to run above as we saved the file - just load the file 

# plot the SPI

spi_results <- readRDS("./Data/spi_results.rds")

# ── Build country average ONCE with all three indices ─────────────────────────
country_avg <- spi_results |>
  group_by(year, month) |>
  summarise(
    spi1  = mean(spi1, na.rm = TRUE),
    spi3  = mean(spi3,  na.rm = TRUE),
    spi6  = mean(spi6,  na.rm = TRUE),
    spi12 = mean(spi12, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year, month) |>
  mutate(
    month   = as.integer(month),
    year    = as.integer(year),
    x_idx   = row_number(),
    x_label = as.character(month)
  )


# based on calculated SPI - we will identify a drought event - based on cumilative SPI values 
# Definition of drought : Drought start with a (-) SPI value, this statring tresholds needs to be
# finalize, once it start with (-) SPI we will get the cumilative sum of (-) values
# and after coming (+) SPI, we do not define end of a drought as a beginign of (+),
# with this criteria the cumilative (+) should be equal to the cumilateive (-) which
# occured in previous dry month 
# Working on Frequency, duration and magnitude - here we depend of few definition 
# on defining tresholds

# option 1: CUMULATIVE SPI DROUGHT DETECTION

# ─────────────────────────────────────────────────────────────
# Cumilative SPI detection
# Based on Yevjevich (1967) Run Theory + Cumulative Deficit
#
# Your dataframe: country_avg
# Columns used:   year, month, spi3, spi6, spi12
#
# Algorithm per SPI variable:
#   1. Drought Start : SPI drops below threshold (-1.0 WMO standard)
#   2. Accumilate    : cumsum of SPI while in drought (tracks total deficit)
#   3. Drought end   : cumsum returns to 0 (surplus has repaid full deficit)
#   4. Record        : duration, peak SPI, total severity, start/end date
# ─────────────────────────────────────────────────────────────


# ── Step 1: Add a proper date column for readability ──
country_avg <- country_avg %>%
  select(-x_idx, -x_label)%>%
  arrange(year, month) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


# ─────────────────────────────────────────────────────────────
# Core drought detection function
# Inputs:
#   spi_vec   : vector of SPI values in chronological order
#   dates     : matching vector of dates
#   threshold : drought start trigger (default -1.0)
#
# Returns a dataframe of drought EVENTS with:
#   drought_id       : sequential event number
#   start_date       : first month SPI crossed threshold
#   end_date         : month cumulative deficit returned to 0
#   duration_months  : total length of event
#   peak_spi         : worst single month SPI during event
#   peak_date        : when peak occurred
#   total_severity   : sum of all SPI values during event (total deficit)


# ─────────────────────────────────────────────────────────────
detect_droughts <- function(spi_vec, dates, threshold = -1.0) {   # check with Caily thresholds is it ok to start value needs to be -1 or any negative value
  
  n          <- length(spi_vec)
  in_drought <- FALSE      # are we currently inside a drought?
  cum_deficit <- 0         # running cumulative SPI sum
  drought_id  <- 0         # event counter
  
  # Per-month tracking
  event_id   <- rep(NA_integer_, n)
  cum_track  <- rep(NA_real_,    n)
  
  for (i in seq_len(n)) {
    
    val <- spi_vec[i]
    if (is.na(val)) next
    
    if (!in_drought) {
      # ── OUTSIDE drought: watch for threshold crossing ──
      if (val < threshold) {
        in_drought  <- TRUE
        drought_id  <- drought_id + 1
        cum_deficit <- val          # start accumulating
        event_id[i] <- drought_id
        cum_track[i] <- cum_deficit
      }
      # If SPI is positive and we are not in drought, cumsum stays 0
      
    } else {
      # ── INSIDE drought: keep accumulating regardless of sign ──
      cum_deficit  <- cum_deficit + val
      event_id[i]  <- drought_id
      cum_track[i] <- cum_deficit
      
      # ── DROUGHT END: cumulative deficit repaid (crosses back to >= 0) ──
      if (cum_deficit >= 0) {
        in_drought  <- FALSE
        cum_deficit <- 0            # reset — next drought starts fresh
      }
    }
  }
  
  # ── If still in drought at end of record, close it open-ended ──
  if (in_drought) {
    message("Note: drought event #", drought_id, " is ongoing at end of record (open-ended event)")
  }
  
  # ── Summarise into one row per event ──
  if (drought_id == 0) {
    message("No drought events detected with this threshold.")
    return(tibble())
  }
  
  monthly_track <- tibble(
    date       = dates,
    spi        = spi_vec,
    event_id   = event_id,
    cum_deficit = cum_track
  ) %>% filter(!is.na(event_id))
  
  events <- monthly_track %>%
    group_by(drought_id = event_id) %>%
    summarise(
      start_date      = min(date),
      end_date        = max(date),
      duration_months = n(),
      peak_spi        = min(spi,         na.rm = TRUE),   # most negative = worst
      peak_date       = date[which.min(spi)],
      
      # Sum of negative SPI only
      #sum_negative_spi = sum(spi[spi < 0], na.rm = TRUE),
      
      # Sum of positive SPI only
      sum_positive_spi = sum(spi[spi > 0], na.rm = TRUE),
      
      # Total severity = negative + positive
      total_severity   = sum(spi, na.rm = TRUE),
      
      #csum_negative_SPI = sum(pmin(spi, 0), na.rm = TRUE),
      #total_severity  = sum(spi,         na.rm = TRUE),   # cumulative deficit
      min_cum_deficit = min(cum_deficit, na.rm = TRUE),   # deepest cumulative point
      
      
      
      
      .groups = "drop"
    ) %>%
    mutate(
      # Drought category based on peak SPI (WMO classification)
      severity_class = case_when(
        peak_spi >= -1.0                    ~ "Mild",
        peak_spi >= -1.5 & peak_spi < -1.0 ~ "Moderate",
        peak_spi >= -2.0 & peak_spi < -1.5 ~ "Severe",
        peak_spi <  -2.0                    ~ "Extreme"
      )
    )
  
  return(events)
}

# ─────────────────────────────────────────────────────────────
# STEP 2: Run drought detection for all three SPI timescales
# ─────────────────────────────────────────────────────────────
message("Detecting droughts...")

drought_threshold <- -0.5 # ← change this to -0.5, -1.5, or -2.0 if needed

drought_spi1  <- detect_droughts(country_avg$spi1,  country_avg$date, threshold = drought_threshold)
drought_spi3  <- detect_droughts(country_avg$spi3,  country_avg$date, threshold = drought_threshold)
drought_spi6  <- detect_droughts(country_avg$spi6,  country_avg$date, threshold = drought_threshold)
drought_spi12 <- detect_droughts(country_avg$spi12, country_avg$date, threshold = drought_threshold)

message("Done!")


# ─────────────────────────────────────────────────────────────
# STEP 3: View results
# ─────────────────────────────────────────────────────────────

# Full event catalogue for SPI3
print(drought_spi3)

# Quick summary: how many events, average duration, worst event
cat("\n── SPI1 Drought Summary ──\n")                                         
cat("Total events     :", nrow(drought_spi1), "\n")                            
cat("Avg duration     :", round(mean(drought_spi1$duration_months), 1), "months\n") 
cat("Longest event    :", max(drought_spi1$duration_months), "months  |",      
    format(drought_spi1$start_date[which.max(drought_spi1$duration_months)], "%b %Y"),  
    "→",                                                                        
    format(drought_spi1$end_date[which.max(drought_spi1$duration_months)],   "%b %Y"), "\n") 
cat("Most severe event:", round(min(drought_spi1$total_severity), 2), "(total SPI deficit)  |",  
    format(drought_spi1$start_date[which.min(drought_spi1$total_severity)], "%b %Y"), "\n")      

cat("\n── SPI3 Drought Summary ──\n")
cat("Total events     :", nrow(drought_spi3), "\n")
cat("Avg duration     :", round(mean(drought_spi3$duration_months), 1), "months\n")
cat("Longest event    :", max(drought_spi3$duration_months), "months  |",
    format(drought_spi3$start_date[which.max(drought_spi3$duration_months)], "%b %Y"),
    "→",
    format(drought_spi3$end_date[which.max(drought_spi3$duration_months)],   "%b %Y"), "\n")
cat("Most severe event:", round(min(drought_spi3$total_severity), 2), "(total SPI deficit)  |",
    format(drought_spi3$start_date[which.min(drought_spi3$total_severity)], "%b %Y"), "\n")

cat("\n── SPI6 Drought Summary ──\n")
cat("Total events     :", nrow(drought_spi6), "\n")
cat("Avg duration     :", round(mean(drought_spi6$duration_months), 1), "months\n")

cat("\n── SPI12 Drought Summary ──\n")
cat("Total events     :", nrow(drought_spi12), "\n")
cat("Avg duration     :", round(mean(drought_spi12$duration_months), 1), "months\n")



# ─────────────────────────────────────────────────────────────
# STEP 4: Add per-month drought tracking back onto country_avg
# Useful for plotting the cumulative deficit through time
# ─────────────────────────────────────────────────────────────
add_cumulative_track <- function(spi_vec, dates, threshold = -1.0) {
  n           <- length(spi_vec)
  in_drought  <- FALSE
  cum_deficit <- 0
  drought_id  <- 0
  event_id    <- rep(NA_integer_, n)
  cum_track   <- rep(0,           n)
  
  for (i in seq_len(n)) {
    val <- spi_vec[i]
    if (is.na(val)) next
    if (!in_drought) {
      if (val < threshold) {
        in_drought  <- TRUE
        drought_id  <- drought_id + 1
        cum_deficit <- val
        event_id[i] <- drought_id
        cum_track[i] <- cum_deficit
      }
    } else {
      cum_deficit   <- cum_deficit + val
      event_id[i]   <- drought_id
      cum_track[i]  <- cum_deficit
      if (cum_deficit >= 0) {
        in_drought  <- FALSE
        cum_deficit <- 0
      }
    }
  }
  return(list(event_id = event_id, cum_deficit = cum_track))
}

# Attach tracking columns to country_avg
track1  <- add_cumulative_track(country_avg$spi1,  country_avg$date, drought_threshold)  
track3  <- add_cumulative_track(country_avg$spi3,  country_avg$date, drought_threshold)
track6  <- add_cumulative_track(country_avg$spi6,  country_avg$date, drought_threshold)
track12 <- add_cumulative_track(country_avg$spi12, country_avg$date, drought_threshold)

country_avg <- country_avg %>%
  mutate(
    # ── ADDED: SPI1 tracking columns ────────────────────────────────────────
    drought_id_spi1   = track1$event_id,
    cum_deficit_spi1  = track1$cum_deficit,
    # ── END ADDITION ─────────────────────────────────────────────────────────
    
    # Which drought event each month belongs to (NA = not in drought)
    drought_id_spi3  = track3$event_id,
    drought_id_spi6  = track6$event_id,
    drought_id_spi12 = track12$event_id,
    # Running cumulative SPI deficit (0 outside drought, negative inside)
    cum_deficit_spi3  = track3$cum_deficit,
    cum_deficit_spi6  = track6$cum_deficit,
    cum_deficit_spi12 = track12$cum_deficit
  )

# Preview the monthly tracking
# ── ADDED: preview for spi1 alongside spi3 ──────────────────────────────────
country_avg %>%
  filter(!is.na(drought_id_spi1)) %>%
  select(date, spi1, drought_id_spi1, cum_deficit_spi1) %>%
  head(20)
# ── END ADDITION ─────────────────────────────────────────────────────────────

country_avg %>%
  filter(!is.na(drought_id_spi3)) %>%
  select(date, spi3, drought_id_spi3, cum_deficit_spi3) %>%
  head(20)


library(dplyr)
library(ggplot2)
library(lubridate)

# ─────────────────────────────────────────────────────────────
# PLOT SPI TIME SERIES WITH DROUGHT EVENT MARKERS
# Red   vertical line = drought START
# Green vertical line = drought END
# Grey  shaded band   = drought period
# ─────────────────────────────────────────────────────────────

plot_spi_droughts <- function(country_avg, drought_events, spi_col, title_label) {
  
  plot_data <- country_avg %>%
    select(date, spi = all_of(spi_col)) %>%
    filter(!is.na(spi))
  
  if (nrow(drought_events) > 0) {
    drought_bands <- drought_events %>%
      mutate(
        xmin = start_date,
        xmax = end_date + days(15)
      )
  } else {
    drought_bands <- NULL
  }
  
  severity_colors <- c(
    "Mild"     = "#FFF9C4",
    "Moderate" = "#FFCC80",
    "Severe"   = "#EF9A9A",
    "Extreme"  = "#CE93D8"
  )
  
  p <- ggplot(plot_data, aes(x = date, y = spi)) +
    geom_hline(yintercept = 0,    color = "#888888", linewidth = 0.4, linetype = "solid") +
    geom_hline(yintercept = -1.0, color = "#FF5722", linewidth = 0.4, linetype = "dashed") +
    {if (!is.null(drought_bands))
      geom_rect(
        data = drought_bands,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = severity_class),
        inherit.aes = FALSE, alpha = 0.25
      )
    } +
    scale_fill_manual(values = severity_colors, name = "Drought severity") +
    geom_line(color = "#5C9BD6", linewidth = 0.55, alpha = 0.9) +
    geom_line(
      data = plot_data %>% mutate(spi_neg = ifelse(spi < -1.0, spi, NA)),
      aes(y = spi_neg),
      color = "#E53935", linewidth = 0.7
    ) +
    {if (!is.null(drought_bands))
      geom_vline(
        data        = drought_bands,
        aes(xintercept = as.numeric(xmin)),
        color       = "#E53935",
        linewidth   = 0.8,
        linetype    = "solid",
        inherit.aes = FALSE
      )
    } +
    {if (!is.null(drought_bands))
      geom_vline(
        data        = drought_bands,
        aes(xintercept = as.numeric(end_date)),
        color       = "#43A047",
        linewidth   = 0.8,
        linetype    = "solid",
        inherit.aes = FALSE
      )
    } +
    {if (!is.null(drought_bands))
      geom_label(
        data = drought_bands,
        aes(
          x     = xmin + (end_date - xmin) / 2,
          y     = Inf,
          label = paste0("D", drought_id)
        ),
        inherit.aes = FALSE,
        vjust       = 1.3,
        size        = 2.8,
        color       = "#333333",
        fill        = "white",
        label.size  = 0.2,
        fontface    = "bold"
      )
    } +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0)) +
    scale_y_continuous(breaks = seq(-3, 3, by = 1)) +
    labs(
      title    = paste(title_label, "— Drought Events (Run Theory)"),
      subtitle = paste(
        "Red line = drought start (SPI < -1.0) |",
        "Green line = drought end (cumulative deficit repaid) |",
        nrow(drought_events), "events detected"
      ),
      x        = NULL,
      y        = paste(title_label, "value"),
      caption  = "Shading intensity = severity class (Mild/Moderate/Severe/Extreme)"
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
  
  return(p)
}


# ─────────────────────────────────────────────────────────────
# GENERATE PLOTS FOR SPI1, SPI3, SPI6, SPI12
# ─────────────────────────────────────────────────────────────

p_spi1  <- plot_spi_droughts(country_avg, drought_spi1,  "spi1",  "SPI-1")   
p_spi3  <- plot_spi_droughts(country_avg, drought_spi3,  "spi3",  "SPI-3")
p_spi6  <- plot_spi_droughts(country_avg, drought_spi6,  "spi6",  "SPI-6")
p_spi12 <- plot_spi_droughts(country_avg, drought_spi12, "spi12", "SPI-12")

# ── View individually ──
print(p_spi1)                                                                  
print(p_spi3)
print(p_spi6)
print(p_spi12)

# ── Stack all four vertically ──                                              
library(patchwork)
p_spi1 / p_spi3 / p_spi6 / p_spi12 +                                         
  plot_annotation(
    title    = "Multi-scale SPI Drought Detection",
    subtitle = "Cumulative deficit run theory | Threshold = -1.0",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

# ── Save ──
ggsave("drought_events_spi.png", width = 14, height = 16, dpi = 300)         
message("Plot saved as drought_events_spi.png")


#### Average values across all drought events - Caily's asking single value table

drought_spi1_summary <- drought_spi1 %>%
  filter(!if_any(everything(), is.infinite))%>% # this is becsue if the last date is still runnign drought it come as inf
  summarise(
    mean_duration_months = mean(duration_months, na.rm = TRUE),
    mean_peak_spi = mean(peak_spi, na.rm = TRUE),
    mean_sum_positive_spi = mean(sum_positive_spi, na.rm = TRUE),
    mean_total_severity = mean(total_severity, na.rm = TRUE),
    mean_min_cum_deficit = mean(min_cum_deficit, na.rm = TRUE)
  )%>%
  mutate(SPI = "SPI1")


drought_spi3_summary <- drought_spi3 %>%
  filter(!if_any(everything(), is.infinite))%>% # this is becsue if the last date is still runnign drought it come as inf
  summarise(
    mean_duration_months = mean(duration_months, na.rm = TRUE),
    mean_peak_spi = mean(peak_spi, na.rm = TRUE),
    mean_sum_positive_spi = mean(sum_positive_spi, na.rm = TRUE),
    mean_total_severity = mean(total_severity, na.rm = TRUE),
    mean_min_cum_deficit = mean(min_cum_deficit, na.rm = TRUE)
  )%>%
  mutate(SPI = "SPI3")


drought_spi6_summary <- drought_spi6 %>%
  filter(!if_any(everything(), is.infinite))%>% # this is becsue if the last date is still runnign drought it come as inf
  summarise(
    mean_duration_months = mean(duration_months, na.rm = TRUE),
    mean_peak_spi = mean(peak_spi, na.rm = TRUE),
    mean_sum_positive_spi = mean(sum_positive_spi, na.rm = TRUE),
    mean_total_severity = mean(total_severity, na.rm = TRUE),
    mean_min_cum_deficit = mean(min_cum_deficit, na.rm = TRUE)
  )%>%
  mutate(SPI = "SPI6")


drought_spi12_summary <- drought_spi12 %>%
  filter(!if_any(everything(), is.infinite))%>% # this is becsue if the last date is still runnign drought it come as inf
  summarise(
    mean_duration_months = mean(duration_months, na.rm = TRUE),
    mean_peak_spi = mean(peak_spi, na.rm = TRUE),
    mean_sum_positive_spi = mean(sum_positive_spi, na.rm = TRUE),
    mean_total_severity = mean(total_severity, na.rm = TRUE),
    mean_min_cum_deficit = mean(min_cum_deficit, na.rm = TRUE)
  )%>%
  mutate(SPI = "SPI12")


spi_drought_sumamry_def1 <- rbind(drought_spi1_summary,
                                  drought_spi3_summary,
                                  drought_spi6_summary,
                                  drought_spi12_summary)

write.csv(spi_drought_sumamry_def1, "Data/spi_drought_summary_def1")



