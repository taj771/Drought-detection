# =============================================================================
# 07. Analysis 2 — Cross-Period Comparison: Do definitions agree on how
#     drought has changed between reference and comparison periods?
# =============================================================================
# Analysis Plan Reference: Phase 2 Section 2.3.2
#
# Step 1: % change in each characteristic between reference (1973–2002) and
#         comparison (2003–2023) periods.
# Step 2: Friedman test on comparison period annual values (n=21 years).
# Step 3: Rank stability check — compare avg rank order between periods.
#
# Inputs (VOLUME_PATH):
#   drought_annual_summary_ref.rds   — reference period (from Script 03 / 05)
#   drought_annual_summary_comp.rds  — comparison period (same structure)
#   analysis1_avg_ranks.rds          — avg ranks from Script 06
#
# Outputs (VOLUME_PATH):
#   analysis2_pct_change.rds         — % change per config × country × characteristic
#   analysis2_friedman_comp.rds      — Friedman results on comparison period
#   analysis2_rank_stability.rds     — rank order comparison ref vs comp
#   analysis2_summary.rds            — combined summary table
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)


library(arrow)   # read v2 .parquet inputs

# -- Configuration ------------------------------------------------------------
# v2 pipeline writes .parquet to drought_outputs/v2/<country>. The runner
# (_run_r_step) sets VOLUME_PATH from the COUNTRY job parameter; the fallback
# below only applies when this script is run standalone.
if (!exists("VOLUME_PATH")) {
  COUNTRY_TAG <- if (exists("COUNTRY_TAG")) COUNTRY_TAG else "Benin"
  VOLUME_PATH <- file.path("/Volumes/projects/gwsc_drought/drought_outputs/v2",
                           COUNTRY_TAG)
}
REF_START    <- 1973
REF_END      <- 2002
COMP_START   <- 2003
COMP_END     <- 2023
P_THRESHOLD  <- 0.05


# -- Load data ----------------------------------------------------------------
# drought_summary_yearly_ref.rds covers full detection period (1970–2023).
# Split here into reference and comparison periods by year filter.
# Column rename matches script 06 convention.
message("Loading annual summaries...")
yearly_full <- arrow::read_parquet(file.path(VOLUME_PATH, "drought_summary_yearly_ref.parquet")) %>%
  rename(
    annual_freq_events   = mean_events_per_pixel,
    annual_mean_duration = mean_duration_months,
    annual_mean_peak     = mean_peak_index,
    annual_pct_pixels    = pct_pixels_affected
  )

# Compute cv_freq per config × country (needed by char_cols map)
cv_lookup <- yearly_full %>%
  group_by(config_id, country) %>%
  summarise(
    cv_freq = round(sd(annual_freq_events, na.rm = TRUE) /
                    max(mean(annual_freq_events, na.rm = TRUE), 1e-9), 3),
    .groups = "drop"
  )
yearly_full <- yearly_full %>% left_join(cv_lookup, by = c("config_id", "country"))

# Reference avg ranks from Script 06. The stratified redesign writes
# analysis1_strat_avg_ranks.rds (per-threshold), not the old flat
# analysis1_avg_ranks.rds. Load whichever exists; Step 3 (rank stability) is
# guarded below and skipped if the expected schema is absent.
.ranks_flat  <- file.path(VOLUME_PATH, "analysis1_avg_ranks.rds")
.ranks_strat <- file.path(VOLUME_PATH, "analysis1_strat_avg_ranks.rds")
avg_ranks_ref <- if (file.exists(.ranks_flat)) {
  readRDS(.ranks_flat)
} else if (file.exists(.ranks_strat)) {
  message("Using stratified avg ranks (analysis1_strat_avg_ranks.rds).")
  readRDS(.ranks_strat)
} else {
  message("No avg-ranks file found — Step 3 rank stability will be skipped.")
  NULL
}

annual_ref  <- yearly_full %>% filter(event_year >= REF_START,  event_year <= REF_END)
annual_comp <- yearly_full %>% filter(event_year >= COMP_START, event_year <= COMP_END)
rm(yearly_full)

countries <- intersect(unique(annual_ref$country), unique(annual_comp$country))

cat(sprintf("Reference  : %d–%d  (%d years)\n",
            REF_START, REF_END, length(unique(annual_ref$event_year))))
cat(sprintf("Comparison : %d–%d  (%d years)\n",
            COMP_START, COMP_END, length(unique(annual_comp$event_year))))
cat(sprintf("Countries  : %s\n", paste(countries, collapse = ", ")))


# =============================================================================
# CHARACTERISTICS MAP
# =============================================================================

char_cols <- c(
  frequency  = "annual_freq_events",
  duration   = "annual_mean_duration",
  severity   = "annual_mean_peak",
  cv         = "cv_freq",
  pct_pixels = "annual_pct_pixels"
)


# =============================================================================
# STEP 1 — Percentage change between periods (Analysis Plan Section 2.3.2 Step 1)
# =============================================================================

message("\n=== Step 1: Percentage change ref → comparison ===")

period_means <- function(df, period_label) {
  df %>%
    group_by(config_id, country) %>%
    summarise(across(all_of(unname(char_cols)),
                     \(x) mean(x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(period = period_label)
}

ref_means  <- period_means(annual_ref,  "reference")
comp_means <- period_means(annual_comp, "comparison")

pct_change_all <- map_dfr(names(char_cols), function(char_name) {

  col <- char_cols[char_name]

  ref_vals  <- ref_means  %>% select(config_id, country, value = all_of(unname(col)))
  comp_vals <- comp_means %>% select(config_id, country, value = all_of(unname(col)))

  inner_join(ref_vals, comp_vals, by = c("config_id", "country"),
             suffix = c("_ref", "_comp")) %>%
    mutate(
      characteristic = char_name,
      pct_change     = round(
        ifelse(
          value_ref == 0,
          NA_real_,
          (value_comp - value_ref) / abs(value_ref) * 100
        ),
        1
      ),
      # Severity (annual_mean_peak) is a negative number: more negative = worse.
      # Raw formula produces negative pct_change when severity worsens, which
      # reads as "decrease" — inverted meaning. Flip sign so positive = worsening.
      pct_change = ifelse(
        char_name == "severity",
        -pct_change,
        pct_change
      ),
      direction = case_when(
        is.na(pct_change)    ~ "undefined",
        pct_change  > 0      ~ "increase",
        pct_change  < 0      ~ "decrease",
        TRUE                 ~ "no_change"
      )
    )
})

# Recompute pct_change from value_ref/value_comp to guard against join issues
pct_change_all <- pct_change_all %>%
  mutate(
    pct_change = round(
      ifelse(
        is.na(value_ref) | value_ref == 0,
        NA_real_,
        (value_comp - value_ref) / abs(value_ref) * 100
      ),
      1
    ),
    pct_change = ifelse(characteristic == "severity", -pct_change, pct_change),
    direction  = case_when(
      is.na(pct_change) ~ "undefined",
      pct_change  > 0   ~ "increase",
      pct_change  < 0   ~ "decrease",
      TRUE              ~ "no_change"
    )
  )

cat(sprintf("%% change rows: %d\n", nrow(pct_change_all)))
saveRDS(pct_change_all, file.path(VOLUME_PATH, "analysis2_pct_change.rds"))
message("Saved: analysis2_pct_change.rds")

# Summary: direction consistency across configs per country × characteristic
direction_consistency <- pct_change_all %>%
  filter(!is.na(pct_change)) %>%
  group_by(country, characteristic) %>%
  summarise(
    n_configs      = n(),
    pct_increase   = round(100 * mean(direction == "increase"), 1),
    pct_decrease   = round(100 * mean(direction == "decrease"), 1),
    median_pct_change = round(median(pct_change, na.rm = TRUE), 1),
    consistent_direction = pct_increase >= 80 | pct_decrease >= 80,
    .groups = "drop"
  )

cat("\n=== Direction consistency (≥80% configs agree = consistent) ===\n")
print(direction_consistency, row.names = FALSE)


# =============================================================================
# FRIEDMAN ON COMPARISON PERIOD (Analysis Plan Section 2.3.2 Step 2)
# =============================================================================

message("\n=== Step 2: Friedman test on comparison period ===")

run_friedman <- function(wide_mat, characteristic) {

  wide_mat <- as.matrix(wide_mat)
  wide_mat <- wide_mat[, colSums(!is.na(wide_mat)) > 0, drop = FALSE]
  wide_mat <- wide_mat[rowSums(!is.na(wide_mat)) > 0, , drop = FALSE]

  n_obs     <- nrow(wide_mat)
  n_configs <- ncol(wide_mat)

  if (n_obs < 3 || n_configs < 2) return(NULL)

  rank_mat <- t(apply(wide_mat, 1, function(row) {
    rank(row, ties.method = "average", na.last = "keep")
  }))

  avg_ranks <- sort(colMeans(rank_mat, na.rm = TRUE))
  R_j       <- colSums(rank_mat, na.rm = TRUE)
  Q_raw     <- (12 / (n_obs * n_configs * (n_configs + 1))) * sum(R_j^2) -
                3 * n_obs * (n_configs + 1)

  tie_correction <- mean(apply(wide_mat, 1, function(row) {
    tbl <- table(row[!is.na(row)])
    sum(tbl^3 - tbl) / (n_configs * (n_configs^2 - 1))
  }))
  Q     <- if (tie_correction < 1) Q_raw / (1 - tie_correction) else Q_raw

  df    <- n_configs - 1
  p_val <- pchisq(Q, df = df, lower.tail = FALSE)
  S         <- sum((R_j - mean(R_j))^2)
  W         <- 12 * S / (n_obs^2 * (n_configs^3 - n_configs))

  list(
    friedman = data.frame(
      characteristic = characteristic,
      n_years        = n_obs,
      n_configs      = n_configs,
      friedman_Q     = round(Q, 4),
      df             = df,
      p_value        = round(p_val, 6),
      kendall_W      = round(W, 4),
      significant    = p_val <= P_THRESHOLD,
      stringsAsFactors = FALSE
    ),
    avg_ranks = data.frame(
      config_id      = names(avg_ranks),
      avg_rank_comp  = round(unname(avg_ranks), 4),
      characteristic = characteristic,
      stringsAsFactors = FALSE
    )
  )
}

friedman_comp_all  <- list()
avg_ranks_comp_all <- list()

for (ctry in countries) {

  cat(sprintf("\n  Country: %s\n", ctry))
  ctry_comp <- annual_comp %>% filter(country == ctry)

  for (char_name in names(char_cols)) {

    col <- char_cols[char_name]

    wide <- ctry_comp %>%
      select(config_id, event_year, value = all_of(unname(col))) %>%
      pivot_wider(names_from = config_id, values_from = value,
                  values_fn = mean) %>%
      arrange(event_year) %>%
      select(-event_year)

    result <- tryCatch(run_friedman(wide, char_name), error = function(e) NULL)
    if (is.null(result)) next

    result$friedman$country   <- ctry
    result$avg_ranks$country  <- ctry

    friedman_comp_all[[length(friedman_comp_all) + 1]]   <- result$friedman
    avg_ranks_comp_all[[length(avg_ranks_comp_all) + 1]] <- result$avg_ranks

    cat(sprintf("    %-12s Q=%.2f p=%.4f W=%.3f %s\n",
                char_name,
                result$friedman$friedman_Q,
                result$friedman$p_value,
                result$friedman$kendall_W,
                ifelse(result$friedman$significant, "***", "")))
  }
}

friedman_comp  <- do.call(rbind, friedman_comp_all)
avg_ranks_comp <- do.call(rbind, avg_ranks_comp_all)

saveRDS(friedman_comp, file.path(VOLUME_PATH, "analysis2_friedman_comp.rds"))
message("Saved: analysis2_friedman_comp.rds")


# =============================================================================
# STEP 3 — Rank stability (Analysis Plan Section 2.3.2 Step 2 — descriptive)
# Cross-period rank comparison: ref avg rank vs comparison avg rank per config.
# Not a statistical test — descriptive rank ordering comparison only.
# =============================================================================

message("\n=== Step 3: Rank stability check (descriptive) ===")

# Guard: needs a reference avg-ranks frame with the flat schema
# (config_id, country, characteristic, calibrated, avg_rank). The stratified
# redesign (Script 06) writes a per-threshold frame without `calibrated`, so
# skip this descriptive step when the expected columns are missing.
.have_ranks <- !is.null(avg_ranks_ref) &&
  all(c("config_id", "country", "characteristic", "calibrated", "avg_rank")
      %in% names(avg_ranks_ref))

if (!.have_ranks) {
  message("Skipping Step 3 (rank stability) — reference avg ranks not in the ",
          "expected flat schema. Re-run Script 06's flat-rank output to enable.")
} else {
  avg_ranks_ref_raw <- avg_ranks_ref %>%
    filter(!calibrated) %>%
    select(config_id, country, characteristic, avg_rank_ref = avg_rank)

  rank_stability <- inner_join(
    avg_ranks_ref_raw,
    avg_ranks_comp,
    by = c("config_id", "country", "characteristic")
  ) %>%
    mutate(
      rank_change      = avg_rank_comp - avg_rank_ref,
      rank_change_abs  = abs(rank_change),
      rank_direction   = case_when(
        rank_change  > 0.5  ~ "rank_up",
        rank_change  < -0.5 ~ "rank_down",
        TRUE                ~ "stable"
      )
    ) %>%
    arrange(country, characteristic, avg_rank_ref)

  rank_correlation <- rank_stability %>%
    group_by(country, characteristic) %>%
    summarise(
      spearman_r  = round(cor(avg_rank_ref, avg_rank_comp,
                              method = "spearman", use = "complete.obs"), 3),
      n_stable    = sum(rank_direction == "stable"),
      n_rank_up   = sum(rank_direction == "rank_up"),
      n_rank_down = sum(rank_direction == "rank_down"),
      .groups = "drop"
    )

  cat("\n=== Rank correlation ref vs comparison period ===\n")
  print(rank_correlation, row.names = FALSE)

  saveRDS(rank_stability, file.path(VOLUME_PATH, "analysis2_rank_stability.rds"))
  message("Saved: analysis2_rank_stability.rds")
}


# =============================================================================
# COMBINED SUMMARY TABLE
# =============================================================================

summary_combined <- pct_change_all %>%
  group_by(country, characteristic) %>%
  summarise(
    median_pct_change = round(median(pct_change, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  left_join(
    friedman_comp %>%
      select(country, characteristic, comp_p = p_value, comp_W = kendall_W,
             comp_sig = significant),
    by = c("country", "characteristic")
  )

# Add Spearman rank correlation only if Step 3 ran (guarded above).
if (exists("rank_correlation")) {
  summary_combined <- summary_combined %>%
    left_join(
      rank_correlation %>% select(country, characteristic, spearman_r),
      by = c("country", "characteristic")
    )
} else {
  summary_combined$spearman_r <- NA_real_
}

saveRDS(summary_combined, file.path(VOLUME_PATH, "analysis2_summary.rds"))
message("Saved: analysis2_summary.rds")

cat("\n=== Analysis 2 summary ===\n")
print(summary_combined, row.names = FALSE)

message("\n=== Script 07 complete ===")
message("Next: Script 08 (Appendix A1 figure — visual event inspection)")
