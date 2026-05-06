# =============================================================================
# 06. Analysis 1 — Friedman + Nemenyi: Do definitions agree on drought
#     characteristics within the reference period?
# =============================================================================
# Analysis Plan Reference: Phase 2 Section 2.3.1
#
# For each of 5 drought characteristics (frequency, duration, severity, CV,
# pct_pixels_affected), runs:
#   - Friedman test across 75 configs, ranked within each year (n=30 years)
#   - Nemenyi post-hoc if Friedman p <= 0.05
#   - Kendall's W (effect size)
#
# Calibration rules (GWSC operational):
#   Frequency  : 0 < x < 1 → 1; round to nearest integer
#   Duration   : round to nearest integer
#   Severity   : round to nearest 0.1
#   CV         : round to nearest 0.05
#   Pct pixels : round to nearest 5 pp
#
# Two passes: raw values (for publication) + calibrated values (for GWSC).
#
# Inputs:
#   VOLUME_PATH/drought_annual_summary_ref.rds  — one row per config × country × year
#   (reference period 1973–2002, produced after Script 05 run confirms)
#
# Outputs (VOLUME_PATH):
#   analysis1_friedman_results.rds   — Friedman p, W, df per characteristic
#   analysis1_nemenyi_pairs.rds      — pairwise Nemenyi results (significant pairs)
#   analysis1_equivalence_groups.rds — config equivalence clusters
#   analysis1_avg_ranks.rds          — mean rank per config per characteristic
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)


# -- Configuration ------------------------------------------------------------
VOLUME_PATH  <- "/Volumes/projects/gwsc_drought/drought_outputs"
REF_START    <- 1973
REF_END      <- 2002
P_THRESHOLD  <- 0.05


# -- Load data ----------------------------------------------------------------
# drought_summary_yearly_ref.rds has one row per config × country × year
# (produced by script 03). drought_annual_summary_ref.rds is the aggregated
# version (no event_year) — Friedman needs per-year rows so use the yearly file.
message("Loading reference period yearly summary...")
annual_ref_raw <- readRDS(file.path(VOLUME_PATH, "drought_summary_yearly_ref.rds"))

# Rename script-03 columns to script-06 expected names
annual_ref <- annual_ref_raw %>%
  rename(
    annual_freq_events   = mean_events_per_pixel,
    annual_mean_duration = mean_duration_months,
    annual_mean_peak     = mean_peak_index,
    annual_pct_pixels    = pct_pixels_affected
  ) %>%
  filter(event_year >= REF_START, event_year <= REF_END)

# Compute cv_freq (CV of annual frequency) per config × country across years
cv_lookup <- annual_ref %>%
  group_by(config_id, country) %>%
  summarise(
    cv_freq = round(sd(annual_freq_events, na.rm = TRUE) /
                    max(mean(annual_freq_events, na.rm = TRUE), 1e-9), 3),
    .groups = "drop"
  )
annual_ref <- annual_ref %>%
  left_join(cv_lookup, by = c("config_id", "country"))

rm(annual_ref_raw)

n_years   <- length(unique(annual_ref$event_year))
n_configs <- length(unique(annual_ref$config_id))
countries <- unique(annual_ref$country)

cat(sprintf("Reference period: %d–%d (%d years)\n", REF_START, REF_END, n_years))
cat(sprintf("Configurations  : %d\n", n_configs))
cat(sprintf("Countries       : %s\n", paste(countries, collapse = ", ")))


# =============================================================================
# CALIBRATION FUNCTIONS (Analysis Plan Section 2.3.1 Step 1)
# =============================================================================

calibrate_frequency <- function(x) {
  x_cal <- ifelse(x > 0 & x < 1, 1, x)
  round(x_cal)
}

calibrate_duration <- function(x) {
  round(x)
}

calibrate_severity <- function(x) {
  round(x, 1)
}

calibrate_cv <- function(x) {
  round(x / 0.05) * 0.05
}

calibrate_pct_pixels <- function(x) {
  round(x / 5) * 5
}

apply_calibration <- function(df) {
  df %>%
    mutate(
      freq_cal     = calibrate_frequency(annual_freq_events),
      dur_cal      = calibrate_duration(annual_mean_duration),
      sev_cal      = calibrate_severity(annual_mean_peak),
      cv_cal       = calibrate_cv(cv_freq),
      pct_cal      = calibrate_pct_pixels(annual_pct_pixels)
    )
}


# =============================================================================
# FRIEDMAN + NEMENYI FUNCTION
# =============================================================================
# Input: wide matrix (rows = years, cols = configs), one characteristic.
# Returns: list(friedman, nemenyi_pairs, avg_ranks, kendall_w).

run_friedman_nemenyi <- function(wide_mat, characteristic, calibrated = FALSE) {

  wide_mat <- as.matrix(wide_mat)

  # Remove configs (cols) or years (rows) with all-NA
  wide_mat <- wide_mat[, colSums(!is.na(wide_mat)) > 0, drop = FALSE]
  wide_mat <- wide_mat[rowSums(!is.na(wide_mat)) > 0, , drop = FALSE]

  n_obs     <- nrow(wide_mat)
  n_configs <- ncol(wide_mat)

  if (n_obs < 3 || n_configs < 2) {
    warning(sprintf("Insufficient data for %s (n=%d, k=%d)", characteristic, n_obs, n_configs))
    return(NULL)
  }

  # Rank within each year (row), average ties
  rank_mat <- t(apply(wide_mat, 1, function(row) {
    rank(row, ties.method = "average", na.last = "keep")
  }))

  avg_ranks <- sort(colMeans(rank_mat, na.rm = TRUE))

  # Friedman statistic with ties correction
  # Q_raw = [12 / (n*k*(k+1))] * sum(R_j^2) - 3*n*(k+1)
  # C_ties = sum(t^3 - t) / (n * k * (k^2 - 1))  where t = tie group sizes per row
  R_j   <- colSums(rank_mat, na.rm = TRUE)
  Q_raw <- (12 / (n_obs * n_configs * (n_configs + 1))) * sum(R_j^2) -
            3 * n_obs * (n_configs + 1)

  tie_correction <- mean(apply(wide_mat, 1, function(row) {
    tbl <- table(row[!is.na(row)])
    sum(tbl^3 - tbl) / (n_configs * (n_configs^2 - 1))
  }))
  Q     <- if (tie_correction < 1) Q_raw / (1 - tie_correction) else Q_raw
  df    <- n_configs - 1
  p_val <- pchisq(Q, df = df, lower.tail = FALSE)

  # Kendall's W
  S <- sum((R_j - mean(R_j))^2)
  W <- 12 * S / (n_obs^2 * (n_configs^3 - n_configs))

  friedman_result <- data.frame(
    characteristic = characteristic,
    calibrated     = calibrated,
    n_years        = n_obs,
    n_configs      = n_configs,
    friedman_Q     = round(Q, 4),
    df             = df,
    p_value        = round(p_val, 6),
    kendall_W      = round(W, 4),
    significant    = p_val <= P_THRESHOLD,
    stringsAsFactors = FALSE
  )

  nemenyi_pairs <- data.frame()

  if (p_val <= P_THRESHOLD) {
    # Nemenyi critical difference:
    # CD = q_alpha * sqrt(k*(k+1) / (6*n))
    # q_alpha at alpha=0.05 from Studentised range distribution / sqrt(2)
    # Use approximation: qtukey(0.95, k, Inf) / sqrt(2)
    q_alpha <- qtukey(1 - P_THRESHOLD, nmeans = n_configs, df = Inf) / sqrt(2)
    cd      <- q_alpha * sqrt(n_configs * (n_configs + 1) / (6 * n_obs))

    config_names <- colnames(rank_mat)
    pairs        <- combn(config_names, 2, simplify = FALSE)

    nemenyi_pairs <- map_dfr(pairs, function(pair) {
      diff <- abs(avg_ranks[pair[1]] - avg_ranks[pair[2]])
      data.frame(
        config_a          = pair[1],
        config_b          = pair[2],
        rank_diff         = round(diff, 4),
        critical_distance = round(cd, 4),
        significant       = diff > cd,
        stringsAsFactors  = FALSE
      )
    }) %>%
      filter(significant) %>%
      arrange(desc(rank_diff))

    cat(sprintf("  Nemenyi: %d significant pairs (CD = %.3f)\n",
                nrow(nemenyi_pairs), cd))
  }

  # Equivalence groups via single-linkage clustering on non-significant pairs
  equiv_groups <- assign_equivalence_groups(
    config_names = colnames(rank_mat),
    nemenyi_pairs = nemenyi_pairs,
    all_pairs     = combn(colnames(rank_mat), 2, simplify = FALSE)
  )

  list(
    friedman       = friedman_result,
    nemenyi_pairs  = nemenyi_pairs,
    avg_ranks      = data.frame(
      config_id      = names(avg_ranks),
      avg_rank       = round(unname(avg_ranks), 4),
      characteristic = characteristic,
      calibrated     = calibrated,
      stringsAsFactors = FALSE
    ),
    equiv_groups   = equiv_groups
  )
}


# =============================================================================
# EQUIVALENCE GROUP ASSIGNMENT
# Configs not significantly different from each other → same group.
# Uses union-find (greedy single-linkage).
# =============================================================================

assign_equivalence_groups <- function(config_names, nemenyi_pairs, all_pairs) {

  if (nrow(nemenyi_pairs) == 0) {
    return(data.frame(config_id = config_names, equiv_group = 1L,
                      stringsAsFactors = FALSE))
  }

  # Build non-significant (equivalent) edges — connected components = same group.
  sig_set <- paste(nemenyi_pairs$config_a, nemenyi_pairs$config_b, sep = "||")

  # Union-find with path compression
  parent <- setNames(config_names, config_names)

  find <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]  # path compression (two-step)
      x <- parent[x]
    }
    x
  }

  union <- function(a, b) {
    ra <- find(a); rb <- find(b)
    if (ra != rb) parent[rb] <<- ra
  }

  for (pair in all_pairs) {
    key <- paste(pair[1], pair[2], sep = "||")
    if (!key %in% sig_set) union(pair[1], pair[2])
  }

  roots       <- vapply(config_names, find, character(1))
  unique_roots <- unique(roots)
  relabel     <- setNames(seq_along(unique_roots), unique_roots)

  data.frame(
    config_id   = config_names,
    equiv_group = unname(relabel[roots]),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# MAIN LOOP — run per country, per characteristic, raw + calibrated
# =============================================================================

characteristics <- list(
  raw = c(
    frequency   = "annual_freq_events",
    duration    = "annual_mean_duration",
    severity    = "annual_mean_peak",
    cv          = "cv_freq",
    pct_pixels  = "annual_pct_pixels"
  ),
  cal = c(
    frequency   = "freq_cal",
    duration    = "dur_cal",
    severity    = "sev_cal",
    cv          = "cv_cal",
    pct_pixels  = "pct_cal"
  )
)

all_friedman    <- list()
all_nemenyi     <- list()
all_avg_ranks   <- list()
all_equiv_groups <- list()

for (ctry in countries) {

  cat(sprintf("\n=== Country: %s ===\n", ctry))

  # D1 fix: Friedman always uses RAW values. Calibration applied only for the
  # calibrated-reporting pass — never before the rank matrix is built.
  ctry_raw <- annual_ref %>% filter(country == ctry)
  ctry_cal <- apply_calibration(ctry_raw)

  for (cal_pass in c("raw", "calibrated")) {

    char_cols <- if (cal_pass == "raw") characteristics$raw else characteristics$cal
    is_cal    <- cal_pass == "calibrated"
    ctry_df   <- if (cal_pass == "raw") ctry_raw else ctry_cal

    for (char_name in names(char_cols)) {

      col <- char_cols[char_name]
      cat(sprintf("  [%s] %s ... ", cal_pass, char_name))

      # Guard: duplicate config/year rows would silently average in pivot_wider.
      n_dupes <- nrow(ctry_df) -
        nrow(unique(ctry_df[, c("config_id", "event_year")]))
      if (n_dupes > 0) {
        warning(sprintf(
          "%s [%s] %s: %d duplicate config/year rows — check upstream data",
          ctry, cal_pass, char_name, n_dupes))
      }

      # Wide matrix: rows = years, cols = configs
      wide <- ctry_df %>%
        select(config_id, event_year, value = all_of(unname(col))) %>%
        pivot_wider(names_from = config_id, values_from = value,
                    values_fn = mean) %>%
        arrange(event_year) %>%
        select(-event_year)

      result <- tryCatch(
        run_friedman_nemenyi(wide, char_name, calibrated = is_cal),
        error = function(e) {
          cat(sprintf("ERROR: %s\n", e$message))
          NULL
        }
      )

      if (is.null(result)) next

      result$friedman$country   <- ctry
      result$avg_ranks$country  <- ctry
      if (nrow(result$nemenyi_pairs) > 0) result$nemenyi_pairs$country <- ctry
      result$equiv_groups$country      <- ctry
      result$equiv_groups$characteristic <- char_name
      result$equiv_groups$calibrated   <- is_cal

      all_friedman[[length(all_friedman) + 1]]     <- result$friedman
      all_avg_ranks[[length(all_avg_ranks) + 1]]   <- result$avg_ranks
      if (nrow(result$nemenyi_pairs) > 0)
        all_nemenyi[[length(all_nemenyi) + 1]]     <- result$nemenyi_pairs
      all_equiv_groups[[length(all_equiv_groups) + 1]] <- result$equiv_groups

      cat(sprintf("Q=%.2f, p=%.4f, W=%.3f %s\n",
                  result$friedman$friedman_Q,
                  result$friedman$p_value,
                  result$friedman$kendall_W,
                  ifelse(result$friedman$significant, "*** SIGNIFICANT", "")))
    }
  }
}


# =============================================================================
# COMPILE AND SAVE
# =============================================================================

friedman_results   <- do.call(rbind, all_friedman)
avg_ranks_all      <- do.call(rbind, all_avg_ranks)
nemenyi_pairs_all  <- if (length(all_nemenyi) > 0) do.call(rbind, all_nemenyi) else data.frame()
equiv_groups_all   <- do.call(rbind, all_equiv_groups)

saveRDS(friedman_results,  file.path(VOLUME_PATH, "analysis1_friedman_results.rds"))
saveRDS(nemenyi_pairs_all, file.path(VOLUME_PATH, "analysis1_nemenyi_pairs.rds"))
saveRDS(equiv_groups_all,  file.path(VOLUME_PATH, "analysis1_equivalence_groups.rds"))
saveRDS(avg_ranks_all,     file.path(VOLUME_PATH, "analysis1_avg_ranks.rds"))

message("\n=== Analysis 1 complete ===")
message("Outputs saved:")
message("  analysis1_friedman_results.rds")
message("  analysis1_nemenyi_pairs.rds")
message("  analysis1_equivalence_groups.rds")
message("  analysis1_avg_ranks.rds")

# Quick summary table
cat("\n=== Friedman results summary (raw values) ===\n")
friedman_results %>%
  filter(!calibrated) %>%
  select(country, characteristic, friedman_Q, p_value, kendall_W, significant) %>%
  arrange(country, characteristic) %>%
  print(row.names = FALSE)


# =============================================================================
# STRATIFIED FRIEDMAN — k=15 per WMO-standard threshold (D11 fix)
# =============================================================================
# Flat k=75 Nemenyi CD ≈ 24.9 — too conservative to detect meaningful pairs.
# Stratified: one Friedman per threshold (-0.5, -1.0, -1.5) × country × char.
# k=15 (5 indices × 3 accumulation periods per threshold). CD ≈ 4.8.
# Thresholds 0.0 and -0.25 excluded — not WMO standard operational thresholds.
# Separate Spearman rank correlation tests whether config rank order is stable
# across the three thresholds (threshold robustness check).
# Outputs:
#   analysis1_friedman_stratified.rds  — Friedman results per threshold stratum
#   analysis1_threshold_robustness.rds — Spearman r of avg rank across thresholds
# =============================================================================

message("\n=== Stratified Friedman (k=15 per WMO threshold) ===")

WMO_THRESHOLDS <- c(-0.5, -1.0, -1.5)

# Parse threshold from config_id suffix: S3-T-050 → -0.50, ET6-T-100 → -1.00
# Suffix format: last hyphen-separated token, numeric × (-1/100)
parse_threshold <- function(config_ids) {
  suffix <- sub(".*-", "", config_ids)
  as.numeric(suffix) / -100
}

annual_ref_thr <- annual_ref %>%
  mutate(threshold = parse_threshold(config_id)) %>%
  filter(threshold %in% WMO_THRESHOLDS)

cat(sprintf("Configs after WMO threshold filter: %d\n",
            n_distinct(annual_ref_thr$config_id)))

strat_friedman_all  <- list()
strat_avg_ranks_all <- list()

for (ctry in countries) {
  for (thr in WMO_THRESHOLDS) {

    ctry_thr <- annual_ref_thr %>%
      filter(country == ctry, threshold == thr)

    n_cfg_thr <- n_distinct(ctry_thr$config_id)
    if (n_cfg_thr < 2) next

    for (char_name in names(characteristics$raw)) {

      col <- characteristics$raw[char_name]

      wide <- ctry_thr %>%
        select(config_id, event_year, value = all_of(unname(col))) %>%
        tidyr::pivot_wider(names_from = config_id, values_from = value,
                           values_fn = mean) %>%
        arrange(event_year) %>%
        select(-event_year)

      result <- tryCatch(
        run_friedman_nemenyi(wide, char_name, calibrated = FALSE),
        error = function(e) NULL
      )
      if (is.null(result)) next

      result$friedman$country   <- ctry
      result$friedman$threshold <- thr
      result$avg_ranks$country  <- ctry
      result$avg_ranks$threshold <- thr

      strat_friedman_all[[length(strat_friedman_all) + 1]]   <- result$friedman
      strat_avg_ranks_all[[length(strat_avg_ranks_all) + 1]] <- result$avg_ranks
    }
  }
}

strat_friedman  <- if (length(strat_friedman_all)  > 0) do.call(rbind, strat_friedman_all)  else data.frame()
strat_avg_ranks <- if (length(strat_avg_ranks_all) > 0) do.call(rbind, strat_avg_ranks_all) else data.frame()

saveRDS(strat_friedman,  file.path(VOLUME_PATH, "analysis1_friedman_stratified.rds"))
message("Saved: analysis1_friedman_stratified.rds")

# Quick stratified summary
if (nrow(strat_friedman) > 0) {
  cat("\n=== Stratified Friedman summary ===\n")
  strat_friedman %>%
    select(country, characteristic, threshold, friedman_Q, p_value, kendall_W, significant) %>%
    arrange(country, characteristic, threshold) %>%
    print(row.names = FALSE)
}


# -- Threshold robustness: Spearman r of avg rank order across thresholds -----
# Per country × characteristic: do configs rank the same at -0.5 vs -1.0 vs -1.5?
# High r = threshold choice doesn't change relative config ranking.

if (nrow(strat_avg_ranks) > 0) {

  thr_pairs <- combn(WMO_THRESHOLDS, 2, simplify = FALSE)

  threshold_robustness <- map_dfr(countries, function(ctry) {
    map_dfr(names(characteristics$raw), function(char_name) {
      map_dfr(thr_pairs, function(tp) {
        r1 <- strat_avg_ranks %>%
          filter(country == ctry, characteristic == char_name, threshold == tp[1]) %>%
          select(config_id, r1 = avg_rank)
        r2 <- strat_avg_ranks %>%
          filter(country == ctry, characteristic == char_name, threshold == tp[2]) %>%
          select(config_id, r2 = avg_rank)
        joined <- inner_join(r1, r2, by = "config_id")
        if (nrow(joined) < 3) return(data.frame())
        data.frame(
          country        = ctry,
          characteristic = char_name,
          thr_a          = tp[1],
          thr_b          = tp[2],
          spearman_r     = round(cor(joined$r1, joined$r2,
                                     method = "spearman", use = "complete.obs"), 3),
          n_configs      = nrow(joined),
          stringsAsFactors = FALSE
        )
      })
    })
  })

  saveRDS(threshold_robustness, file.path(VOLUME_PATH, "analysis1_threshold_robustness.rds"))
  message("Saved: analysis1_threshold_robustness.rds")

  cat("\n=== Threshold robustness (Spearman r of rank order across WMO thresholds) ===\n")
  threshold_robustness %>%
    group_by(country, characteristic) %>%
    summarise(mean_r = round(mean(spearman_r, na.rm = TRUE), 3), .groups = "drop") %>%
    arrange(country, characteristic) %>%
    print(row.names = FALSE)
}


# =============================================================================
# LEVEL 3 — Spatial agreement tests (Analysis Plan Section 2.3.1)
# =============================================================================
# Two tests:
#   A. Spatial rank correlation — Spearman r of pixel drought freq ranks
#      across configs per country. High r = definitions agree on where worst.
#   B. Hotspot agreement map — % of configs classifying each pixel as hotspot
#      (freq >= 75th percentile within country). Pixels where most configs agree
#      = robust hotspots for GWSC targeting.
#
# Input: pixel_freq_ref.rds (per pixel × config, ref period total events)
# Outputs:
#   analysis1_spatial_rank_corr.rds  — Spearman r per country × config pair
#   analysis1_hotspot_agreement.rds  — per pixel × country: pct configs hotspot
# =============================================================================

message("\n=== Level 3: Spatial agreement tests ===")

pf_path <- file.path(VOLUME_PATH, "pixel_freq_ref.rds")

if (!file.exists(pf_path)) {
  message("  pixel_freq_ref.rds not found — skipping Level 3 (rerun Script 03 first)")
} else {

  pixel_freq <- readRDS(pf_path)
  message(sprintf("  pixel_freq_ref: %s rows, %d configs, %d pixels",
                  format(nrow(pixel_freq), big.mark = ","),
                  n_distinct(pixel_freq$config_id),
                  n_distinct(paste(pixel_freq$x, pixel_freq$y))))

  # ---- A. Spatial rank correlation -------------------------------------------
  # Per country: pivot wide (rows = pixels, cols = configs), compute all-pairs
  # Spearman r, summarise as mean r across pairs.

  spatial_rank_corr <- map_dfr(countries, function(ctry) {

    pf_c <- pixel_freq %>% filter(country == ctry)
    if (n_distinct(pf_c$config_id) < 2 || n_distinct(paste(pf_c$x, pf_c$y)) < 5)
      return(data.frame())

    wide <- pf_c %>%
      select(x, y, config_id, ref_n_events) %>%
      tidyr::pivot_wider(names_from = config_id, values_from = ref_n_events,
                         values_fn = sum) %>%
      select(-x, -y)

    mat        <- as.matrix(wide)
    cfg_names  <- colnames(mat)
    pairs      <- combn(cfg_names, 2, simplify = FALSE)

    pair_r <- map_dfr(pairs, function(p) {
      r <- cor(mat[, p[1]], mat[, p[2]],
               method = "spearman", use = "complete.obs")
      data.frame(country   = ctry,
                 config_a  = p[1],
                 config_b  = p[2],
                 spearman_r = round(r, 4),
                 stringsAsFactors = FALSE)
    })

    pair_r
  })

  if (nrow(spatial_rank_corr) > 0) {
    summary_src <- spatial_rank_corr %>%
      group_by(country) %>%
      summarise(mean_spearman_r  = round(mean(spearman_r,  na.rm = TRUE), 3),
                pct_r_above_0.8  = round(100 * mean(spearman_r > 0.8, na.rm = TRUE), 1),
                .groups = "drop")
    cat("\n=== Spatial rank correlation summary ===\n")
    print(summary_src, row.names = FALSE)
    saveRDS(spatial_rank_corr, file.path(VOLUME_PATH, "analysis1_spatial_rank_corr.rds"))
    message("  Saved: analysis1_spatial_rank_corr.rds")
  }

  # ---- B. Hotspot agreement map ----------------------------------------------
  # Hotspot = pixel freq >= 75th percentile within country × config.
  # Agreement = % of configs where pixel is hotspot.

  hotspot_agreement <- pixel_freq %>%
    group_by(country, config_id) %>%
    mutate(hotspot_threshold = quantile(ref_n_events, 0.75, na.rm = TRUE),
           is_hotspot        = ref_n_events >= hotspot_threshold) %>%
    ungroup() %>%
    group_by(country, x, y) %>%
    summarise(
      n_configs       = n(),
      n_hotspot       = sum(is_hotspot, na.rm = TRUE),
      pct_hotspot     = round(100 * n_hotspot / n_configs, 1),
      hotspot_robust  = pct_hotspot >= 80,
      hotspot_uncertain = pct_hotspot > 20 & pct_hotspot < 80,
      .groups = "drop"
    )

  cat("\n=== Hotspot agreement summary ===\n")
  hotspot_agreement %>%
    group_by(country) %>%
    summarise(
      total_pixels    = n(),
      robust_hotspots = sum(hotspot_robust),
      uncertain       = sum(hotspot_uncertain),
      pct_robust      = round(100 * robust_hotspots / total_pixels, 1),
      .groups = "drop"
    ) %>%
    print(row.names = FALSE)

  saveRDS(hotspot_agreement, file.path(VOLUME_PATH, "analysis1_hotspot_agreement.rds"))
  message("  Saved: analysis1_hotspot_agreement.rds")
}
