# =============================================================================
# 00_config.R — shared config for the Q5 reporting-window deck
# =============================================================================
# Q5 question: our bar plots report drought frequency as events per YEAR. If the
# story is told on a 10-year or 30-year basis instead, do SPI and SPEI separate
# into different bins, or do they stay in the same bin?
#
# Rescaling per-year to per-decade multiplies every index by the same constant,
# so it cannot change which index is larger. What changes is RESOLUTION: at one
# decimal place, 1.54 and 1.39 events/yr both read as "about 1.5"; the same two
# rates over 30 years are 46 and 42 events, which no longer collide. This deck
# quantifies where the gap survives rounding, and whether it survives the
# spread across pixels.
#
# SPI and SPEI only (no NDVI / soil moisture — those are Q3 witnesses).
# Single source of truth for paths, thresholds, window and the event rule.
# Sourced by 01 (compute) and 10 (figures).
# =============================================================================

Q5 <- list(
  Y0      = 2003,                 # observation window, matches Q3/Q4 decks
  Y1      = 2023,
  R0      = 1973,                 # reference window, matches v2 scripts 06/07/09
  R1      = 2002,
  THR     = c(-0.5, -1.0, -1.5),  # the three WMO-standard thresholds
  SCALES  = c(1, 3, 6, 12),       # accumulation periods (months)
  MINDUR  = 1,                    # min event length (months)
  WINDOWS = c(1, 10, 30),         # reporting windows compared (years)
  # families kept, mapped to the `index` column of events.parquet
  FAMS    = c("SPI" = "spi", "SPEI-T" = "spei", "SPEI-P" = "spei_pm"),
  NBOOT   = 2000,                 # bootstrap resamples for the pixel-mean CI
  SEED    = 20260818,
  EVENTS  = "Codes/_archive/_data/events.parquet",  # detected events, all thr
  OUT     = "Codes/agg_window/out",
  FIGS    = "Codes/agg_window/out/figs"
)
Q5$NYR <- Q5$Y1 - Q5$Y0 + 1       # 21 years of record
Q5$NRYR <- Q5$R1 - Q5$R0 + 1      # 30 years in the reference window

# -- Labels -------------------------------------------------------------------
thr_lab <- function(t) factor(sprintf("thr %.1f", t),
                              levels = sprintf("thr %.1f", Q5$THR))
sc_lab  <- function(s) factor(sprintf("%d-month", s),
                              levels = sprintf("%d-month", Q5$SCALES))
win_lab <- function(w) factor(c("1" = "per year", "10" = "per decade",
                                "30" = "per 30 years")[as.character(w)],
                              levels = c("per year", "per decade",
                                         "per 30 years"))

# -- Reporting helpers --------------------------------------------------------
# A rate in events/yr, expressed over a reporting window of `w` years.
as_window <- function(rate_per_yr, w) rate_per_yr * w

# What the audience actually reads off a bar plot: the rate rounded to the
# number of decimals the axis shows. `dp = 0` is the whole-event rounding that
# collapses SPI and SPEI at annual scale.
displayed <- function(x, dp = 0) round(x, dp)

# Return period: average months between event onsets. The framing this deck
# recommends, because it is invariant to the reporting window.
return_months <- function(rate_per_yr) 12 / rate_per_yr
