# =============================================================================
# 00 — Shared config + event rule for the Q3 no-standardization freq comparison.
# Sourced by 01/02/03 (compute) and 10 (figures). No outputs of its own.
# =============================================================================
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(stringr)})

# --- common config -----------------------------------------------------------
Q3 <- list(
  wd    = "c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices",
  out   = "Codes/nostd_freq/out",
  Y0    = 2003,           # Observation window (NDVI starts 2000, so this is the
  Y1    = 2023,           # common ground across all three witnesses)
  MINDUR= 1,              # ONE rule for every witness: a below-threshold run of
                          # >= 1 month = one event (same as the char-deck pipeline,
                          # min_duration = 1). No per-witness exception.
  SCALES= c(1,3,6,12),
  THR   = c(0.0,-0.5,-1.0,-1.5)
)
Q3$NYR <- Q3$Y1 - Q3$Y0 + 1

# --- the ONE event rule, used for every witness ------------------------------
# events = number of runs where `cond` is TRUE for at least `mindur` months.
count_events <- function(cond, mindur=Q3$MINDUR){
  cond[is.na(cond)] <- FALSE
  r <- rle(cond)
  sum(r$values & r$lengths >= mindur)
}
