# =============================================================================
# 01_q6_two_country_boxes.R - Benin vs Eritrea, the characteristics boxes
# =============================================================================
# Q6 question: Benin alone suggested that the PET treatment barely matters --
# SPI, SPEI-Thornthwaite and SPEI-Penman landed close together. Does that hold
# in an arid country where temperature dominates the water balance?
#
# Two claims are under test, both read off these figures:
#   A  SPI ~ SPEI        does including PET at all change the picture?
#   B  SPEI-T ~ SPEI-P   does WHICH PET formulation is used change it?
#
# Method: reuse the published Benin figure exactly and add Eritrea as a second
# facet ROW. The metric code is copied from
# Codes/reports/drought_characteristics_OFAT.qmd, so the Benin row must
# reproduce the published figure; any difference between the rows is then the
# country, not the code. Eritrea (166 px) is matched to Benin (154 px) so the
# comparison is not confounded by sample size.
#
# Both countries share a panel, and y is free ACROSS threshold facets: a stricter
# threshold flags fewer events by construction, so a shared axis only squashed the
# strict panels. The comparison the figure exists to make (Benin vs Eritrea,
# SPI vs SPEI) is within a panel, so it is unaffected. Panel heights are NOT
# comparable to each other.
#
# Run from the repo root:
#   Rscript Codes/aridity_test/01_q6_two_country_boxes.R
# =============================================================================
suppressMessages({
  library(arrow); library(dplyr); library(stringr); library(ggplot2)
})

Q6 <- list(
  # WMO-standard drought thresholds only. 0.0 is dropped: it flags roughly half
  # of all months by construction, so its boxes describe the definition rather
  # than the climate.
  THRESHOLDS = c(-0.5, -1.0, -1.5),
  # WMO-standard accumulation periods only. 1-month is dropped: it is close to
  # raw monthly rainfall, carries no accumulation memory, and is not part of the
  # WMO SPI handbook's recommended set.
  SCALES     = c(3, 6, 12),
  REF0 = 1973, REF1 = 2002,                # reference window, as in the deck
  OBS0 = 2003, OBS1 = 2023,                # observation window
  EVENTS = c(Benin   = "Codes/_archive/_data/events.parquet",
             Eritrea = "Codes/_archive/_data/events_eritrea.parquet"),
  FIGS   = "Codes/aridity_test/out/figs"
)
Q6$NREF <- Q6$REF1 - Q6$REF0 + 1
Q6$NOBS <- Q6$OBS1 - Q6$OBS0 + 1

thr_labels <- c("Threshold -0.5", "Threshold -1.0", "Threshold -1.5")

# Index colour (hue) + period shade (light = reference, dark = observation).
# Benin keeps the published deck palette so the two decks read together.
# Eritrea repeats the same six entries desaturated toward grey, so within a
# panel the eye groups by index first and separates country by saturation.
make_pal <- function() c(
  "SPI · Reference"    = "#f4a6a0", "SPI · Observation"    = "#c0392b",
  "SPEI-T · Reference" = "#a3d9a5", "SPEI-T · Observation" = "#2e8b3d",
  "SPEI-P · Reference" = "#a8c8ec", "SPEI-P · Observation" = "#2c6fb5"
)

# Pull each Benin colour toward neutral grey. amt = how far (0 none, 1 all grey).
desat <- function(hex, amt = 0.55) {
  m <- grDevices::col2rgb(hex) / 255
  g <- 0.299 * m[1, ] + 0.587 * m[2, ] + 0.114 * m[3, ]   # luma
  mix <- (1 - amt) * m + amt * rbind(g, g, g)
  grDevices::rgb(mix[1, ], mix[2, ], mix[3, ])
}

# Fill key is family + period + country, so one scale drives all 12 boxes.
make_pal2 <- function(countries) {
  base <- make_pal()
  out <- c()
  for (i in seq_along(countries)) {
    v <- if (i == 1) base else desat(base)
    names(v) <- paste0(names(base), " · ", countries[i])
    out <- c(out, v)
  }
  out
}

# -- Load ---------------------------------------------------------------------
# config_id is NOT portable across countries (the two runs use different
# prefixes), so family and scale are derived from `index`, which is stable.
load_ev <- function(path, cty) {
  as.data.frame(read_parquet(path)) |>
    filter(method == "threshold", start_thr %in% Q6$THRESHOLDS) |>
    mutate(
      country = cty,
      scale   = as.integer(str_extract(index, "[0-9]+")),
      family  = case_when(str_detect(index, "_pm$")  ~ "SPEI-P",
                          str_detect(index, "^spei") ~ "SPEI-T",
                          str_detect(index, "^spi")  ~ "SPI", TRUE ~ index),
      family  = factor(family, levels = c("SPI", "SPEI-T", "SPEI-P"))
    )
}

ev <- bind_rows(lapply(names(Q6$EVENTS),
                       function(k) load_ev(Q6$EVENTS[[k]], k)))
ev$country <- factor(ev$country, levels = names(Q6$EVENTS))

npx <- ev |> distinct(country, x, y) |> count(country, name = "pixels")
px_lab <- paste(sprintf("%s %d px", npx$country, npx$pixels), collapse = ", ")
print(npx)

# -- Metric -------------------------------------------------------------------
# Per-pixel value of one box characteristic, for one scale, both periods.
# This is the deck's pixel_metric() with `country` added to the group-by.
pixel_metric <- function(metric, scale_val) {
  sub <- ev |> filter(scale == scale_val)
  one <- function(y0, y1, ny, plabel) {
    g <- sub |> filter(event_year >= y0, event_year <= y1) |>
      group_by(country, family, start_thr, x, y)
    out <- switch(metric,
      frequency = summarise(g, value = n() / ny, .groups = "drop"),
      duration  = summarise(g, value = mean(duration_months, na.rm = TRUE),
                            .groups = "drop"),
      # abs() -> positive magnitude, so "higher" reads as "more severe"
      severity  = summarise(g, value = mean(abs(peak_index), na.rm = TRUE),
                            .groups = "drop")
    )
    out$period <- plabel
    out
  }
  bind_rows(one(Q6$REF0, Q6$REF1, Q6$NREF, "Reference"),
            one(Q6$OBS0, Q6$OBS1, Q6$NOBS, "Observation"))
}

add_aes <- function(d, scale_val) {
  ctys <- levels(d$country)
  d |> mutate(
    period  = factor(period, levels = c("Reference", "Observation")),
    thr_f   = factor(start_thr, levels = Q6$THRESHOLDS, labels = thr_labels),
    # x position is family x country, so the two countries sit side by side
    # under one index; period stays the dodge within each position.
    fam_lab = factor(
      paste0(family, "-", scale_val, "
", country),
      levels = as.vector(t(outer(
        paste0(c("SPI", "SPEI-T", "SPEI-P"), "-", scale_val),
        ctys, function(a, b) paste0(a, "
", b))))
    ),
    grp = factor(paste0(family, " · ", period, " · ", country),
                 levels = names(make_pal2(ctys)))
  )
}

# -- Figure -------------------------------------------------------------------
box_fig <- function(d, ylab, title) {
  pal <- make_pal2(levels(d$country))
  # Grey band behind every Eritrea x-position, so the country a box belongs to
  # is readable without decoding the fill saturation. One rect per Eritrea
  # level, half a slot either side; drawn first so it sits under the boxes.
  eri <- which(str_detect(levels(d$fam_lab), "\nEritrea$"))
  band <- data.frame(xmin = eri - 0.5, xmax = eri + 0.5)
  # A few arid-country pixels saturate. At 12-month SPEI-T, two adjacent cells
  # in northern Eritrea sit below the threshold for almost the whole
  # observation window, giving pixel means near 190 months. Left alone they
  # stretch that panel by an order of magnitude and flatten every box in it.
  #
  # Each threshold panel is capped at its own 99th percentile (rounded up to a
  # multiple of 5), and only where the cap buys something: if the panel maximum
  # is already within 1.5x of the cap, the panel is left alone. Values above
  # the cap are pulled down to it for DRAWING only, so the axis cannot be
  # stretched by a saturating pixel. With ~500 pixels per box and a handful
  # affected, the median and quartiles are unchanged; the upper whisker of an
  # affected box is the one statistic the cap can move. The subtitle records
  # how many points were pulled in.
  cap_of <- function(v) {
    hi <- ceiling(stats::quantile(v, 0.995, na.rm = TRUE) / 5) * 5
    # Only intervene on a genuine runaway: the panel maximum must be several
    # times the bulk of the distribution. A merely long tail is left intact.
    if (max(v, na.rm = TRUE) < hi * 3) Inf else hi
  }
  caps <- d |>
    group_by(thr_f) |>
    summarise(cap = cap_of(value), .groups = "drop")
  d <- d |> left_join(caps, by = "thr_f")
  n_off <- sum(d$value > d$cap, na.rm = TRUE)
  off_lab <- if (n_off > 0)
    paste0(" y capped at the 99th pct in ",
           sum(is.finite(caps$cap)), " panel(s); ", n_off,
           " point(s) drawn at the cap.") else ""
  d$value_plot <- pmin(d$value, d$cap)
  ggplot(d, aes(x = fam_lab, y = value_plot, fill = grp)) +
    geom_rect(data = band, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "grey88", alpha = 0.55) +
    geom_boxplot(outlier.size = 0.4, width = 0.7, colour = "grey20",
                 linewidth = 0.4, position = position_dodge(width = 0.8)) +
    # Both countries now share a panel; thresholds stay as the facet columns.
    # y is FREE across thresholds: a stricter threshold flags fewer events by
    # construction, so a shared axis only squashed the strict panels. The
    # comparison this figure exists to make (Benin vs Eritrea, SPI vs SPEI)
    # is within a panel, so it is unaffected. Heights are NOT comparable
    # between panels.
    facet_wrap(~ thr_f, nrow = 1, scales = "free_y") +
    scale_fill_manual(values = pal, name = NULL, breaks = names(pal)) +
    labs(title = title,
         subtitle = paste0("Light = Reference (", Q6$REF0, "-", Q6$REF1,
                           "), Dark = Observation (", Q6$OBS0, "-", Q6$OBS1,
                           "). Grey band + muted colour = Eritrea. Box = across pixels. ",
                           px_lab, ".", off_lab),
         x = NULL, y = ylab) +
    theme_bw(base_size = 10) +
    theme(legend.position = "top", legend.text = element_text(size = 7),
          legend.key.size = unit(0.8, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
          panel.border = element_rect(colour = "grey30", fill = NA,
                                      linewidth = 0.6),
          panel.spacing = unit(0.6, "lines"),
          strip.background = element_rect(colour = "grey30", fill = "grey92"),
          strip.text = element_text(face = "bold")) +
    guides(fill = guide_legend(nrow = 3))
}

METRICS <- list(
  frequency = list(ylab = "events per pixel per year", tag = "freq",
                   title = "Drought frequency"),
  duration  = list(ylab = "mean event duration (months)", tag = "dur",
                   title = "Drought duration"),
  severity  = list(ylab = "mean |peak index|", tag = "sev",
                   title = "Drought peak severity")
)

dir.create(Q6$FIGS, recursive = TRUE, showWarnings = FALSE)

for (m in names(METRICS)) {
  spec <- METRICS[[m]]
  for (s in Q6$SCALES) {
    d <- add_aes(pixel_metric(m, s), s)
    p <- box_fig(d, spec$ylab,
                 sprintf("%s, %d-month accumulation - Benin vs Eritrea",
                         spec$title, s))
    f <- sprintf("%s/Q6_%s_%02dmo.png", Q6$FIGS, spec$tag, s)
    ggsave(f, p, width = 14, height = 7, dpi = 140)
    cat("wrote", f, "\n")
  }
}
