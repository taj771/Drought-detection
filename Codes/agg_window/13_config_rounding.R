# =============================================================================
# 13_config_rounding.R — Q5 second question: how many configs share a bin?
# =============================================================================
# The meeting's second question. The first question asked whether SPI and SPEI
# collide after rounding. This one asks the same thing across the WHOLE config
# space: we run 75 configurations, each producing a frequency. Round each one
# to a whole number and count how many distinct values survive.
#
# If 60 configs collapse onto 2 or 3 displayed numbers, then the config choice
# -- index family, accumulation period, threshold -- is invisible in the
# reported figure, and two teams using different configs would quote the same
# number while their underlying rates differ substantially.
#
# CONFIG COUNT: events.parquet holds 75 config_ids, but 15 use method
# "cumulative" and the remaining 60 use "threshold". Q5 is a threshold-rule
# deck throughout (Q3/Q4 event rule), so the 60 threshold configs are used and
# the count is stated honestly in the figure rather than rounded up to 75.
# The 60 = 5 thresholds x 12 index-scale combinations (SPI/SPEI-T/SPEI-P x
# 1/3/6/12 month). All 5 thresholds are kept here, not just the 3 WMO ones,
# because the question is about the config space as a whole.
#
# Builds:
#   C1_config_histogram.png   — distribution of the 60 rounded values, per window
#   C2_config_collapse.png    — distinct displayed values vs window, by threshold
#   C5a/b/c_bins_*.png        — bin membership trees, observation window
#   C5a/b/c_bins_*_ref.png    — the same trees over the reference window
# =============================================================================

source("Codes/agg_window/00_config.R")
suppressMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(patchwork)
})

ALL_THR <- c(0.0, -0.25, -0.5, -1.0, -1.5)   # the full threshold set, not just WMO

# -- Country-mean frequency for every config ----------------------------------
# One number per config, the quantity a report would actually quote: the mean
# per-pixel event rate over the 154 pixels. Pixels with no events are true
# zeros and must be included, so the full grid is rejoined as in script 01.
# Read once, tag the period, so the observation and reference windows come from
# one pass over the file and are guaranteed to use the same event rule.
ev_all <- as.data.frame(read_parquet(Q5$EVENTS)) |>
  filter(method == "threshold",
         start_thr %in% ALL_THR,
         min_duration == Q5$MINDUR) |>
  mutate(scale = as.integer(str_extract(index, "\\d+")),
         fam   = case_when(str_starts(index, "spi")  ~ "SPI",
                           str_ends(index,   "_pm")  ~ "SPEI-P",
                           str_starts(index, "spei") ~ "SPEI-T"))

# Events inside a year window. The pixel grid is taken from the FULL table, not
# from the window's own events, so a pixel that happens to be event-free inside
# one window still counts as a true zero in that window's mean rate. Deriving
# the grid per window would silently drop those pixels and inflate the rate.
grid <- expand_grid(distinct(ev_all, x, y),
                    distinct(ev_all, fam, scale, start_thr))

.ev_window <- function(y0, y1) filter(ev_all, event_year >= y0, event_year <= y1)

.cfg_rate <- function(ev, nyr) {
  ev |>
    count(x, y, fam, scale, start_thr, name = "events") |>
    right_join(grid, by = c("x", "y", "fam", "scale", "start_thr")) |>
    mutate(events = coalesce(events, 0L), rate = events / nyr) |>
    group_by(fam, scale, start_thr) |>
    summarise(rate = mean(rate), .groups = "drop")
}

ev  <- .ev_window(Q5$Y0, Q5$Y1)
cfg <- .cfg_rate(ev, Q5$NYR)

N_CFG <- nrow(cfg)
cat("configs summarised:", N_CFG, "\n")

# -- The other two headline characteristics, per config ------------------------
# Duration and peak severity are properties OF an event, not rates, so they are
# averaged over the events a config produces rather than over the pixel grid --
# a pixel with no events contributes no duration, and coercing it to zero would
# invent a 0-month drought. Zero-event pixels therefore drop out here, which is
# why these use the event table directly instead of the rejoined grid.
.cfg_char <- function(ev) {
  ev |>
    group_by(fam, scale, start_thr) |>
    summarise(dur  = mean(duration_months),
              peak = mean(peak_index),
              .groups = "drop")
}

char <- .cfg_char(ev)

# -- The displayed value at each reporting window ------------------------------
cfg_win <- cfg |>
  cross_join(tibble(window = Q5$WINDOWS)) |>
  mutate(value = as_window(rate, window),
         shown = round(value),
         win_f = win_lab(window),
         thr_f = factor(sprintf("thr %.2f", start_thr),
                        levels = sprintf("thr %.2f", ALL_THR)))

# -- How much of the config space collapses? -----------------------------------
# n_distinct(shown) is the number of different numbers a reader could ever see.
# The ratio to N_CFG is the share of the config space that is distinguishable.
collapse <- cfg_win |>
  group_by(win_f, window) |>
  summarise(n_shown  = n_distinct(shown),
            n_cfg    = n(),
            biggest  = max(table(shown)),
            .groups  = "drop") |>
  mutate(lab = sprintf("%d configs land on %d distinct values\nlargest single bin holds %d",
                       n_cfg, n_shown, biggest))

print(as.data.frame(collapse))

# -- C1: how many configs share each displayed value, per characteristic -------
# One panel per headline number a report quotes. All three share the y axis --
# "number of configs (of 60)" -- because all three ask the same question of the
# same 60 configs. Only the x axis differs, so each panel labels its own.
#
# Each characteristic is rounded the way it is actually written down:
#   frequency  whole events per year
#   duration   whole months
#   severity   one decimal, which is how an index value is normally quoted
# Severity at whole numbers would be a rigged comparison -- the whole config
# space spans only -2.16 to -1.00, so rounding to integers forces 60 configs
# onto 2 values by construction. That case is reported in the console instead.
.hist_metric <- function(vals, x_lab, digits, show_y) {
  dd <- tibble(shown = round(vals, digits))
  n_shown <- n_distinct(dd$shown)
  cc <- tibble(lab = sprintf("%d configs land on %d distinct values\nlargest single bin holds %d",
                             length(vals), n_shown, max(table(dd$shown))))
  ggplot(dd, aes(x = factor(shown))) +
    geom_bar(fill = "#2c6fb5", colour = "grey20", linewidth = 0.3,
             width = 0.85) +
    geom_text(stat = "count", aes(label = after_stat(count)),
              vjust = -0.4, size = 3.0, colour = "grey15", fontface = "bold") +
    geom_text(data = cc, inherit.aes = FALSE,
              aes(x = Inf, y = Inf, label = lab), hjust = 1.04, vjust = 1.2,
              size = 2.5, colour = "#d95f02", fontface = "bold") +
    scale_y_continuous(limits = c(0, 40),
                       expand = expansion(mult = c(0, 0.20))) +
    labs(x = x_lab,
         y = if (show_y) sprintf("number of configs (of %d)", N_CFG) else NULL) +
    theme_bw(base_size = 11) +
    theme(panel.border = element_rect(colour = "grey30", fill = NA,
                                      linewidth = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.x = element_text(size = 9),
          axis.text.x = element_text(size = 8))
}

# Annual window only. The decade and 30-year panels showed the same 60 configs
# spreading out again, which is the Q1 rescaling point already made; the deck
# needs only the annual collapse itself, drawn large enough to read.
freq_yr <- filter(cfg_win, window == 1)$value

p1 <- (.hist_metric(freq_yr,   "drought events per year",       0, TRUE) |
       .hist_metric(char$dur,  "mean duration (months)",        0, FALSE) |
       .hist_metric(char$peak, "mean peak severity (index)",    1, FALSE))
# No caption inside the figure: the deck slide carries the same sentence just
# below it, and the space is better spent on bar height.

ggsave(file.path(Q5$FIGS, "C1_config_histogram.png"), p1,
       width = 10.0, height = 3.6, dpi = 200)

# -- C3: which configs sit inside each bar -------------------------------------
# The histogram says "35 configs land on 1 event per year" but not WHICH 35. This
# pairs the same annual bars with a membership matrix underneath, sharing the x
# axis, so a column of dots and the bar above it are the same 60 configs seen two
# ways: the bar gives the count, the matrix gives the identity.
#
# Rows are the 12 index-scale combinations, ordered scale-within-family. Colour
# carries the threshold, and each threshold keeps a FIXED lane inside its bin
# column (a deterministic dodge, not jitter) so one threshold can be tracked
# horizontally across the whole matrix. Empty cells are left blank -- the
# blankness is the collapse.
mat <- cfg_win |>
  filter(window == 1) |>
  mutate(row_f = factor(sprintf("%s-%d", fam, scale),
                        levels = rev(as.vector(outer(Q5$SCALES,
                                                     c("SPI", "SPEI-T", "SPEI-P"),
                                                     function(s, f) sprintf("%s-%d", f, s))))),
         thr_c = factor(sprintf("%.2f", start_thr),
                        levels = sprintf("%.2f", ALL_THR)),
         bin_f = factor(shown, levels = sort(unique(shown))),
         # fixed lane per threshold: 5 evenly spaced offsets inside the column
         lane  = as.integer(thr_c),
         xpos  = as.integer(bin_f) + (lane - (length(ALL_THR) + 1) / 2) * 0.14)

THR_COL <- setNames(c("#c7e0f4", "#7fb3dd", "#2c6fb5", "#1a4c85", "#0b2a4d"),
                    sprintf("%.2f", ALL_THR))
BINS    <- levels(mat$bin_f)
XLIM    <- c(0.4, length(BINS) + 0.6)

# One label per bar. geom_text(stat = "count") would fire once per stacked
# threshold segment and print five numbers on top of each other, so the totals
# are precomputed and drawn as their own layer.
bin_tot <- count(mat, bin_f, name = "n") |>
  mutate(xb = as.integer(bin_f))

# Bars: same counts as the C1 frequency panel, but stacked by threshold so the
# bar segments and the dot colours read off one legend.
p_bar <- ggplot(mat, aes(x = as.integer(bin_f), fill = thr_c)) +
  geom_bar(colour = "grey20", linewidth = 0.25, width = 0.85) +
  geom_text(data = bin_tot, inherit.aes = FALSE,
            aes(x = xb, y = n, label = n), vjust = -0.5,
            size = 3.2, colour = "grey15", fontface = "bold") +
  scale_fill_manual(values = THR_COL, name = "threshold") +
  scale_x_continuous(limits = XLIM, expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(y = sprintf("configs (of %d)", N_CFG)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(4, 4, 0, 4))

# Faint separators between the three index families, drawn between rows 4/5 and
# 8/9 of the reversed row order.
sep_y <- c(4.5, 8.5)

p_mat <- ggplot(mat, aes(x = xpos, y = row_f, colour = thr_c)) +
  geom_hline(yintercept = sep_y, colour = "grey75", linewidth = 0.4) +
  geom_vline(xintercept = seq_len(length(BINS) - 1) + 0.5,
             colour = "grey88", linewidth = 0.3) +
  geom_point(size = 2.1) +
  scale_colour_manual(values = THR_COL, name = "threshold") +
  scale_x_continuous(limits = XLIM, expand = c(0, 0),
                     breaks = seq_along(BINS), labels = BINS) +
  labs(x = "drought events per year, as displayed") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 3))) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.3),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        plot.margin = margin(0, 4, 4, 4))

p3 <- p_bar / p_mat + plot_layout(heights = c(1, 2.4))

ggsave(file.path(Q5$FIGS, "C3_config_matrix.png"), p3,
       width = 9.0, height = 5.6, dpi = 200)

# -- C4: the same membership, blocked by threshold -----------------------------
# C3 packs all five thresholds into one grid by giving each a colour lane, which
# is compact but asks the reader to hold five colours at once. C4 unpacks the
# same data into one horizontal block per threshold: rows are still the 12
# index-scale combinations, but a config's threshold is now its position rather
# than its colour, so no legend is needed to tell where a dot belongs.
#
# Restricted to the three WMO thresholds. All five would be 60 rows tall and the
# 0.00 / -0.25 blocks are not thresholds anyone reports against; they stay in C3
# and in the console tables, where the full config space is the point.
mat2 <- filter(mat, start_thr %in% Q5$THR) |>
  mutate(blk = factor(sprintf("threshold %.1f", start_thr),
                      levels = sprintf("threshold %.1f", Q5$THR)))

# Per-block bar counts, so each block carries its own totals rather than sharing
# one bar row -- the collapse tightens as the threshold tightens, and that is
# only visible if each block is counted separately.
tot2 <- count(mat2, blk, bin_f, name = "n") |> mutate(xb = as.integer(bin_f))

p4 <- ggplot(mat2, aes(x = as.integer(bin_f), y = row_f)) +
  geom_vline(xintercept = seq_len(length(BINS) - 1) + 0.5,
             colour = "grey88", linewidth = 0.3) +
  geom_hline(yintercept = sep_y, colour = "grey75", linewidth = 0.4) +
  geom_point(colour = "#2c6fb5", size = 2.4) +
  geom_text(data = tot2, inherit.aes = FALSE,
            aes(x = xb, y = 13.1, label = n),
            size = 3.0, colour = "#d95f02", fontface = "bold") +
  facet_wrap(~ blk, ncol = 3) +
  scale_x_continuous(limits = XLIM, expand = c(0, 0),
                     breaks = seq_along(BINS), labels = BINS) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.4))) +
  labs(x = "drought events per year, as displayed",
       caption = "orange = configs sharing that displayed value, within the block") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.3),
        strip.background = element_rect(fill = "grey92", colour = "grey40"),
        strip.text = element_text(face = "bold", size = 9.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        plot.caption = element_text(size = 8, colour = "grey30", hjust = 0))

ggsave(file.path(Q5$FIGS, "C4_config_matrix_blocked.png"), p4,
       width = 10.0, height = 4.6, dpi = 200)

# -- C5: which configs sit inside each rounded bin, as a tree ------------------
# C1 says "9 configs round to 0 events per year" but not which 9. C3 answers
# that with a dot matrix. C5 answers it as a tree instead: the rounded value is
# the root, the index families branch off it, and the individual configs hang
# underneath. Beside each bin a stacked bar gives the family composition, so the
# bin's size and its make-up are readable in one pass.
#
# The question is whether a bin is family-homogeneous. If bin 0 were all SPEI-P
# the collapse would be a property of one index; if every bin mixes all three
# families, then no index family escapes the rounding, which is the stronger
# claim and the one the deck makes.
#
# One figure per characteristic, each rounded the way it is written down:
#   frequency  whole events per year   duration  whole months
#   severity   one decimal, as an index value is normally quoted
# Severity keeps its decimal for the reason given at C1: rounding an index that
# spans only -2.16 to -1.00 to integers would force the collapse by construction.

FAM_LV  <- c("SPI", "SPEI-T", "SPEI-P")
FAM_COL <- setNames(c("#2c6fb5", "#1b9e8a", "#bd6bb0"), FAM_LV)

# Configs are folded to family nodes rather than drawn as 60 leaves: bin 1 holds
# 35 configs at annual frequency, and 35 leaves in one tree is unreadable. Each
# family node then prints its own configs as a leaf label where they fit. Above
# LEAF_MAX the label folds to a count -- stated on the node, so a folded branch
# is never mistaken for a small one. The cap is per FAMILY node, not per bin:
# frequency's bin 1 holds 35 configs but splits into three family nodes, so a cap
# of 15 lets every family print its configs in full and no branch is stunted
# purely because its bin is the crowded one.
LEAF_MAX <- 15

# Character width a family node's config list is wrapped to. The list is printed
# on one row until it exceeds this, so a family owning several scales costs one
# or two rows rather than one row per scale -- the figure has to fit the height
# of a slide, and per-scale rows made it far too tall to read there.
WRAP_CH <- 60

# Blank space added to each family row, in text-line units. The row is as tall as
# the text it prints plus this much air; at 0 the labels of adjacent branches
# would touch, so it buys separation only, not layout. Kept small because the
# gaps between branches are what made the figure too tall for a slide.
PAD_ROW <- 0

# Height of a composition bar, in the same y units the tree lays branches out
# on. Shared by both panels: the tree's root segment leaves from the bar's TOP
# EDGE, which is half this width above the bin's centre line, so the two panels
# have to agree on the number.
BAR_W <- 0.62

# The three characteristics on a common shape: one row per config, carrying the
# value, the family, and the full config label. Frequency is the annual window
# (Q5's reporting window); duration and severity are per-config over thresholds.
.c5_data <- function(cfg_tab, char_tab) {
  bind_rows(
    cfg_tab |>
      transmute(metric = "events per year", fam, scale, start_thr,
                value = rate),
    char_tab |> transmute(metric = "mean duration (months)", fam, scale,
                          start_thr, value = dur),
    char_tab |> transmute(metric = "mean peak severity (|index|)", fam, scale,
                          start_thr, value = abs(peak))
  ) |>
    mutate(metric = factor(metric, levels = c("events per year",
                                              "mean duration (months)",
                                              "mean peak severity (|index|)")),
           digits = ifelse(metric == "mean peak severity (|index|)", 1, 0),
           shown  = round(value, digits),
           fam_f  = factor(fam, levels = FAM_LV),
           cfg    = sprintf("%s-%d @ %.2f", fam, scale, start_thr))
}

c5_dat <- .c5_data(cfg, char)

# One tree-plus-bar figure for a single characteristic. Built as its own function
# so the three figures are the same object with different inputs, and any layout
# fix lands on all three at once.
.bin_layout <- function(dd) {
  fmt <- if (dd$digits[1] == 1) "%.1f" else "%d"

  # Family nodes: one per (bin, family) that actually occurs. Empty families are
  # dropped rather than drawn as zero-length branches -- an absent family in a
  # bin is information, and a stub line would read as a present-but-tiny one.
  # One printed line per index-and-scale, not per config: configs that differ only
  # by threshold land in the same bin together, so listing them on separate lines
  # repeats "SPI-3" three times and triples the node height for no information.
  # The thresholds are joined onto the one line instead -- "SPI-3 @ -0.50, -1.00".
  lines <- dd |>
    group_by(shown, fam_f, scale) |>
    summarise(n = n(),
              line = sprintf("%s-%d @ %s", fam_f[1], scale[1],
                             paste(sprintf("%.2f", sort(start_thr)),
                                   collapse = ", ")),
              .groups = "drop")

  # The scale lines are then joined onto ONE row separated by " | " and wrapped
  # only when they exceed WRAP_CH characters. A family that owns four scales in a
  # bin therefore prints as one or two rows instead of four, which is what keeps
  # the figure short enough to read on a slide.
  nodes <- lines |>
    group_by(shown, fam_f) |>
    summarise(n = sum(n),
              cfgs = paste(strwrap(paste(line[order(scale)], collapse = " | "),
                                   width = WRAP_CH),
                           collapse = "\n"),
              .groups = "drop") |>
    mutate(n_lines = lengths(regmatches(cfgs, gregexpr("\n", cfgs))) + 1) |>
    arrange(shown, fam_f) |>
    mutate(leaf = ifelse(n_lines <= LEAF_MAX, cfgs, sprintf("%d configs", n)))

  # Vertical layout. Nodes are stacked top to bottom in bin order; a bin's root
  # sits at the mean y of its own family nodes, so the root line points at the
  # centre of the group it owns. Row height is the number of TEXT LINES the node
  # actually prints -- not its config count -- so a folded "11 configs" node
  # takes one line and does not reserve the space of eleven.
  nodes <- nodes |>
    mutate(h = ifelse(n_lines <= LEAF_MAX, n_lines, 2) + PAD_ROW) |>
    mutate(ytop = cumsum(h) - h, y = -(ytop + h / 2))

  roots <- nodes |>
    group_by(shown) |>
    summarise(y = mean(y), n = sum(n), .groups = "drop") |>
    mutate(lab = sprintf(paste0(fmt, "  (n=%d)"), shown, n))

  list(nodes = nodes, roots = roots, fmt = fmt,
       ylim = c(min(-(nodes$ytop + nodes$h)) - 0.4, 0.4))
}

# Tree panel. Root x = 0, family node x = 1, elbowed so the tree reads as a tree
# rather than as a fan of diagonals. The composition bar sits to the LEFT of this
# panel, so the root label is the join between the two: the bar ends, the root
# names the bin, and the branches open rightward into the family nodes.
.bin_tree <- function(lay, x_lab) {
  nodes <- lay$nodes; roots <- lay$roots

  # Root segments sit on the bin's centre line. The stub that carries each bar
  # out to the panel edge is drawn in the BAR panel, not here -- only the bar
  # panel knows where an individual bar's tip is, and a stub drawn here would
  # start at a fixed x and leave a gap in front of every short bar.
  ed <- nodes |> left_join(select(roots, shown, ry = y), by = "shown")
  seg_h1 <- data.frame(x = 0.0, xend = 0.5, y = ed$ry, yend = ed$ry)
  seg_v  <- data.frame(x = 0.5, xend = 0.5, y = ed$ry, yend = ed$y)
  seg_h2 <- data.frame(x = 0.5, xend = 0.95, y = ed$y, yend = ed$y)

  ggplot() +
    geom_segment(data = rbind(seg_h1, seg_v, seg_h2),
                 aes(x = x, xend = xend, y = y, yend = yend),
                 linewidth = 0.45, colour = "grey45", lineend = "round") +
    # No root label here: the bin value is now the bar panel's y axis, so
    # repeating it at the branch join would print the same number twice. The
    # root segment simply runs off the left edge and meets the bar.
    # family node, coloured to match the composition bar
    geom_point(data = nodes, aes(x = 0.95, y = y, colour = fam_f), size = 2.6,
               show.legend = TRUE) +
    geom_text(data = nodes, aes(x = 1.03, y = y, label = leaf, colour = fam_f),
              hjust = 0, size = 2.35, lineheight = 0.95) +
    scale_colour_manual(values = FAM_COL, name = NULL, drop = FALSE) +
    # Left limit is exactly 0, where the root segments start: any padding here
    # opens a gap between the bar panel's stub and this panel's first line, and
    # the two read as disconnected.
    scale_x_continuous(limits = c(0, 2.9), expand = c(0, 0)) +
    scale_y_continuous(limits = lay$ylim, expand = c(0, 0)) +
    labs(title = x_lab) +
    guides(colour = guide_legend(override.aes = list(size = 3, label = ""))) +
    theme_void(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 9.5, face = "bold", hjust = 0,
                                    margin = margin(b = 5)),
          plot.margin = margin(4, 2, 4, 0))
}

# Composition bar for one characteristic: bar length is the config count in the
# bin, segments are the family split. Same bin order and same y positions as the
# tree, so a bar and its branch sit on the same line.
#
# The bar panel sits at the LEFT end of the figure and grows the ordinary way,
# left to right from zero. Its right-hand TIP is therefore the bar's total, and
# that tip is what the tree's root segment leaves from: the bar ends, the branch
# continues from where it ended, and the two panels read as one line.
.bin_bar <- function(dd, lay) {
  # Bars sit at the SAME continuous y as the tree roots, not on a discrete axis
  # of their own -- a discrete axis spaces bins evenly and would float each bar
  # away from the branch it describes.
  bars <- dd |>
    count(shown, fam_f, name = "n") |>
    left_join(select(lay$roots, shown, y), by = "shown") |>
    # position_stack draws the LAST factor level at the LOW-x end. With a normal
    # x axis low x is the left, so reversing the family levels here puts SPI at
    # the left of every bar -- families then run left to right in the same order
    # the legend lists them and the tree stacks them.
    mutate(fam_r = factor(fam_f, levels = rev(FAM_LV)))

  # Segment midpoints for the in-bar counts. Computed here rather than left to
  # position_stack(vjust) so the label sits at the true centre of its own
  # segment on the continuous y, independent of the bar's stacking order.
  # Percent is of ALL configs in the characteristic, not of the bin: the point
  # of the figure is how much of the config space each rounded value swallows,
  # so a within-bin share would answer a question nobody asked.
  n_all <- nrow(dd)
  labs_seg <- bars |>
    arrange(shown, desc(as.integer(fam_r))) |>
    group_by(shown) |>
    mutate(xmid = cumsum(n) - n / 2) |>
    ungroup()

  # Connector stub: each bar's tip carried out to the right edge of the panel,
  # where the tree's root segment picks it up. Drawn here because only this
  # panel knows an individual bar's total; the tree would have to guess.
  tips <- bars |>
    group_by(shown, y) |>
    summarise(n = sum(n), .groups = "drop") |>
    mutate(lab = sprintf("%d  (%.0f%%)", n, 100 * n / n_all))
  x_max <- max(tips$n) * 1.03

  ggplot(bars, aes(x = n, y = y, fill = fam_r)) +
    geom_segment(data = tips, inherit.aes = FALSE,
                 aes(x = n, xend = x_max, y = y, yend = y),
                 linewidth = 0.45, colour = "grey45", lineend = "round") +
    geom_col(width = BAR_W, colour = "white", linewidth = 0.35,
             orientation = "y") +
    # Per-family count inside its own segment -- the split is not recoverable
    # from the axis, so each family states its own n. Kept to the bare count:
    # a percent on a two-config segment needs two lines and does not fit.
    geom_text(data = labs_seg, inherit.aes = FALSE,
              aes(x = xmid, y = y, label = n),
              size = 2.4, colour = "white", fontface = "bold") +
    # Bin total and its share of the whole config space, sitting above the bar
    # where there is room for both numbers regardless of how short the bar is.
    geom_text(data = tips, inherit.aes = FALSE,
              aes(x = 0, y = y + BAR_W * 0.62, label = lab),
              hjust = 0, vjust = 0, size = 2.5, colour = "grey30",
              fontface = "bold") +
    scale_fill_manual(values = FAM_COL, name = NULL, drop = FALSE) +
    # y axis carries the rounded value itself -- the tree no longer prints it at
    # the branch join, so this is the single place the bin is named. Breaks are
    # the bins that exist, not a regular sequence: the bins are irregular and an
    # even sequence would imply values that no config lands on.
    scale_y_continuous(limits = lay$ylim, expand = c(0, 0),
                       breaks = lay$roots$y,
                       labels = sprintf(lay$fmt, lay$roots$shown)) +
    # Right limit is exactly where the stubs end, with zero expansion, so the
    # stubs run right up to the panel border and meet the tree with no gap.
    scale_x_continuous(limits = c(0, x_max), expand = c(0, 0),
                       breaks = scales::breaks_pretty(4)) +
    labs(x = "number of configs", y = "rounded value") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
          # Drawn axes, not just labels: the bar panel is a real measured chart
          # and the axis lines say so.
          axis.line = element_line(colour = "grey45", linewidth = 0.4),
          axis.ticks = element_line(colour = "grey45", linewidth = 0.4),
          axis.ticks.length = unit(2.5, "pt"),
          axis.text = element_text(size = 8, colour = "grey30"),
          axis.title = element_text(size = 8.5, colour = "grey35"),
          # No right margin: the bar tip must meet the tree panel's root
          # segment, and any gap breaks the line the two panels share.
          plot.margin = margin(4, 0, 4, 6))
}

# Three separate files rather than one combined figure: each characteristic has
# a different number of bins and a different label length, so a shared canvas
# would size to the worst case and leave the other two sparse. One per slide.
C5_OUT <- c("events per year"              = "C5a_bins_frequency",
            "mean duration (months)"       = "C5b_bins_duration",
            "mean peak severity (|index|)" = "C5c_bins_severity")

# Both reporting periods, same figures. The observation window keeps the plain
# filenames the deck already points at; the reference window is written beside
# it with a "_ref" suffix, so the pair can be shown as a before/after without
# either file moving. The rates are per YEAR in both, so a 30-year reference
# window and a 21-year observation window are directly comparable -- only the
# number of years feeding each mean differs, not the units.
C5_PERIODS <- list(
  list(tag = "",     lab = sprintf("observation %d-%d", Q5$Y0, Q5$Y1),
       y0 = Q5$Y0, y1 = Q5$Y1, nyr = Q5$NYR),
  list(tag = "_ref", lab = sprintf("reference %d-%d",   Q5$R0, Q5$R1),
       y0 = Q5$R0, y1 = Q5$R1, nyr = Q5$NRYR)
)

for (pr in C5_PERIODS) {
  ev_p  <- .ev_window(pr$y0, pr$y1)
  dat_p <- .c5_data(.cfg_rate(ev_p, pr$nyr), .cfg_char(ev_p))

  for (mt in levels(dat_p$metric)) {
    dd  <- filter(dat_p, metric == mt)
    lay <- .bin_layout(dd)
    # Bar first, tree second: the composition bar anchors the left end and the
    # branches open out of it to the right.
    p   <- .bin_bar(dd, lay) +
           .bin_tree(lay, sprintf("%s, %s", mt, pr$lab)) +
           plot_layout(widths = c(1, 1.9))

    # Height follows the printed line count -- the same quantity the layout uses
    # for row spacing -- so text density is constant across the three figures.
    ggsave(file.path(Q5$FIGS, sprintf("%s%s.png", C5_OUT[[mt]], pr$tag)), p,
           width = 10.0,
           height = max(3.6, min(9, 0.17 * sum(lay$nodes$h) + 1.2)),
           dpi = 200)
  }

  cat(sprintf("\n== C5 bin membership, by characteristic (%s) ==\n", pr$lab))
  print(as.data.frame(dat_p |>
    group_by(metric, shown) |>
    summarise(n = n(),
              fams = paste(sprintf("%s:%d", levels(droplevels(fam_f)),
                                   as.vector(table(droplevels(fam_f)))),
                           collapse = " "),
              .groups = "drop")))
}

# -- Console summary -----------------------------------------------------------
# Kept as a printed table rather than a second figure: the deck needs only the
# annual histogram, but the per-threshold breakdown is worth having to hand when
# the meeting asks where the collapse is worst.
cat("\n== distinct displayed values, per threshold (annual) ==\n")
print(as.data.frame(cfg_win |>
  filter(window == 1) |>
  group_by(thr_f) |>
  summarise(n_shown = n_distinct(shown), n_cfg = n(), .groups = "drop")))

cat("\n== the annual collapse in full: which configs share each value ==\n")
ann <- cfg_win |>
  filter(window == 1) |>
  arrange(shown, rate) |>
  mutate(cfg = sprintf("%s-%d @ %.2f", fam, scale, start_thr)) |>
  group_by(shown) |>
  summarise(n = n(),
            rate_lo = min(rate), rate_hi = max(rate),
            .groups = "drop") |>
  mutate(spread_pct = 100 * (rate_hi - rate_lo) / rate_lo)
print(as.data.frame(ann), digits = 3)

cat("\n== duration, whole months ==\n")
print(as.data.frame(char |>
  mutate(shown = round(dur)) |>
  group_by(shown) |>
  summarise(n = n(), lo = min(dur), hi = max(dur), .groups = "drop")), digits = 3)

cat("\n== peak severity, one decimal ==\n")
print(as.data.frame(char |>
  mutate(shown = round(peak, 1)) |>
  group_by(shown) |>
  summarise(n = n(), lo = min(peak), hi = max(peak), .groups = "drop")), digits = 3)

# Severity at whole numbers, for the record: the whole config space spans only
# -2.16 to -1.00, so integer rounding collapses all 60 configs onto 2 values.
# Not plotted -- with a range that narrow the collapse is arithmetic, not a
# finding about the configs.
cat("\n== peak severity, whole numbers (not plotted) ==\n")
print(table(round(char$peak)))

write_parquet(char, file.path(Q5$OUT, "config_characteristics.parquet"))
write_parquet(cfg_win, file.path(Q5$OUT, "config_rounding.parquet"))
cat("\n13 config rounding ->", Q5$FIGS, "\n")
