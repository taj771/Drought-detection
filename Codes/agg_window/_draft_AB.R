# Draft only: option A (count matrix) vs option B (dot matrix) under the
# frequency histogram. Scratch script, not part of the pipeline.

source("Codes/agg_window/00_config.R")
suppressMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(patchwork)
})

ALL_THR <- c(0.0, -0.25, -0.5, -1.0, -1.5)

ev <- as.data.frame(read_parquet(Q5$EVENTS)) |>
  filter(method == "threshold",
         event_year >= Q5$Y0, event_year <= Q5$Y1,
         start_thr %in% ALL_THR,
         min_duration == Q5$MINDUR) |>
  mutate(scale = as.integer(str_extract(index, "\\d+")),
         fam   = case_when(str_starts(index, "spi")  ~ "SPI",
                           str_ends(index,   "_pm")  ~ "SPEI-P",
                           str_starts(index, "spei") ~ "SPEI-T"))

grid <- expand_grid(distinct(ev, x, y), distinct(ev, fam, scale, start_thr))

cfg <- ev |>
  count(x, y, fam, scale, start_thr, name = "events") |>
  right_join(grid, by = c("x", "y", "fam", "scale", "start_thr")) |>
  mutate(events = coalesce(events, 0L), rate = events / Q5$NYR) |>
  group_by(fam, scale, start_thr) |>
  summarise(rate = mean(rate), .groups = "drop") |>
  mutate(shown = round(rate), bin = factor(shown))

N_CFG <- nrow(cfg)

# control levels, one row each, in a fixed reading order
LEVELS <- c("SPI", "SPEI-T", "SPEI-P",
            sprintf("thr %.2f", ALL_THR),
            sprintf("%d-month", c(1, 3, 6, 12)))

long <- cfg |>
  mutate(cid = row_number()) |>
  pivot_longer(c(fam, start_thr, scale),
               names_to = "ctl", values_to = "lev",
               values_transform = as.character) |>
  mutate(lev = case_when(ctl == "start_thr" ~ sprintf("thr %.2f", as.numeric(lev)),
                         ctl == "scale"     ~ sprintf("%s-month", lev),
                         TRUE ~ lev),
         lev = factor(lev, levels = rev(LEVELS)),
         block = factor(ctl, levels = c("fam", "start_thr", "scale"),
                        labels = c("index", "threshold", "accumulation")))

# -- histogram (shared top panel) ---------------------------------------------
hist_p <- ggplot(cfg, aes(bin)) +
  geom_bar(fill = "#2c6fb5", colour = "grey20", linewidth = 0.3, width = 0.85) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 3.0, fontface = "bold", colour = "grey15") +
  scale_y_continuous(limits = c(0, 40), expand = expansion(mult = c(0, 0.20))) +
  labs(x = NULL, y = sprintf("configs (of %d)", N_CFG)) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

# -- OPTION A: count matrix ---------------------------------------------------
# One cell per (control level, bin). Number = how many of that bin's configs
# carry that level; shade = that count as a share of the bin.
mat <- long |>
  count(block, lev, bin, name = "n") |>
  complete(nesting(block, lev), bin, fill = list(n = 0L)) |>
  left_join(count(cfg, bin, name = "bin_n"), by = "bin") |>
  mutate(frac = n / bin_n)

pA <- ggplot(mat, aes(bin, lev, fill = frac)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(n == 0, "·", as.character(n)),
                colour = frac > 0.55), size = 2.9, fontface = "bold") +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_gradient(low = "grey96", high = "#2c6fb5", guide = "none") +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey25"),
                      guide = "none") +
  labs(x = "drought events per year, as reported", y = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        strip.placement = "outside",
        strip.background = element_rect(colour = "grey30", fill = "grey92"),
        strip.text.y.left = element_text(angle = 90, face = "bold", size = 7),
        axis.text.y = element_text(size = 7.5),
        axis.title.x = element_text(size = 9))

ggsave("optionA.png", hist_p / pA + plot_layout(heights = c(1, 2.5)),
       width = 5.6, height = 6.0, dpi = 200)

# -- OPTION B: dot matrix, one column per config ------------------------------
# Configs ordered by rate, so columns run left to right within each bin.
ord <- cfg |> mutate(cid = row_number()) |> arrange(rate) |>
  mutate(col = row_number()) |> select(cid, col)
longB <- long |> left_join(ord, by = "cid") |> mutate(cid = col)

pB <- ggplot(longB, aes(cid, lev)) +
  # A column is ONE config, and the reader has to see that: the three dots in a
  # column are that config's index, threshold and accumulation. A count matrix
  # can only say "3 of them are SPI"; this says WHICH three. The alternating
  # stripe carries the eye down a column across the three blocks.
  geom_rect(data = distinct(longB, cid, bin, block) |> filter(cid %% 2 == 0),
            aes(xmin = cid - 0.5, xmax = cid + 0.5, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey93") +
  geom_point(size = 1.5, colour = "#2c6fb5") +
  facet_grid(block ~ bin, scales = "free", space = "free", switch = "y") +
  labs(x = "one column = one config, ordered by true rate within its bin",
       y = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.background = element_rect(colour = "grey30", fill = "grey92"),
        strip.text.y.left = element_text(angle = 90, face = "bold", size = 7),
        strip.text.x = element_text(face = "bold", size = 8),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 7.5),
        axis.title.x = element_text(size = 9))

histB <- hist_p + facet_grid(. ~ bin, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave("optionB.png", histB / pB + plot_layout(heights = c(1, 2.5)),
       width = 9.0, height = 6.0, dpi = 200)

# -- OPTION C: A's compactness, B's specificity --------------------------------
# A's weakness is structural: a count cell sums over configs, so it can say
# "3 of them are SPI" but never which threshold or accumulation those three
# carry. The fix is to stop aggregating -- give every config its own cell and
# identify it by POSITION instead: threshold down the y axis, accumulation
# across the x, one panel per index family. That is all 60 configs, each fully
# labelled by where it sits, in a grid small enough for a slide. The fill is
# the bin it reports, so the histogram above and this grid share one colour key.
BIN_COL <- c("0" = "#f0f0f0", "1" = "#bdd7e7", "2" = "#6baed6", "3" = "#2171b5")

gridC <- cfg |>
  mutate(sc_f  = factor(sprintf("%d-month", scale),
                        levels = sprintf("%d-month", c(1, 3, 6, 12))),
         thr_f = factor(sprintf("%.2f", start_thr),
                        levels = sprintf("%.2f", ALL_THR)),
         fam_f = factor(fam, levels = c("SPI", "SPEI-T", "SPEI-P")))

pC <- ggplot(gridC, aes(sc_f, thr_f, fill = bin)) +
  geom_tile(colour = "white", linewidth = 1.1) +
  # True rate under the reported bin: the point of the figure is that cells
  # sharing a colour do NOT share a rate.
  geom_text(aes(label = sprintf("%s\n%.2f", bin, rate),
                colour = bin %in% c("2", "3")),
            size = 2.5, lineheight = 0.95, fontface = "bold") +
  facet_wrap(~ fam_f, nrow = 1) +
  scale_fill_manual(values = BIN_COL, name = "reads as") +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey20"),
                      guide = "none") +
  labs(x = "accumulation period", y = "threshold") +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(colour = "grey30", fill = "grey92"),
        strip.text = element_text(face = "bold", size = 9),
        axis.text.x = element_text(size = 7.5),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9))

histC <- hist_p +
  scale_fill_manual(values = BIN_COL, guide = "none") +
  aes(fill = bin) +
  labs(x = "drought events per year, as reported") +
  theme(axis.text.x = element_text(), axis.ticks.x = element_line(),
        axis.title.x = element_text(size = 9))

ggsave("optionC.png", histC / pC + plot_layout(heights = c(1, 1.9)),
       width = 8.6, height = 5.6, dpi = 200)

# -- OPTION D: name every config, listed under the bin it reports --------------
# The most literal answer to "which configs are in this bin": write their names
# out. Naming convention is family-accumulation @ threshold, e.g. "SPI-3 @
# -0.50", so a name carries all three controls and no legend is needed. The
# rate is printed beside each name, because the finding is that names sharing a
# bin do NOT share a rate. Bin 1 holds 35 names, too many for one column, so
# each bin wraps into as many sub-columns as it needs at ROWS_MAX per column.
# Compact name: family, accumulation, threshold run together -- "SPI3-0.5" --
# so a whole config fits on one short line and 35 of them stack in one column.
named <- cfg |>
  arrange(bin, rate) |>
  mutate(nm = sprintf("%s%d @ %s", fam, scale,
                      sub("0\\.00", "0", sprintf("%.2f", start_thr)))) |>
  group_by(bin) |>
  mutate(row = row_number() - 1) |>
  ungroup()

pD <- ggplot(named, aes(0, -row)) +
  geom_text(aes(label = sprintf("%-12s %.2f", nm, rate)),
            size = 2.0, family = "mono", hjust = 0.5, colour = "grey15") +
  facet_grid(. ~ bin) +
  scale_y_continuous(expand = expansion(add = 0.8)) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        strip.background = element_rect(colour = "grey30", fill = "grey92"),
        strip.text = element_text(face = "bold", size = 9))

histD <- hist_p + facet_grid(. ~ bin, scales = "free_x", space = "free_x") +
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave("optionD.png", histD / pD + plot_layout(heights = c(1, 4.2)),
       width = 8.0, height = 7.6, dpi = 200)

cat("bins:\n"); print(count(cfg, bin))
