# One config (SPI-3, threshold -0.5), all box-able characteristics
# (frequency, duration, severity), each shown BOTH ways:
#   space-collapse (21 yearly values) vs time-collapse (154 pixel values).
# Faceted by characteristic so the team can compare the two paths directly.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUTF <- "Codes/reports/agg_paths"; dir.create(OUTF, showWarnings = FALSE, recursive = TRUE)
OBS0 <- 2003; OBS1 <- 2023; NYR <- OBS1 - OBS0 + 1

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  filter(method == "threshold", start_thr == -0.5,
         event_year >= OBS0, event_year <= OBS1) |>
  mutate(scale = as.integer(str_extract(index, "\\d+"))) |>
  filter(str_starts(index, "spi"), scale == 3)     # SPI-3, thr -0.5

# ---- per event -> we need per pixel-year values for each characteristic ------
# frequency: count events per pixel per year
# duration : mean event duration per pixel per year
# severity : mean peak (driest) per pixel per year
py <- ev |> group_by(x, y, event_year) |>
  summarise(frequency = n(),
            duration = mean(duration_months, na.rm = TRUE),
            severity = mean(peak_index, na.rm = TRUE), .groups = "drop") |>
  complete(nesting(x, y), event_year = OBS0:OBS1,
           fill = list(frequency = 0, duration = NA, severity = NA))

CHARS <- c(frequency = "Frequency (events/yr)",
           duration  = "Duration (months)",
           severity  = "Peak severity (index)")

build <- function(col) {
  # Way 1: collapse space -> yearly mean -> 21 values
  w1 <- py |> group_by(event_year) |>
    summarise(v = mean(.data[[col]], na.rm = TRUE), .groups = "drop") |>
    transmute(value = v, path = "Space-collapse\n(21 yrs)")
  # Way 2: collapse time -> per-pixel summary -> 154 values
  w2 <- if (col == "frequency")
    py |> group_by(x, y) |> summarise(v = sum(frequency) / NYR, .groups = "drop")
  else
    py |> group_by(x, y) |> summarise(v = mean(.data[[col]], na.rm = TRUE), .groups = "drop")
  w2 <- w2 |> transmute(value = v, path = "Time-collapse\n(154 px)")
  bind_rows(w1, w2) |> mutate(char = CHARS[[col]])
}
d <- bind_rows(lapply(names(CHARS), build)) |>
  mutate(char = factor(char, levels = CHARS),
         path = factor(path, levels = c("Space-collapse\n(21 yrs)",
                                        "Time-collapse\n(154 px)")))

p <- ggplot(d, aes(path, value, fill = path)) +
  geom_boxplot(width = 0.55, outlier.size = 0.4, colour = "grey20", linewidth = 0.3) +
  geom_jitter(width = 0.08, size = 0.4, alpha = 0.25) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.4,
               fill = "white", colour = "black") +
  facet_wrap(~char, scales = "free_y") +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"), guide = "none") +
  labs(title = "SPI-3, threshold -0.5 — two aggregation paths, all characteristics",
       subtitle = "White diamond = overall mean (same both ways). Space-collapse box = temporal spread; time-collapse box = spatial spread.",
       x = NULL, y = NULL) +
  theme_bw(base_size = 10) +
  theme(strip.background = element_rect(fill = "grey92", colour = "grey30"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.subtitle = element_text(size = 8))
ggsave(file.path(OUTF, "agg_allchar_pair.png"), p, width = 9, height = 4.4, dpi = 150, bg = "white")

# print means + sds for the record
for (col in names(CHARS)) {
  dd <- build(col)
  s <- dd |> group_by(path) |> summarise(m = round(mean(value, na.rm=TRUE),3),
                                         sd = round(sd(value, na.rm=TRUE),3), .groups="drop")
  cat(sprintf("%-22s space: mean %.3f sd %.3f | time: mean %.3f sd %.3f\n",
      CHARS[[col]], s$m[1], s$sd[1], s$m[2], s$sd[2]))
}
cat("\nfigure ->", file.path(OUTF, "agg_allchar_pair.png"), "\n")
