# Two ways to summarise a drought characteristic (frequency) for a box plot:
#  Way 1 (collapse SPACE first): each year -> mean freq over 154 pixels ->
#         20 yearly values -> box = TEMPORAL spread.
#  Way 2 (collapse TIME first): each pixel -> events/20yr -> 154 pixel values ->
#         box = SPATIAL spread. (This is what our drought-characteristics deck uses.)
# Same overall mean; different spread. Demo on SPI-3, threshold -1.0, obs 2003-2023.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUTF <- "Codes/reports/agg_paths"; dir.create(OUTF, showWarnings = FALSE, recursive = TRUE)

OBS0 <- 2003; OBS1 <- 2023; NYR <- OBS1 - OBS0 + 1
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  filter(method == "threshold", start_thr == -1.0,
         event_year >= OBS0, event_year <= OBS1) |>
  mutate(scale = as.integer(str_extract(index, "\\d+"))) |>
  filter(str_starts(index, "spi"), scale == 3)     # SPI-3 demo

# per pixel per year: # events that year
ppy <- ev |> group_by(x, y, event_year) |>
  summarise(n = n(), .groups = "drop") |>
  complete(nesting(x, y), event_year = OBS0:OBS1, fill = list(n = 0))

# Way 1: collapse space -> yearly means (20 values)
way1 <- ppy |> group_by(event_year) |>
  summarise(freq = mean(n), .groups = "drop") |>
  transmute(value = freq, path = "Collapse space\n(20 yearly means)")

# Way 2: collapse time -> per-pixel rate (154 values)
way2 <- ppy |> group_by(x, y) |>
  summarise(freq = sum(n) / NYR, .groups = "drop") |>
  transmute(value = freq, path = "Collapse time\n(154 pixel rates)")

d <- bind_rows(way1, way2) |>
  mutate(path = factor(path, levels = c("Collapse space\n(20 yearly means)",
                                        "Collapse time\n(154 pixel rates)")))

cat("Way 1 (space-collapse):  mean", round(mean(way1$value),3),
    " sd", round(sd(way1$value),3), " n", nrow(way1), "\n")
cat("Way 2 (time-collapse):   mean", round(mean(way2$value),3),
    " sd", round(sd(way2$value),3), " n", nrow(way2), "\n")

p <- ggplot(d, aes(path, value, fill = path)) +
  geom_boxplot(width = 0.55, outlier.size = 0.5, colour = "grey20") +
  geom_jitter(width = 0.08, size = 0.5, alpha = 0.35) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", colour = "black") +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"), guide = "none") +
  labs(title = "Same average, different spread — two ways to summarise",
       subtitle = "SPI-3 drought frequency, Benin, 2003-2023, threshold -1.0. White diamond = overall mean.",
       x = NULL, y = "Frequency (events / pixel / yr)") +
  theme_bw(base_size = 11)
ggsave(file.path(OUTF, "agg_box_pair.png"), p, width = 7.5, height = 4.6, dpi = 150, bg = "white")
cat("\nfigure ->", file.path(OUTF, "agg_box_pair.png"), "\n")
