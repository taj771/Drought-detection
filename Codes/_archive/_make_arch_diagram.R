# Build the "big picture" diagram as a clean PNG for the architecture doc.
suppressPackageStartupMessages({ library(ggplot2) })

box <- function(x, y, w, h, label, fill, col = "grey30", txtcol = "black",
                size = 3.4, fontface = 1) {
  list(
    annotate("rect", xmin = x - w/2, xmax = x + w/2,
             ymin = y - h/2, ymax = y + h/2,
             fill = fill, colour = col, linewidth = 0.5),
    annotate("text", x = x, y = y, label = label,
             size = size, colour = txtcol, fontface = fontface, lineheight = 0.9)
  )
}
arrow_seg <- function(x0, y0, x1, y1)
  annotate("segment", x = x0, y = y0, xend = x1, yend = y1,
           arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
           linewidth = 0.5, colour = "grey40")

p <- ggplot() +
  # data source
  box(5, 9.2, 7.4, 1.0,
      "ERA5 global weather data\n(rainfall + temperature, 1950–2026)",
      fill = "#dbe9f6", fontface = 2, size = 3.6) +
  # arrows down to three pipelines
  arrow_seg(2.2, 8.7, 2.2, 7.0) +
  arrow_seg(5.0, 8.7, 5.0, 7.0) +
  arrow_seg(7.8, 8.7, 7.8, 7.0) +
  # three pipeline boxes
  box(2.2, 6.1, 2.6, 1.8,
      "v1 (legacy)\n\nR · single cluster\nDEPRECATED\n(reference only)",
      fill = "#eeeeee") +
  box(5.0, 6.1, 2.6, 1.8,
      "v2 — RESEARCH\n\nPython + R · multi-node\nIndices → detection\n→ statistics (75 configs)",
      fill = "#e7f4e7", fontface = 1) +
  box(7.8, 6.1, 2.6, 1.8,
      "app_monitor — DAILY\n\nR · single cluster\nDetect + visualise\n(1 chosen config)",
      fill = "#fdf0e3") +
  # captions under
  annotate("text", x = 2.2, y = 4.9, label = "kept for traceability",
           size = 2.9, colour = "grey45", fontface = 3) +
  annotate("text", x = 5.0, y = 4.9, label = "full statistical study",
           size = 2.9, colour = "grey45", fontface = 3) +
  annotate("text", x = 7.8, y = 4.9, label = "quick country snapshot",
           size = 2.9, colour = "grey45", fontface = 3) +
  coord_fixed(ratio = 1, xlim = c(0.3, 9.7), ylim = c(4.4, 10)) +
  theme_void()

ggsave("Codes/reports/arch_diagram.png", p, width = 9, height = 5.2,
       dpi = 200, bg = "white")
cat("diagram written\n")
