setwd("Codes/reports"); source("_slide_helpers.R")
p <- box_slide(add_aes(pixel_metric("frequency",6),6), "Frequency (events/pixel/yr)", "x")
ggsave("../_archive/_slidecheck.png", p, width=9.0, height=4.0, dpi=150, bg="white")
