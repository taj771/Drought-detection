setwd("Codes/reports"); source("_slide_helpers.R")
p <- box_slide(add_aes(pixel_metric("severity",6),6),
               "Peak severity (index; more negative = worse)", "x")
ggsave("../_archive/_sevcheck.png", p, width=9.0, height=4.2, dpi=150, bg="white")
cat("done\n")
