# Render one figure at the deck's fig dimensions to confirm the legend fits.
setwd("Codes/reports"); source("_slide_helpers.R")
p <- box_slide(add_aes(pixel_metric("duration",12),12),
               "Duration (months)", "x")
# same as deck: fig.width=9.2, fig.height=4.6
ggsave("../_archive/_deckfig.png", p, width=9.2, height=4.6, dpi=150, bg="white")
cat("done\n")
