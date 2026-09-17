suppressMessages(library(png))
img <- readPNG("Codes/_archive/_r5f-03.png")
d <- dim(img)
cat("image dims (h x w x ch):", d, "\n")
# bottom 22%
h <- d[1]; bot <- img[round(h*0.78):h, , , drop=FALSE]
writePNG(bot, "Codes/_archive/_r5f_bottom.png")
cat("bottom strip written\n")
