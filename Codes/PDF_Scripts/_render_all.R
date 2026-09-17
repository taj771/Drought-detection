Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
library(rmarkdown)
setwd("C:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices/Codes/PDF_Scripts")
rmd_files <- list.files(pattern = "^[0-9].*\\.Rmd$", full.names = TRUE)
for (f in rmd_files) {
  message("Rendering: ", basename(f))
  tryCatch(
    render(f, output_format = "pdf_document", quiet = FALSE),
    error = function(e) message("FAILED: ", basename(f), " -- ", e$message)
  )
}
message("Done.")
