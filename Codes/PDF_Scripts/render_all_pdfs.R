library(rmarkdown)

rmd_files <- list.files(
  path = dirname(rstudioapi::getSourceEditorContext()$path),
  pattern = "^[0-9].*\\.Rmd$",
  full.names = TRUE
)

for (f in rmd_files) {
  message("Rendering: ", basename(f))
  tryCatch(
    render(f, output_format = "pdf_document", quiet = FALSE),
    error = function(e) message("FAILED: ", basename(f), " — ", e$message)
  )
}

message("Done. PDFs in: ", dirname(rmd_files[1]))
