suppressMessages({ library(arrow); library(dplyr) })
d <- as.data.frame(read_parquet("Codes/_archive/_data/summary_yearly.parquet"))
cat("=== summary_yearly columns ===\n"); print(names(d))
cat("rows:", nrow(d), " configs:", length(unique(d$config_id)), "\n")
cat("=== sample config_ids ===\n"); print(head(sort(unique(d$config_id)), 15))
for (c in c("index","method","start_thr","country","event_year")) {
  if (c %in% names(d)) { cat("--", c, ":\n"); print(head(sort(unique(d[[c]])), 8)) }
}
cat("\n=== annual_summary columns ===\n")
a <- as.data.frame(read_parquet("Codes/_archive/_data/annual_summary.parquet"))
print(names(a)); cat("rows:", nrow(a), "\n")
cat("=== annual sample row ===\n"); print(a[1, ])
