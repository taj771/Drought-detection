# Databricks notebook source
# Runner: source one v2 R analysis step + report completion.
# STEP is read from a notebook widget (set per job task via base_parameters),
# falling back to a default for manual runs.
SCRIPT_DIR <- "/Workspace/Users/tajayalath@ua.edu/Drought-indices/files/Codes"

STEP <- tryCatch(dbutils.widgets.get("STEP"), error = function(e) "")
if (is.null(STEP) || STEP == "") STEP <- "04. Sanity_Checks.R"

cat(sprintf("=== Running %s ===\n", STEP))
t0 <- Sys.time()
source(file.path(SCRIPT_DIR, STEP), echo = FALSE)
cat(sprintf("=== %s done in %.1f min ===\n",
            STEP, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
