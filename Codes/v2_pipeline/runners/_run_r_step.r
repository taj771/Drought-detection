# Databricks notebook source
# Runner: source one v2 R analysis step + report completion.
# STEP is read from a notebook widget (set per job task via base_parameters),
# falling back to a default for manual runs.
SCRIPT_DIR <- "/Workspace/Users/tajayalath@ua.edu/Drought-indices/files/Codes/v2_pipeline/R"

STEP <- tryCatch(dbutils.widgets.get("STEP"), error = function(e) "")
if (is.null(STEP) || STEP == "") STEP <- "04. Sanity_Checks.R"

# COUNTRY is injected so all tasks in one job run agree on the country. Set it
# once as a job parameter; the R steps must not hardcode a country.
# source() below runs in this global env, so COUNTRY_TAG and VOLUME_PATH are
# visible to the sourced script (scripts 06/07 pick VOLUME_PATH up via their
# `if (!exists("VOLUME_PATH"))` guard).
COUNTRY_TAG <- tryCatch(dbutils.widgets.get("COUNTRY"), error = function(e) "")
if (is.null(COUNTRY_TAG) || COUNTRY_TAG == "") COUNTRY_TAG <- "Benin"
VOLUME_PATH <- file.path("/Volumes/projects/gwsc_drought/drought_outputs/v2",
                         COUNTRY_TAG)
cat(sprintf("=== COUNTRY = %s ===\n", COUNTRY_TAG))

# STEPS selects which tasks actually do work in this run. "all" runs everything.
# Otherwise a pipe-separated list of task keys (pipe, not comma: --params is
# itself comma-separated). A task not in the list stops here in seconds, so the
# DAG shape and downstream dependencies are unchanged — only the work is
# skipped. TASK comes from the task's own base_parameters.
TASK  <- tryCatch(dbutils.widgets.get("TASK"),  error = function(e) "")
STEPS <- tryCatch(dbutils.widgets.get("STEPS"), error = function(e) "all")
if (is.null(STEPS) || STEPS == "") STEPS <- "all"

if (tolower(STEPS) != "all") {
  wanted <- trimws(strsplit(STEPS, "|", fixed = TRUE)[[1]])
  wanted <- wanted[nzchar(wanted)]
  if (is.null(TASK) || TASK == "") {
    stop("STEPS is set but TASK is empty — this task cannot tell whether it ",
         "was selected. Add TASK to its base_parameters.")
  }
  if (!(TASK %in% wanted)) {
    cat(sprintf("=== SKIP %s: not in STEPS (%s) ===\n", TASK, STEPS))
    dbutils.notebook.exit(paste0("skipped:", TASK))
  }
}

cat(sprintf("=== Running %s ===\n", STEP))
t0 <- Sys.time()
source(file.path(SCRIPT_DIR, STEP), echo = FALSE)
cat(sprintf("=== %s done in %.1f min ===\n",
            STEP, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
