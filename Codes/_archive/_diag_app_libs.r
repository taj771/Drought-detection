chk <- function(p) paste0(p,"=", if(requireNamespace(p,quietly=TRUE)) as.character(utils::packageVersion(p)) else "MISSING")
dbutils.notebook.exit(paste(vapply(c("ggplot2","sf","tidyr","viridisLite","dplyr"), chk, character(1)), collapse=" || "))
