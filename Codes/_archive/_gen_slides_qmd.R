# Generate the slide-deck qmd with EXPLICIT ## slides + per-slide chunk, so each
# becomes a real beamer frame (the results='asis' loop merged them).
hdr <- '---
title: "Drought Characteristics Across Configurations"
subtitle: "Benin — ERA5 | Reference (1973–2002) vs Observation (2003–2023)"
author: "GWSC Drought Project"
date: today
classoption: t          # top-align all frame content (image sits under title)
format:
  beamer:
    aspectratio: 169
    theme: default
    colortheme: seahorse
    fig-align: center
    include-in-header:
      text: |
        \\setbeamersize{text margin left=6pt, text margin right=6pt}
        % Plain theme: no info footline eating the bottom, so the legend is
        % fully visible. Small page number bottom-right only.
        \\setbeamertemplate{footline}{\\hfill\\tiny\\insertframenumber\\kern1em\\vskip1pt}
        \\setbeamertemplate{navigation symbols}{}
        % Section divider slides: vertically center the section title (content
        % slides stay top-aligned via classoption t).
        \\AtBeginSection[]{%
          \\begingroup
          \\setbeamertemplate{section page}{%
            \\vfill
            {\\centering
             \\begin{beamercolorbox}[sep=12pt,center,rounded=false]{frametitle}
               \\usebeamerfont{frametitle}\\insertsectionhead
             \\end{beamercolorbox}\\par}
            \\vfill}%
          \\frame{\\sectionpage}%
          \\endgroup}
execute:
  echo: false
  warning: false
  message: false
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE,
                      fig.align="center", fig.width=9.2, fig.height=4.6,
                      out.height="80%", dpi=150)   # per-slide override in each chunk
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(ggplot2);library(stringr)})
source("_slide_helpers.R")
```
'

# build one slide block. out_h = per-slide figure height (so slides with a
# footnote shrink a bit more; slides without one use more space). Legend must
# always clear the beamer footer, so even the "full" height stays < 100%.
slide_block <- function(section_new, title, expr, footnote=NULL, out_h="80%") {
  s <- ""
  if (!is.null(section_new)) s <- paste0("\n# ", section_new, "\n")
  fn <- ""
  if (!is.null(footnote))
    fn <- paste0("\n\\vspace{1pt}\\footnotesize\\textit{", footnote, "}\n")
  paste0(s,
    "\n## ", title, "\n\n",
    "```{r}\n#| out.height: \"", out_h, "\"\n", expr, "\n```\n", fn)
}

CH <- list(
  list(sec="Frequency",        metric="frequency", ylab='Frequency (events/pixel/yr)', fn="box_slide"),
  list(sec="Duration",         metric="duration",  ylab='Duration (months)',           fn="box_slide"),
  list(sec="Peak severity",    metric="severity",  ylab='Peak severity (index; more negative = worse)',     fn="box_slide"),
  list(sec="Pixels affected (%)", metric="pct",     ylab='Pixels affected (%)',         fn="bar_slide")
)
SCALES  <- c(1,3,6,12)
# %-area characteristic varies THRESHOLD (one slide each); each slide is a facet
# grid of index rows x accumulation-scale cols (a time series can't dodge
# ref/obs, so the ref/obs split is a shaded band and all scales fit in one grid).
THRS    <- c(0.0,-0.5,-1.0,-1.5)
THR_LAB <- c("threshold 0.0","threshold -0.5","threshold -1.0","threshold -1.5")

body <- ""
ci <- 0
for (c in CH) {
  ci <- ci + 1
  sec_title <- paste0("Drought characteristic ", ci, ": ", c$sec)
  first <- TRUE

  if (c$metric == "pct") {
    # vary threshold; fixed 6-month scale; faceted time-series per index
    for (ti in seq_along(THRS)) {
      thr <- THRS[ti]
      title <- paste0(c$sec, " — ", THR_LAB[ti])
      expr  <- sprintf('print(pct_ts_slide(pct_ts(%s), "%s", "%s — %s"))',
                       format(thr, nsmall=1), c$ylab, c$sec, THR_LAB[ti])
      fnote <- NULL; out_h <- "86%"
      # NB: escape LaTeX-special chars (% is a comment char) or the footnote
      # prints literally.
      if (first) fnote <- paste0("Annual \\% of Benin pixels in drought. ",
        "Rows = index, columns = accumulation scale. Grey band = Observation ",
        "period (2003-2023). A static average would read near 100\\% and hide ",
        "this year-to-year variation.")
      if (first) out_h <- "74%"
      body <- paste0(body, slide_block(if (first) sec_title else NULL,
                                       title, expr, fnote, out_h))
      first <- FALSE
    }
  } else {
    for (sc in SCALES) {
      title <- paste0(c$sec, " — ", sc, "-month accumulation")
      expr <- sprintf('print(%s(add_aes(pixel_metric("%s", %d), %d), "%s", "%s — %d-month"))',
                      c$fn, c$metric, sc, sc, c$ylab, c$sec, sc)
      # Footnote only on Duration 12-month (where the threshold-0.0 extreme is
      # clearest). That one slide shrinks a little; all others use more space.
      fnote <- NULL; out_h <- "84%"
      if (c$metric == "duration" && sc == 12) {
        fnote <- paste0("At threshold 0.0 the index only has to fall below normal, ",
          "so a sustained warming/drying drift (strongest in SPEI-Thornthwaite) ",
          "keeps the index below zero for years, producing very long single events. ",
          "See the example time series in the Appendix.")
        out_h <- "72%"
      }
      body <- paste0(body, slide_block(if (first) sec_title else NULL, title, expr, fnote, out_h))
      first <- FALSE
    }
  }
}

# ---- Appendix: example time series explaining the extreme durations ----------
appendix <- paste0(
  "\n# Appendix\n",
  "\n## Appendix: why threshold 0.0 gives very long events\n\n",
  "![](appendix_ts_example.png){height=68%}\n\n",
  "\\scriptsize\\textit{SPEI-Thornthwaite-12 for the pixel with the longest ",
  "observation-period event (193 months). The index shifts below zero around ",
  "the early 1980s and rarely recovers, so at threshold 0.0 the whole stretch ",
  "counts as one event. This reflects a real warming/drying trend (Thornthwaite ",
  "PET is temperature-driven), not a data error, and is why threshold 0.0 is ",
  "reported for completeness rather than used for operational conclusions.}\n")

writeLines(paste0(hdr, body, appendix), "Codes/reports/drought_characteristics_slides.qmd")
cat("qmd written with explicit per-slide chunks + appendix\n")
