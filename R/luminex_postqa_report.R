#' Render the pre-scoring QA report, and export the fixed data when the plan is final
#'
#' Knits one self-contained HTML: Missing & data entry, Tagged participants
#' (problems, plan, and charts as measured and as the scoring step will see
#' them), and a Plan summary.
#'
#' Typical use:
#' \enumerate{
#'   \item run with the defaults and review the report with the wet-lab team;
#'   \item run again listing only the participants whose default plan you
#'     disagree with, and `export = TRUE`. This saves the whole "final master"
#'     sheet with the plan applied, next to the master file, for scoring.
#' }
#' \preformatted{
#' luminex_postqa_report(master, ignore_us = c("2066_V2", "2067_V2"),
#'                       na = "1066_V1_r848", keep = "1163_V1", export = TRUE)
#' }
#'
#' The exported file has every row and column of the sheet, in the same order,
#' with the plan applied (`na`: every cytokine value of the well blank;
#' `ignore us`: the US well's `*_avg_mfi` set to 0) and a `fix` column saying
#' what was done to each row. The master file itself is never changed.
#'
#' @param qa A [luminex_postqa()] result, or the path to the master `.xlsx`
#'   (then [luminex_postqa()] is run with `...`).
#' @param export Save the sheet with the plan applied? Default `FALSE`; set
#'   `TRUE` once the plan has been reviewed.
#' @param output_dir Folder for the files. Default: the master file's folder
#'   when `qa` is a path, otherwise the working directory.
#' @param output_file HTML file name. Default
#'   `"<STUDY>_<VISIT>_luminex_prescoring_qa_<date>.html"`.
#' @param export_file File name for the exported data. Default: the master
#'   file's name plus `" fixed <date>.xlsx"`.
#' @param open Open the report when done? Default: only in an interactive session.
#' @param ... Passed to [luminex_postqa()] when `qa` is a path: `sheet`,
#'   `hsp60`, `r848`, `il10`, `na`, `ignore_us`, `keep`, ...
#' @return The paths of the report (and the exported data), invisibly.
#' @export
luminex_postqa_report <- function(qa, export = FALSE, output_dir = NULL, output_file = NULL,
                                  export_file = NULL, open = interactive(), ...) {
  for (pkg in c("rmarkdown", "knitr", "ggplot2", "htmltools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is needed for the report: install.packages('", pkg, "')", call. = FALSE)
    }
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc was not found. Run this from RStudio (which ships with pandoc), ",
         "or install pandoc from https://pandoc.org.", call. = FALSE)
  }
  master <- NULL
  if (is.character(qa)) {
    master <- qa
    if (is.null(output_dir)) output_dir <- dirname(master)
    qa <- luminex_postqa(master, ...)
  }
  if (!inherits(qa, "luminex_postqa")) stop("`qa` must be a luminex_postqa() result or a path to a master file.", call. = FALSE)
  if (is.null(output_dir)) output_dir <- getwd()
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  tag <- paste(stats::na.omit(c(qa$settings$study, gsub(" & ", "_", qa$settings$visit))), collapse = "_")
  tag <- if (nzchar(tag)) paste0(tag, "_") else ""
  if (is.null(output_file)) output_file <- paste0(tag, "luminex_prescoring_qa_", Sys.Date(), ".html")

  export_path <- NULL
  if (isTRUE(export)) {
    if (is.null(export_file)) {
      base <- if (!is.null(master)) sub("\\.xlsx$", "", basename(master)) else paste0(tag, "final master")
      export_file <- paste0(base, " fixed ", Sys.Date(), ".xlsx")
    }
    export_path <- file.path(output_dir, export_file)
    if (!is.null(master) && normalizePath(export_path, mustWork = FALSE) == normalizePath(master, mustWork = FALSE))
      stop("The export would overwrite the master file; choose another export_file.", call. = FALSE)
    sheet <- if (is.na(qa$settings$sheet)) "final master" else qa$settings$sheet
    openxlsx::write.xlsx(stats::setNames(list(qa$fixed), sheet), export_path, rowNames = FALSE, keepNA = FALSE)
    qa$settings$export_file <- basename(export_path)
  }

  template <- system.file("rmarkdown", "templates", "luminex_postqa", "skeleton", "skeleton.Rmd", package = "fhrc")
  if (!nzchar(template)) stop("Report template not found - please reinstall fhrc.", call. = FALSE)
  work <- tempfile("fhrc_postqa_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  rmd <- file.path(work, "luminex_postqa.Rmd")
  file.copy(template, rmd)

  message("f h r c | rendering pre-scoring QA report...")
  out <- rmarkdown::render(rmd, output_file = output_file, output_dir = output_dir,
                           intermediates_dir = work, knit_root_dir = work,
                           params = list(qa = qa), envir = new.env(parent = globalenv()), quiet = TRUE)
  message("f h r c | report saved to ", out)
  if (!is.null(export_path)) message("f h r c | data with the plan applied saved to ", export_path)
  else message("f h r c | data not exported: run again with export = TRUE once the plan is final")
  if (isTRUE(open)) utils::browseURL(out)
  invisible(c(report = out, data = export_path))
}
