#' Render the Luminex QA report as a single HTML file
#'
#' Knits the packaged R Markdown template into one self-contained HTML file
#' (plots and tables embedded) that opens in any browser and can be shared by
#' email or OneDrive. Nothing else needs to be sent alongside it.
#'
#' Sections: Summary (every problem, plus a count of low-dose reversals),
#' Missing & data entry, Stimulation & inhibition order, Dose-response,
#' Tagged participants (issues, chart and values for each participant with a
#' problem in their values) and Just in case (the same for participants whose
#' only reversals are at the low-dose wells).
#'
#' @param qa A `luminex_qa` object from [luminex_qa()], **or** the path to a
#'   master `.xlsx` (in which case [luminex_qa()] is run first with `...`).
#' @param output_dir Folder for the HTML. Default: the master file's folder
#'   when `qa` is a path, otherwise the working directory.
#' @param output_file File name. Default
#'   `"<STUDY>_<VISIT>_luminex_qa_<date>.html"`.
#' @param open Open the report in the browser when done? Default: only in an
#'   interactive session.
#' @param max_detail Maximum number of participant panels per section (default:
#'   all of them).
#' @param ... Passed to [luminex_qa()] when `qa` is a path: `sheet`, `hsp60`,
#'   `r848`, `il10`, `expected_ids`, ...
#' @return The path to the HTML file, invisibly.
#' @examples
#' \dontrun{
#' luminex_qa_report("C:/.../OTR V1 master file.xlsx", sheet = "for master",
#'                   hsp60 = TRUE, r848 = TRUE, il10 = TRUE)
#'
#' # or, to look at the results in R as well:
#' qa <- luminex_qa("C:/.../OTR V1 master file.xlsx", il10 = TRUE)
#' qa$summary
#' luminex_qa_report(qa, output_dir = "C:/.../QA")
#' }
#' @export
luminex_qa_report <- function(qa, output_dir = NULL, output_file = NULL,
                              open = interactive(), max_detail = Inf, ...) {
  for (pkg in c("rmarkdown", "knitr", "ggplot2", "htmltools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is needed for the report: install.packages('", pkg, "')", call. = FALSE)
    }
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc was not found. Run this from RStudio (which ships with pandoc), ",
         "or install pandoc from https://pandoc.org.", call. = FALSE)
  }
  if (is.character(qa)) {
    path <- qa
    if (is.null(output_dir)) output_dir <- dirname(path)
    qa <- luminex_qa(path, ...)
  }
  if (!inherits(qa, "luminex_qa")) stop("`qa` must be a luminex_qa() result or a path to a master file.", call. = FALSE)
  if (is.null(output_dir)) output_dir <- getwd()
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (is.null(output_file)) {
    tag <- paste(stats::na.omit(c(qa$settings$study, qa$settings$visit)), collapse = "_")
    output_file <- paste0(if (nzchar(tag)) paste0(tag, "_") else "", "luminex_qa_", Sys.Date(), ".html")
  }

  template <- system.file("rmarkdown", "templates", "luminex_qa", "skeleton", "skeleton.Rmd", package = "fhrc")
  if (!nzchar(template)) stop("Report template not found - please reinstall fhrc.", call. = FALSE)

  work <- tempfile("fhrc_qa_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  rmd <- file.path(work, "luminex_qa.Rmd")
  file.copy(template, rmd)

  message("f h r c | rendering QA report...")
  out <- rmarkdown::render(rmd, output_file = output_file, output_dir = output_dir,
                           intermediates_dir = work, knit_root_dir = work,
                           params = list(qa = qa, max_detail = max_detail),
                           envir = new.env(parent = globalenv()), quiet = TRUE)
  message("f h r c | report saved to ", out)
  if (isTRUE(open)) utils::browseURL(out)
  invisible(out)
}
