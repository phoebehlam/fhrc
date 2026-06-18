#' Generate repeat-testing priority list for Luminex assays
#'
#' Prioritizes high-CV samples for repeat testing based on the number
#' of cytokines with MFI CV >10% and the average magnitude of CVs.
#'
#' @param path Character. Complete file path, including the excel workbook name (and the .xlsx extension)
#'
#' @examples
#' luminex_priority("/Users/phoebelam/Library/CloudStorage/OneDrive-SharedLibraries-NorthwesternUniversity/FHRC - Documents/NIH R01 Mentoring and Health (MHS)/Wetlab/Immunoassays/Luminex/Mentors", "MHS culture supernatant master file.xlsx")
#'
#' @export

luminex_priority <- function(path, filename){
  
  dat <- openxlsx::read.xlsx(paste(path, filename, sep="/"),
    sheet = "high cv only"
  ) %>%
    dplyr::filter(!is.na(id))
  
  # remove unstimulated samples
  dat <- dat %>%
    dplyr::filter(!grepl("_us", id))
  
  # remove non-informative CV values
  dat <- dat %>%
    dplyr::filter(
      dplyr::if_all(
        c(il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv),
        ~ !. %in% c(100, 0, 50, 33.33333)
      )
    )
  
  # priority based on number of cytokines with CV > 10%
  dat <- dat %>%
    dplyr::mutate(
      priority1 = dplyr::case_when(
        rowSums(
          dplyr::across(
            c(il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv),
            ~ . > 10
          ),
          na.rm = TRUE
        ) == 3 ~ 1,
        rowSums(
          dplyr::across(
            c(il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv),
            ~ . > 10
          ),
          na.rm = TRUE
        ) == 2 ~ 2,
        rowSums(
          dplyr::across(
            c(il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv),
            ~ . > 10
          ),
          na.rm = TRUE
        ) == 1 ~ 3,
        TRUE ~ 0
      )
    )
  
  # secondary priority based on mean CV magnitude
  dat <- dat %>%
    dplyr::mutate(
      priority2 = -rowMeans(
        dplyr::select(
          .,
          il1b_mfi_cv,
          il6_mfi_cv,
          tnfa_mfi_cv
        ),
        na.rm = TRUE
      )
    )
  
  out <- dat %>%
    dplyr::arrange(priority1, priority2) %>%
    dplyr::select(
      id,
      priority1,
      il1b_mfi_cv,
      il6_mfi_cv,
      tnfa_mfi_cv:dplyr::last_col()
    )
  
  openxlsx::write.xlsx(
    out,
    paste0(path, "/repeat_priority_list.xlsx", sep=""),
    rowNames = FALSE
  )
  
  message("f h r c | done exporting repeat priority list.")
  
  invisible(out)
}
