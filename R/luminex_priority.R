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

luminex_priority <- function(path, filename, 
                             pick_interassay = T, 
                             name_of_master_sheet = 'for master', 
                             number_of_candidates = 30){
  
  
  if (isTRUE(pick_interassay)) {
    
    dat <- openxlsx::read.xlsx(
      paste(path, filename, sep = "/"),
      sheet = name_of_master_sheet
    ) %>%
      dplyr::filter(!is.na(id)) %>%
      dplyr::filter(!grepl("_us", id))
    
    cv_cols <- c("il1b_mfi_cv", "il6_mfi_cv", "tnfa_mfi_cv")
    
    candidate_pool <-
      dat %>%
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(cv_cols),
          ~ !. %in% c(100, 0, 50, 33.33333)
        )
      ) %>%
      dplyr::mutate(
        condition = stringr::str_extract(
          id,
          "crt_6\\.5|crt_6|crt_7|lps|r848|hsp60"
        ),
        mean_cv = rowMeans(
          dplyr::across(dplyr::all_of(cv_cols)),
          na.rm = TRUE
        ),
        max_cv = pmax(
          il1b_mfi_cv,
          il6_mfi_cv,
          tnfa_mfi_cv,
          na.rm = TRUE
        )
      ) %>%
      dplyr::arrange(max_cv, mean_cv)
    
    selected <- list()
    
    condition_counts <- setNames(
      integer(length(unique(candidate_pool$condition))),
      sort(unique(candidate_pool$condition))
    )
    
    plate_counts <- setNames(
      integer(length(unique(candidate_pool$plate))),
      sort(unique(candidate_pool$plate))
    )
    
    while (length(selected) < number_of_candidates &&
           nrow(candidate_pool) > 0) {
      
      candidate_pool <- candidate_pool %>%
        dplyr::mutate(
          condition_n = condition_counts[condition],
          plate_n = plate_counts[plate]
        ) %>%
        dplyr::arrange(
          condition_n,
          plate_n,
          max_cv,
          mean_cv
        )
      
      pick <- candidate_pool[1, ]
      
      selected[[length(selected) + 1]] <- pick
      
      condition_counts[pick$condition] <-
        condition_counts[pick$condition] + 1
      
      plate_counts[pick$plate] <-
        plate_counts[pick$plate] + 1
      
      candidate_pool <- candidate_pool %>%
        dplyr::filter(id != pick$id)
    }
    
    interassay <-
      dplyr::bind_rows(selected) %>%
      dplyr::select(
        id,
        condition,
        plate,
        max_cv,
        mean_cv,
        dplyr::everything()
      )
    
  }
  
  
  
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
  
  if (isTRUE(pick_interassay)) {
  
    openxlsx::write.xlsx(
      list(
        "Inter-assay candidates" = interassay,
        "Repeat priority" = out
      ),
      file = paste0(path, "/luminex prioritization.xlsx"),
      rowNames = FALSE
    )
    
    
  }else {
    openxlsx::write.xlsx(
      out,
      paste0(path, "/luminex prioritization.xlsx", sep=""),
      rowNames = FALSE
    )
  }
  
  
  
  message("f h r c | done exporting repeat priority list.")
  
  invisible(out)

}
