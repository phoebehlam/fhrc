#' mhs-specific script for cleaning a ripple v1 export ready for v2 import
#' 
#' takes a ripple-exported v1 .csv file and turn it into a cleaned .xlsx import
#' 
#' @param path in quotes, enter the path for the exported file (make sure this is .csv)
#' @param new in quotes, enter where you want and the name of the new cleaned file the function outputs (make sure this is .xlsx)
#' @param importid in quotes, ripple -> site admin -> import -> under "download template" select the new study v2 study -> select "template" -> scroll to all the way to the right, you can find the import id under the column "importType"
#' 
#' @examples 
#' mhs.importclean (path = "/Users/phoebelam/Desktop/ripple v1 to v2 migrate/c2021_v1_export.csv", 
#' new = "/Users/phoebelam/Desktop/ripple v1 to v2 migrate/c2021_v2_import.xlsx", 
#' importid = "aDTL2wkzBmMEXsL4u")
#'  
#'@importFrom magrittr "%>%"
#'@export
mhs.importclean <- function(path, importid, new){
  dat <- read.csv(path)
  
  dat %>%
    dplyr::filter(!statusId %in% c("MHS Refused to Enroll",
                            "Need to Schedule/ Reschedule- Mentee",
                            "V1 Complete-Mentor-Dropped Out of Cities")) -> dat
  
  dat %>%
    dplyr::mutate_at(dplyr::vars(sex, race, ethnicity, statusId, cv.v1_difficult_blood_draw_, 
                   cv.registered_with_cities_,
                   cv.mentor_or_mentee_, cv.transportation, cv.dropped_cities_after_v1_, 
                   cv.food_allergies, cv.condition, cv.staff_that_ran_visit,cv.school),
              list(~tolower(.))) %>%
    dplyr::mutate_at(dplyr::vars(birthday, dateSignedConsentForm),
              list(~as.character(.))) %>%
    dplyr::mutate(importType = dplyr::case_when(dplyr::row_number()==1~ importid)) %>%
    dplyr::mutate_at(dplyr::vars(race, ethnicity,statusId,cv.school),
              list(~gsub("/| / | |-|\\.", "_", .)))-> dat
  
  dat %>%
    dplyr::mutate(cv.school = gsub("__", "_", cv.school),
           race = dplyr::case_when(race == "black_african_america"~ "black_african_american",
                            TRUE~ race)) -> dat
  
  print(list("check the following output to make sure these are the ones you want to import",
             janitor::tabyl(dat$statusId, show_missing_levels=F)))
  xlsx::write.xlsx(dat, new, showNA=F, row.names=F)
  
}

