#'report entries with duplicated entries
#'
#'@importFrom magrittr "%>%"
#'
#'@examples dup("/Users/phoebelam/Desktop/clean", "MHS")
#'
#'@export
dup <- function (path, study) {
  
  filenames = intersect(list.files(path = paste(path, "/", study, sep=""), pattern = study, full.names = TRUE, recursive = TRUE), 
                        list.files(path = paste(path, "/", study, sep=""), pattern = ".csv", full.names = TRUE, recursive = TRUE))

  # duplicate check
  check <- function (f, study){
    
    basename(f) %>%
      substr(., 23, 24) %>% as.numeric () -> day
    
    d <- read.csv(f, na.strings = "")
    d %>% 
      tail (., -2) %>% 
      dplyr::filter (is.na (ExternalReference)==FALSE & is.na(RecipientLastName) == FALSE, DistributionChannel != "preview" & ExternalReference != 9999) %>%
      dplyr::filter (grepl("TEST", .$RecipientFirstName)== FALSE & grepl("hoebe", .$RecipientFirstName)== FALSE & grepl("Test", .$RecipientFirstName)== FALSE) %>%
      dplyr::mutate (dup = duplicated(.$ExternalReference)) %>% 
      dplyr::filter (dup == "TRUE") -> d
    
    if (nrow(d) != 0) {
      
      result = paste ("diary day", day, "has duplicate entries")
      ids= as.data.frame(janitor::tabyl(d$ExternalReference, show_na = FALSE, show_missing_levels = FALSE))
      
    } else if (nrow(d)==0) {
      
      result = paste ("diary day", day, "ok")
      ids = "none"
      
    }
    
    list <- list ("result" = result, "ids with duplicates" = ids)
    
    return (list)
  }
  for (f in filenames) {
    print(check(f))
  }
  
}


