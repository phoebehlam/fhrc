#'@importFrom magrittr "%>%"
#'@export
dup <- function (path, study) {
  
  check <- function (f, study){
    
    basename(f) %>%
      substr(., 23, 24) %>% as.numeric () -> day
    
    d <- read.csv(f, na.strings = "")
    d %>% 
      tail (., -2) %>% 
      filter (is.na (ExternalReference)==FALSE & is.na(RecipientLastName) == FALSE, DistributionChannel != "preview" & ExternalReference != 9999) %>%
      filter (grepl("TEST", .$RecipientFirstName)== FALSE & grepl("hoebe", .$RecipientFirstName)== FALSE & grepl("Test", .$RecipientFirstName)== FALSE) %>%
      mutate (dup = duplicated(.$ExternalReference)) %>% 
      filter (dup == "TRUE") -> d
    
    if (nrow(d) != 0) {
      
      result = paste ("diary day", day, "has duplicate entries")
      ids= as.data.frame(tabyl(d$ExternalReference, show_na = FALSE, show_missing_levels = FALSE))
      
    } else if (nrow(d)==0) {
      
      result = paste ("diary day", day, "ok")
      ids = "none"
      
    }
    
    list <- list ("result" = result, "ids with duplicates" = ids)
    
    return (list)
  }
  
  filenames = intersect(list.files(path = paste(path, "/", study, sep=""), pattern = study, full.names = TRUE, recursive = TRUE), 
                        list.files(path = paste(path, "/", study, sep=""), pattern = ".csv", full.names = TRUE, recursive = TRUE))
  
  
  for (f in filenames) {
    print (check(f))
    basename(f) %>%
      substr(., 23, 24) %>% as.numeric (.) -> day
    
    d <- read.csv(f, na.strings = "")
    d %>% 
      tail (., -2) %>% 
      filter (is.na (ExternalReference)==FALSE & is.na(RecipientLastName) == FALSE, DistributionChannel != "preview" & ExternalReference != 9999) %>%
      filter (grepl("TEST", .$RecipientFirstName)== FALSE & grepl("hoebe", .$RecipientFirstName)== FALSE & grepl("Test", .$RecipientFirstName)== FALSE) %>%
      mutate (dup = duplicated(.$ExternalReference)) %>% 
      filter (dup == "TRUE") -> d
    
    if (nrow(d) != 0) {
      
      result = paste ("diary day", day, "has duplicate entries")
      ids= as.data.frame(tabyl(d$ExternalReference, show_na = FALSE, show_missing_levels = FALSE))
      
    } else if (nrow(d)==0) {
      
      result = paste ("diary day", day, "ok")
      ids = "none"
      
    }
    
  }
  list <- list ("result" = result, "ids with duplicates" = ids)
  
  return (list)
}
