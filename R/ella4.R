#'aggregate ella output, 4-plex
#'
#'@importFrom magrittr "%>%"
#'
#'@examples ella4("/Users/phoebelam/Desktop/spah")
#'
#'@export
ella4 <- function(path) {
  
  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, paste(path, "/consolidated.RDS", sep=""))
  
  filenames = list.files(path=path, pattern = ".csv" ,full.names= TRUE, recursive=FALSE)
  
  for (f in filenames) {
    
    print(f)

   # dat<-read.csv("KitsExport_Maggie Plates 6-9 PPS only.csv")
    dat <- read.csv(f)
  
    dat %>%
      rename(analyte = AnalyteName,
             id = SampleName,
             conc_mean = NonlimitedCalculatedConcentration,
             rfu_mean= RFU,
             conc_cv = CalculatedConcentrationPercentCV,
             gnr1_conc = Gnr1CalculatedConcentration,
             gnr2_conc = Gnr2CalculatedConcentration,
             gnr3_conc = Gnr3CalculatedConcentration,
             gnr1_rfu = Gnr1RFU,
             gnr2_rfu = Gnr2RFU,
             gnr3_rfu = Gnr3RFU,
             rfu_cv = RFUPercentCV,
             gnr_count = GnrCount,
             kitid = KitId) -> dat
    
    #merge
    consol <- readRDS(paste(path, "/consolidated.RDS", sep=""))
    consol <- gtools::smartbind(consol,dat)
    saveRDS(consol, paste(path, "/consolidated.RDS", sep=""))
    
  }
  
  dat <- readRDS(paste(path, "/consolidated.RDS", sep=""))[-1,-1] 
  dat %>%
    mutate(id = case_when(id==""~ NA_character_,
                          TRUE~id),
           InletComment = case_when(InletComment==""~ NA_character_,
                                    TRUE~as.character(InletComment))) %>% 
    filter(is.na(id)==F) %>%
    mutate(analyte = case_when(grepl("IL-10", analyte)==T~ "il10",
                               grepl("IL-6", analyte)==T~ "il6",
                               grepl("IL-1ra", analyte)==T~ "il1ra",
                               grepl("TNF-a", analyte)==T~ "tnfa")) -> dat
  

  dat %>%
    select(., id, analyte, rfu_cv, gnr1_rfu:gnr3_rfu, everything() ) %>%
    filter(grepl("SPAH", id)==T)  -> sample
  
  sample %>%
    mutate(id =case_when(is.na(InletComment)==F~  paste(id, InletComment, sep="_"),
                         TRUE~ id)) -> sample
  

  tidyr::pivot_wider(dplyr::select(sample, id, analyte, rfu_cv, gnr1_rfu:gnr3_rfu, gnr1_conc:gnr3_conc, kitid), 
                     id_cols = "id",
                     names_from = "analyte",
                     values_from = rfu_cv:kitid) %>%
    dplyr::select(., id:gnr3_conc_tnfa, kitid_il6) %>%
    dplyr::rename(kitid = kitid_il6) %>%
    dplyr::arrange(id) ->wsample
  
  wsample %>%
    dplyr::filter(if_any(c(rfu_cv_il10, rfu_cv_il1ra, rfu_cv_il6, rfu_cv_tnfa), ~ abs(.)> 10)) %>%
    dplyr::select(., -contains("conc"))  %>%
    arrange(id)->highcv
  
  if (file.exists(paste(path, "/consolidated", sep=""))){
    
    openxlsx::write.xlsx(list("for master" = wsample,
                              "high cv only" = highcv), paste(path, "/consolidated/SPAH consolidated.xlsx", sep=""), rowNames=F)
    
  } else {
    
    dir.create(paste(path, "/consolidated", sep=""))
    openxlsx::write.xlsx(list("for master" = wsample,
                              "high cv only" = highcv), paste(path, "/consolidated/SPAH consolidated.xlsx", sep=""), rowNames=F)
    
  }

  
}
