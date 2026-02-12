#'aggregate ella output, all inclusive
#'
#'@importFrom magrittr "%>%"
#'
#'
#'@export
#'
ella <- function(path) {
  
  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, paste(path, "/consolidated.RDS", sep=""))
 
  filenames = list.files(path=path, pattern = ".csv" ,full.names= TRUE, recursive=FALSE)
  
  for (f in filenames) {
    
    print(f)
    
    # dat<-read.csv('/Users/phoebelam/Desktop/ella/KitsExport_OTRV1_Plates1-10.csv')
    dat <-read.csv(f)
    
    #merge
    consol <- readRDS(paste(path, "/consolidated.RDS", sep=""))
    consol <- gtools::smartbind(consol,dat)
    saveRDS(consol, paste(path, "/consolidated.RDS", sep=""))
    
  }
  
  dat <- readRDS(paste(path, "/consolidated.RDS", sep=""))[-1,-1] 
  
  dat %>%
    dplyr::rename(analyte = AnalyteName,
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
  
  dat %>%
    dplyr::mutate(id = dplyr::case_when(id==""~ NA_character_,
                          TRUE~id),
           InletComment = dplyr::case_when(InletComment==""~ NA_character_,
                                    TRUE~as.character(InletComment))) %>% 
    dplyr::filter(is.na(id)==F) %>%
    tidyr::separate(., analyte, c("analyte", "junk"), sep=" ") -> dat
  
  dat %>%
    dplyr::mutate(analyte = tolower(gsub("-", "", analyte))) %>%
    dplyr::select(., id, analyte, rfu_cv, conc_mean, gnr1_conc:gnr3_conc, gnr1_rfu:gnr3_rfu, InletComment, kitid) %>%
    dplyr::filter(grepl("HC|LC", id)==F)  -> sample
  
  # sample %>%
  #   dplyr::group_by(id, analyte) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L)
  # 
  tidyr::pivot_wider(sample, 
                     id_cols = "id",
                     names_from = "analyte",
                     values_from = rfu_cv:kitid,
                     names_glue = "{analyte}_{.value}") -> wide
  
  wide %>%
    dplyr::select(order(colnames(wide))) %>%
    dplyr::select(., id, contains("cv"), contains("conc_mean"), everything()) %>% 
    dplyr::relocate(dplyr::contains("InletComment"), dplyr::contains("kitid"), .after = last_col()) -> wide
  
  wide %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("cv")),
              list(~as.numeric(as.character(.)))) %>%
    dplyr::filter(., if_any(dplyr::contains("cv"), ~.>10)) %>%
    select(., id, dplyr::contains("cv"), dplyr::contains("gnr")) -> highcv

  
  if (file.exists(paste(path, "/consolidated", sep=""))){
    
    openxlsx::write.xlsx(list("for master" = wide,
                              "high cv only" = highcv), paste(path, "/consolidated/consolidated.xlsx", sep=""), rowNames=F, keepNA = TRUE)
    
    
  } else {
    
    dir.create(paste(path, "/consolidated", sep=""))
    openxlsx::write.xlsx(list("for master" = wide,
                              "high cv only" = highcv), paste(path, "/consolidated/consolidated.xlsx", sep=""), rowNames=F, keepNA = TRUE)
    
  }
  
  
  print("f h r c  |  done exporting consolidated file.")
  
}
