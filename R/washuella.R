#' consolidate ella output for washu
#'
#' @param path the folder to which all the individual files are saved
#' 
#' @importFrom magrittr "%>%"
#do not export
washuella <- function (path) {

  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, paste(path, "/consolidated.RDS", sep=""))

  filenames = list.files(path=path, pattern = ".xlsx" ,full.names= TRUE, recursive=FALSE)

  for (f in filenames){
    print(f)
    
    #grab plate number too!!
    
    #gnr results
    gnr <- xlsx::read.xlsx(f, sheetIndex =1, startRow=61, header =F)
    # gnr <- xlsx::read.xlsx("/Users/phoebelam/Desktop/Plate 6 Data.xlsx", sheetIndex =1, startRow=61, header=F)
    
    colnames(gnr) <- c("inlet", "id", 
                       "rfu1_il10", "conc1_il10", 
                       "rfu2_il10", "conc2_il10",
                       "rfu3_il10", "conc3_il10",
                       "rfu1_il6", "conc1_il6", 
                       "rfu2_il6", "conc2_il6",
                       "rfu3_il6", "conc3_il6",
                       "rfu1_il8", "conc1_il8", 
                       "rfu2_il8", "conc2_il8",
                       "rfu3_il8", "conc3_il8",
                       "rfu1_tnfa", "conc1_tnfa", 
                       "rfu2_tnfa", "conc2_tnfa",
                       "rfu3_tnfa", "conc3_tnfa")
    
    gnr[1:32, ] %>%
      dplyr::filter(!id %in% c("High Control", "Low Control")) -> gnr
    
    #mean results
    # dat <- xlsx::read.xlsx("/Users/phoebelam/Desktop/Plate 6 Data.xlsx", sheetIndex =1, startRow=22, header=F)
    dat <- xlsx::read.xlsx(f, sheetIndex =1, startRow=22, header=F)
    
    colnames(dat) <- c("inlet", "id",
                       "gnr_il10", "rfu_mean_il10", "rfu_cv_il10", "conc_mean_il10", "conc_cv_il10",
                       "gnr_il6", "rfu_mean_il6", "rfu_cv_il6", "conc_mean_il6", "conc_cv_il6",
                       "gnr_il8", "rfu_mean_il8", "rfu_cv_il8", "conc_mean_il8", "conc_cv_il8",
                       "gnr_tnfa", "rfu_mean_tnfa", "rfu_cv_tnfa", "conc_mean_tnfa", "conc_cv_tnfa")
    
    dat[1:32,] %>%
      dplyr::select(., inlet:conc_cv_tnfa) %>%
      dplyr::filter(!id %in% c("High Control", "Low Control")) -> dat
    

    #merge
    temp <- merge(dat, gnr, by = "id", all = T)

    consol <- readRDS(paste(path, "/consolidated.RDS", sep=""))
    consol <- gtools::smartbind(consol,temp)
    saveRDS(consol, paste(path, "/consolidated.RDS", sep=""))
  }
  
  final <- readRDS(paste(path, "/consolidated.RDS", sep=""))[-1,-1]
  write.cvs(final, paste(path, "/washu_ella.csv", sep=""), row.names=F)
  
  print("f h r c  |  done consolidating.")

}






