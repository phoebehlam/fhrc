#'aggregate ella output, 1-plex
#'
#'@importFrom magrittr "%>%"
#'
#'@examples ella1("/Users/phoebelam/Desktop/spah")
#'
#'@export
ella1 <- function (path) {
  
  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, paste(path, "/consolidated.RDS", sep=""))
  
  filenames = list.files(path=path, pattern = ".csv" ,full.names= TRUE, recursive=FALSE)
  
  for (f in filenames){
    print(f)
    
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
  
  final <- readRDS(paste(path, "/consolidated.RDS", sep=""))[-1,-1]
  write.csv(final, paste(path, "/ella_1plex.csv", sep=""), row.names=F)
  
  print("done")
}

 
 
 
 
 
 
 

