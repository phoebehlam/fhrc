#'aggregate .csv stoplight task files, 
#'
#'@importFrom magrittr "%>%"
#'
#'@examples stoplight("/Users/phoebelam/Desktop/stoplight")
#'
#'@export
stoplight <- function(path) {
  
  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, paste(path,"/consolidated.RDS", sep=""))
  
  filenames = list.files(path=path,pattern = ".xlsx" ,full.names= TRUE, recursive=FALSE)
  
  for (f in filenames) {
    
    print (f)
    
    dat <- openxlsx::read.xlsx(f, colNames=F)
    # dat <- openxlsx::read.xlsx("/Users/phoebelam/Desktop/stoplight/sid-1051-20181030T122406.xlsx",
    #                 colNames = F)
    
    id <- dat[1,1]
    dat [-c(1,2), ] %>%
      dplyr::mutate(id = id,
                    filename=basename(f)) %>%
      dplyr::select(., filename, id, everything()) -> dat
    
    consol <- readRDS(paste(path,"/consolidated.RDS", sep=""))
    consol <- gtools::smartbind(consol,dat)
    saveRDS(consol, paste(path,"/consolidated.RDS", sep=""))
  
  }
  
  print("f h r c  |  done consolidating data, exporting long data...")
  
  consol <- readRDS(paste(path,"/consolidated.RDS", sep=""))[-1, -1]
  
  # exporting
  if (file.exists(paste(path, "/consolidated", sep=""))){
    
    openxlsx::write.xlsx(consol, paste(path, "/consolidated/consolidated.xlsx", sep=""), rowNames=F)
    
  } else {
    
    dir.create(paste(path, "/consolidated", sep=""))
    openxlsx::write.xlsx(consol, paste(path, "/consolidated/consolidated.xlsx", sep=""), rowNames=F)
    
  }
  
  print("f h r c  |  data exported, please check your folder.")
  
  
  
}
