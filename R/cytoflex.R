#'consolidate cytoflex data 
#'
#'@importFrom magrittr "%>%"
#'
#'@examples cytoflex("/Users/phoebelam/Desktop/cytoflex", type='emp')
#'
#'@export
cytoflex <- function(path, type = c('emp', 'mono')) {
  
  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, "consolidated.RDS")
  
  # path = "/Users/phoebelam/Library/CloudStorage/OneDrive-NorthwesternUniversity/Documents/NIH R01 On the Rise (OTR)/Wetlab/Flow Cytometry/Cytoflex/EMP/EMP CSV Export/V1"
  
  filenames = list.files(path=path,pattern = ".csv" ,full.names= TRUE, recursive=FALSE)
  
  for (f in filenames) {
    
    print(f)
    
    dat <- read.csv(f, na.strings="", skip=2) 
    
    # dat<-read.csv("/Users/phoebelam/Library/CloudStorage/OneDrive-NorthwesternUniversity/Documents/NIH R01 On the Rise (OTR)/Wetlab/Flow Cytometry/Cytoflex/EMP/EMP CSV Export/V1/EMP OTR 3601 V1 03232021.csv",
    #               na.strings="", skip=2)
    # dat<-read.csv("/Users/phoebelam/Library/CloudStorage/OneDrive-NorthwesternUniversity/Documents/NIH R01 On the Rise (OTR)/Wetlab/Flow Cytometry/Cytoflex/EMP/EMP CSV Export/V1/EMP OTR 4831 V1 10272022.csv",
    #               na.strings="", skip=2)
    # dat<-read.csv("/Users/phoebelam/Library/CloudStorage/OneDrive-NorthwesternUniversity/Documents/NIH R01 On the Rise (OTR)/Wetlab/Flow Cytometry/Cytoflex/Mono Sub/OTR Mono Sub CSV Exports/OTR V1/Mono Sub OTR 3561 V1 03122021.csv",
    #               na.strings="", skip=2)
    
    # dat <- read.csv('/Users/phoebelam/Library/CloudStorage/OneDrive-NorthwesternUniversity/Documents/NIH R01 On the Rise (OTR)/Wetlab/Flow Cytometry/Cytoflex/Mono Sub/OTR Mono Sub CSV Exports/OTR V2/Mono Sub OTR 4321 V2 07102023.csv',
    #                 na.strings = "", skip=2)
  
    
    if (type == 'emp'){
      
      substr(basename(f), 9, 12) -> id
      as.numeric(dat %>% dplyr::filter_all(dplyr::any_vars(. %in% 'CD31 CD42')) %>%
                   select(Q1.UL.Events)) -> cd31cd42
      as.numeric(dat %>% dplyr::filter_all(dplyr::any_vars(. %in% 'CD62E')) %>%
                   select(Q1.UL.Events)) -> cd62
      as.numeric(dat %>% dplyr::filter_all(dplyr::any_vars(. %in% 'Annexin CD31')) %>%
                   select(Q1.UR.Events)) -> cd31
      
      new <- data.frame(id = id,
                        cd31pcd42n = cd31cd42,
                        cd62e = cd62,
                        cd31ppsp = cd31)
      
    } else if(type == 'mono') {
      
      
      substr(basename(f), 14, 17) -> id
      dat$Volume.μL..[1] -> `vol`
      dat$P3.Events[1] -> p3
      dat$P7.Events[1] -> p7
      dat$P6.Events[1] -> p6
      
      if (length(p3)==0) {
        dat$CD45..Events[1] -> p3
        dat$Classical.Events[1] -> p7
        dat$Non.classical.Events[1] -> p6
      }
      
      if(length(vol)==0) {
        vol <- "n/a"
      }
      
      if(length(p3)==0) {
        p3 <- "n/a"
      }
      
      if(length(p7)==0) {
        p7 <- "n/a"
      }
      
      if(length(p6)==0) {
        p6 <- "n/a"
      }
      
      if(length(vol)==0) {
        p6 <- "n/a"
      }
      
      
      new <- data.frame(id = id,
                        volume = vol,
                        cd45p = p3,
                        cd14ppcd16n = p7,
                        cd14pcd16pp = p6)
    }
      
    # consolidate
    consol <- readRDS("consolidated.RDS")
    consol <- gtools::smartbind(consol,new)
    saveRDS(consol, "consolidated.RDS")

  }
  
  consol <- readRDS("consolidated.RDS")[-1, -1]
  
  if (file.exists(paste(path, "/consolidated", sep=""))){
    openxlsx::write.xlsx(consol, paste(path, "/consolidated/consolidated.xlsx", sep=""), rowNames=F)
  }else {
    dir.create(paste(path, "/consolidated", sep=""))
    openxlsx::write.xlsx(consol, paste(path, "/consolidated/consolidated.xlsx", sep=""), rowNames=F)
  }
  
  print("f h r c  |  done consolidating flow data...")
  
  
  
}
