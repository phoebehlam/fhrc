#'endothelial function - consolidate and score
#'
#'@importFrom magrittr "%>%"
#'
#'
#'@export
#'
endo <- function(path, truebase0 = F) {
  
  widecon <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(widecon, paste(path, "/widecon.RDS", sep=""))
  
  longcon <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(longcon, paste(path, "/longcon.RDS", sep=""))
  
  filenames = list.files(path = path, pattern = '.csv', full.names=T, recursive=F)
  
  for (f in filenames) {
    print(f)
    dat <- read.csv(f)
    
    # dat <- read.csv("/Users/phoebelam/Desktop/endo/OTR4741V1_20220923135417_T_Detail.csv")

    dat %>% dplyr::rename (id = PatientID) -> dat
    
    # wide data extraction
    dat %>%
      dplyr::select (id:Decay.time.constant.2) %>%
      dplyr::filter (is.na(Rest.Diameter)==F) %>%
      dplyr::mutate(filename = basename(f))-> wide
    
    # dat %>%
    #   dplyr::select (id:Decay.time.constant.2) %>%
    #   dplyr::filter (is.na(Rest.Diameter)==F) -> wide
    
    # long data extraction
    dat %>%
      dplyr::select (id, Elapsed.time.sec., Diameter.mm., Rest.Diameter, Base.Diameter) %>%
      dplyr::rename (time = Elapsed.time.sec.,
                     diameter = Diameter.mm.) %>%
      tidyr::fill(Rest.Diameter:Base.Diameter) %>% 
      filter(is.na(time)==F & diameter !=0)-> long
      
    
    if(truebase0==T & nrow(long)!=0) {
      
      long %>%
        mutate(Base.Diameter = case_when(Base.Diameter == 0~ long$diameter[1],
                                         TRUE~ as.numeric(as.character(Base.Diameter)))) -> long

      wide %>%
        mutate(Base.Diameter = case_when(Base.Diameter == 0~ long$diameter[1],
                                         TRUE~ as.numeric(as.character(Base.Diameter))),
               X.FMD = 100*(Max.Diameter - Rest.Diameter)/Rest.Diameter, 
               X.FMD.b = 100*(Max.Diameter - Base.Diameter)/Base.Diameter) -> wide
    }
    
  
    #extract start and max times & only keep those
    if (as.numeric(dat$Dilation.start.time)[1] == 0) {
      
      long %>%
        mutate(diff = diameter - lag(diameter)) %>% 
        filter(diff != 0) %>% 
        slice(1L) -> temp

      
      if (nrow(temp)==0) {
        0 -> start
      } else {
        temp$time -> start
      }
      
    } else {
      as.numeric(dat$Dilation.start.time)[1] -> start
    }
    
    as.numeric(dat$at)[1] -> max
    
    long$diameter[2]-> check
    
    
    if (max == 0 & check != 0) {
        
        long %>%
          dplyr::mutate (time = round (time, 1),
                         diameter_rdiff = diameter - Rest.Diameter) %>%
          dplyr::filter (time >= 54.5 & time <= 65.4) %>%
          dplyr::mutate (mean.55to65 = mean(diameter)) %>% 
          dplyr::mutate(perc.55to65.rest = ((mean.55to65-Rest.Diameter)/Rest.Diameter)*100,
                        perc.55to65.base = ((mean.55to65-Base.Diameter)/Base.Diameter)*100,
                        auc.max.base = NA_real_,
                        auc.55to65.base = MESS::auc(time, diameter_rdiff, type = "linear")) %>%
          dplyr::select (., id, auc.max.base, auc.55to65.base, perc.55to65.rest, perc.55to65.base) %>%
          .[1, ] -> trim2
        
        merge (wide, trim2, by = "id", all=T) -> wide
        
      } else if (max == 0 & check == 0) {
        
        long %>%
          tidyr::fill(Rest.Diameter:Base.Diameter) %>% 
          dplyr::mutate(auc.max.base = NA_real_,
                        auc.55to65.base = NA_real_,
                        perc.55to65.rest= NA_real_,
                        perc.55to65.base=NA_real_) %>%
          dplyr::select (., id, auc.max.base, auc.55to65.base, perc.55to65.rest, perc.55to65.base) %>%
          .[1, ] -> trim2
        
        merge (wide, trim2, by = "id", all=T) -> wide
        
      }else {
        
        long %>%
          dplyr::mutate(diameter_bdiff = diameter - Base.Diameter,
                        diameter_rdiff = diameter - Rest.Diameter,
                        time = round (time, 1)) %>%
          dplyr::filter (time >= start & time <= max) %>% 
          dplyr::mutate (auc.max.base = MESS::auc (time, diameter_bdiff, type = "linear")) %>% 
          dplyr::select (id, auc.max.base) %>% 
          .[1, ] -> trim
        
        long %>%
          tidyr::fill(Rest.Diameter:Base.Diameter) %>% 
          dplyr::mutate (time = round (time, 1)) %>%
          dplyr::filter (time >= 54.5 & time <= 65.4) %>%
          dplyr::mutate (mean.55to65 = mean(diameter),
                         diameter_bdiff = diameter - Base.Diameter,
                         diameter_rdiff = diameter - Rest.Diameter) %>% 
          dplyr::mutate(perc.55to65.rest = ((mean.55to65-Rest.Diameter)/Rest.Diameter)*100,
                        perc.55to65.base = ((mean.55to65-Base.Diameter)/Base.Diameter)*100,
                        auc.55to65.base = MESS::auc (time, diameter_bdiff, type = "linear")) %>%
          dplyr::select (., id, perc.55to65.rest, perc.55to65.base, auc.55to65.base) %>%
          .[1, ] -> trim2
        
        merge (wide, trim, by = "id", all = T) -> wide1
        merge (wide1, trim2, by = "id", all = T) -> wide
      }
    
    
    #consolidating the wide dat
    widecon <- readRDS(paste(path, "/widecon.RDS", sep=""))
    widecon <- gtools::smartbind(widecon, wide)
    saveRDS(widecon, paste(path, "/widecon.RDS", sep=""))
    
    #consolidation the long dat
    longcon <- readRDS(paste(path, "/longcon.RDS", sep=""))
    longcon <- gtools::smartbind(longcon, long)
    saveRDS(longcon, paste(path, "/longcon.RDS", sep=""))
    
  }
  
  widecon <- readRDS(paste(path, "/widecon.RDS", sep=""))[-1, -1]
  longcon <- readRDS(paste(path, "/longcon.RDS", sep=""))[-1, -1]

  
  widecon %>%
    dplyr::rename (datetime = Measurement.start.time,
                  diameter.rest = Rest.Diameter,
                  diameter.max = Max.Diameter,
                  diameter.maxtime = at,
                  fmd.rest = X.FMD,
                  diameter.base = Base.Diameter,
                  fmd.base = X.FMD.b,
                  imt = bIMT,
                  dilation.starttime = Dilation.start.time,
                  dilation.duration = Dilation.time,
                  dilation.velocity = Max.dilation.velocity,
                  dilation.area = Dilation.area,
                  dilation.time.con = Dilation.time.constant,
                  flowrate.rest = Rest.Flow.Rate,
                  flowrate.max = Max.Flow.Rate,
                  flowrate.maxtime = at.1, 
                  flowrate.increase = Flow.Rate.augumentation,
                  flowrate.base = Baseline.Flow.Rate,
                  flowrate.estmax = Estimated.Max.Flow.Rate,
                  flowrate.estincrease = Estimated.Flow.Rate.augumentation,
                  flowrate.decayt.con = Decay.time.constant,
                  flowvol.rest = Rest.Flow.vol.,
                  flowvol.max = Max.Flow.vol.,
                  flowvol.maxtime = at.2,
                  flowvol.increase = Flow.vol..augumentation,
                  flowvol.base = Baseline.Flow.vol., 
                  flowvol.estmax = Estimated.Max.Flow.vol., 
                  flowvol.estincrease = Estimated.Flow.vol..augumentation,
                  flowvol.decayt.con = Decay.time.constant.1,
                  shear.rest = Rest.Shear.Rate,
                  shear.max = Max.Shear.Rate,
                  shear.maxtime = at.3, 
                  shear.increase = Shear.Rate.augumentation,
                  shear.base = Baseline.Shear.Rate,
                  shear.estmax = Estimated.Max.Shear.Rate,
                  shear.estincrease = Estimated.Shear.Rate.augumentation,
                  shear.decayt.con = Decay.time.constant.2) %>%
    dplyr::select(., filename, id, datetime:shear.decayt.con, auc.max.base, auc.55to65.base, perc.55to65.rest, perc.55to65.base) -> widecon
  
  if (file.exists(paste(path, "/consolidated", sep=""))){
    
    openxlsx::write.xlsx(widecon, paste(path, "/consolidated/Endothelial function wide data.xlsx", sep=""), rowNames=F)
    openxlsx::write.xlsx(longcon, paste(path, "/consolidated/Endothelial function long data.xlsx", sep=""), rowNames=F)
    
  } else {
    
    dir.create(paste(path, "/consolidated", sep=""))
    openxlsx::write.xlsx(widecon, paste(path, "/consolidated/Endothelial function wide data.xlsx", sep=""), rowNames=F)
    openxlsx::write.xlsx(longcon, paste(path, "/consolidated/Endothelial function long data.xlsx", sep=""), rowNames=F)
    
  }
  
  widecon <- readRDS(paste(path, "/widecon.RDS", sep=""))
  
  filter(widecon, Base.Diameter == 0) -> temp
  
  if(nrow(temp)>0){
    
    print ("f h r c  |  done consolidating and scoring endothelial data. a wide and a long format data exported.")
    
    print('the following files have base diameter = 0. please check if this is an exporting error. if not, please re-run using truebase0=T argument')
    print(temp$filename)
  } else{
    
    print ("f h r c  |  done consolidating and scoring endothelial data. a wide and a long format data exported.")
    
  }
  
  
 
}
