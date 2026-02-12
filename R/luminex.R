#'aggregate luminex output, compute mfi cv's, 
#'
#'@importFrom magrittr "%>%"
#'
#'@examples luminex("/Users/phoebelam/Desktop/luminex")
#'
#'@export
luminex <- function (path) {
  
  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, "consolidated.RDS")
  
  filenames = list.files(path=path,pattern = ".xlsx" ,full.names= TRUE, recursive=FALSE)
  
  for (f in filenames) {
    
    print(f)
    
    dat <- xlsx::read.xlsx(f, sheetIndex=1, na = "NaN")
    
    # mfi
    which(dat$xPONENT == "Net MFI") + 2-> start
    which(dat$xPONENT == "Count") -1 -> end
    dat[start:end, 1:5] -> mfi
    
    colnames(mfi) <-c("location", "id", "il1b", "il6", "tnfa")
    
    mfi %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==F) %>% 
      dplyr::group_by(id) %>%
      dplyr::mutate(dup = dplyr::row_number()) %>%
      dplyr::ungroup() -> mfi
    
    tidyr::pivot_wider(dplyr::select(mfi, id, il1b:tnfa, dup), 
                names_from = dup,
                values_from = c(il1b, il6, tnfa)) %>%
      dplyr::rename_at(vars(il1b_1:tnfa_2),
                list(~paste(., "_mfi", sep=""))) -> mfi
    
    # mfi cv (computed)
    sd.p=function(x){sd(x, na.rm=T)*sqrt((length(x)-1)/length(x))}
    cv.p=function(x){
      
      if(is.na(sd.p(x))==T) {
        return(NA_real_)
      } else if(sd.p(x)==0) {
        return(0)
      } else {100*sd.p(x)/mean(x, na.rm=T)}
    }
    
    mfi %>%
      dplyr::mutate_at(vars(il1b_1_mfi:tnfa_2_mfi),
                list(~as.numeric(as.character(.)))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(il1b_mfi_cv = abs(round(cv.p(c(il1b_1_mfi, il1b_2_mfi)),5)),
             il6_mfi_cv = abs(round(cv.p(c(il6_1_mfi, il6_2_mfi)),5)),
             tnfa_mfi_cv = abs(round(cv.p(c(tnfa_1_mfi, tnfa_2_mfi)),5))) %>% 
      dplyr::ungroup() -> mfi
    
    # concentration
    which(dat$xPONENT == "Result") + 2-> start
    which(dat$xPONENT == "Range") -1 -> end
    dat[start:end, 1:5] -> conc
    
    colnames(conc) <-c("location", "id", "il1b", "il6", "tnfa")
    
    conc %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==F) %>% 
      dplyr::group_by(id) %>%
      dplyr::mutate(dup = dplyr::row_number()) %>%
      dplyr::ungroup() -> conc
    
    tidyr::pivot_wider(dplyr::select(conc, id, il1b:tnfa, dup), 
                names_from = dup,
                values_from = c(il1b, il1b, il6, tnfa)) %>%
      dplyr::rename_at(vars(il1b_1:tnfa_2),
                list(~paste(., "_conc", sep=""))) -> conc
    
    
    # concentration cv
    which(dat$xPONENT == "%CV Replicates") + 2-> start
    which(dat$xPONENT == "% Recovery") -1 -> end
    dat[start:end, 1:4] -> cv
    
    colnames(cv) <-c("id", "il1b", "il6", "tnfa")
    
    cv %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==F) -> cv
    
    cv %>%
      dplyr::rename_at(vars(il1b:tnfa),
                list(~paste(., "_conc_cv", sep=""))) -> cv
    
    # consolidating mean results
    # mfi mean
    which(dat$xPONENT == "Avg Net MFI") + 2-> start
    which(dat$xPONENT == "Avg Result") -1 -> end
    dat[start:end, 1:4] -> mfim
    
    colnames(mfim) <-c("id", "il1b", "il6", "tnfa")
    
    mfim %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==F) %>% 
      dplyr::rename_at(vars(il1b:tnfa),
                list(~paste(., "_avg_mfi", sep=""))) -> mfim
    
    # conc mean
    which(dat$xPONENT == "Avg Result") + 2-> start
    which(dat$xPONENT == "Avg Range") -1 -> end
    dat[start:end, 1:4] -> concm
    
    colnames(concm) <-c("id", "il1b", "il6", "tnfa")
    
    concm %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==F) %>% 
      dplyr::rename_at(vars(il1b:tnfa),
                list(~paste(., "_avg_conc", sep=""))) -> concm
    
    # merge them together
    pecan::mergethem(mfi, conc, mfim, concm, cv) %>%
      dplyr::select(., id, contains("il1b"), contains("il6"), contains("tnfa")) -> together
    
    together %>%
      dplyr::mutate(plate = basename(f)) -> together
    
    
    # consolidate
    consol <- readRDS("consolidated.RDS")
    consol <- gtools::smartbind(consol,together)
    saveRDS(consol, "consolidated.RDS")
    
    
  }
  
  
  
  print("f h r c  |  done consolidating samples, consolidating standards/controls...")
  

  
  stanconsol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(stanconsol, "stanconsolidated.RDS")
  
  filenames = list.files(path=path,pattern = ".xlsx" ,full.names= TRUE, recursive=FALSE)
  
  for (f in filenames) {
    
    print(f)
    
    dat <- xlsx::read.xlsx(f, sheetIndex=1, na = "NaN")
    
    
    # mfi standard + controls
    which(dat$xPONENT == "Net MFI") + 2-> start
    which(dat$xPONENT == "Count") -1 -> end
    dat[start:end, 1:5] -> mfi
    
    colnames(mfi) <-c("location", "id", "il1b", "il6", "tnfa")
    
    mfi %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==T) %>% 
      dplyr::group_by(id) %>%
      dplyr::mutate(dup = dplyr::row_number()) %>%
      dplyr::ungroup() -> mfi
    
    tidyr::pivot_wider(dplyr::select(mfi, id, il1b:tnfa, dup), 
                       names_from = dup,
                       values_from = c(il1b, il6, tnfa)) %>%
      dplyr::rename_at(vars(il1b_1:tnfa_2),
                       list(~paste(., "_mfi", sep=""))) -> mfi
    
    # mfi cv (computed)
    mfi %>%
      dplyr::mutate_at(vars(il1b_1_mfi:tnfa_2_mfi),
                       list(~as.numeric(as.character(.)))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(il1b_mfi_cv = abs(round(cv.p(c(il1b_1_mfi, il1b_2_mfi)),5)),
                    il6_mfi_cv =  abs(round(cv.p(c(il6_1_mfi, il6_2_mfi)),5)),
                    tnfa_mfi_cv = abs(round(cv.p(c(tnfa_1_mfi, tnfa_2_mfi)),5))) %>% 
      dplyr::ungroup() -> mfi
    
    # concentration
    which(dat$xPONENT == "Result") + 2-> start
    which(dat$xPONENT == "Range") -1 -> end
    dat[start:end, 1:5] -> conc
    
    colnames(conc) <-c("location", "id", "il1b", "il6", "tnfa")
    
    conc %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==T) %>% 
      dplyr::group_by(id) %>%
      dplyr::mutate(dup = dplyr::row_number()) %>%
      dplyr::ungroup() -> conc
    
    tidyr::pivot_wider(dplyr::select(conc, id, il1b:tnfa, dup), 
                       names_from = dup,
                       values_from = c(il1b, il6, tnfa)) %>%
      dplyr::rename_at(dplyr::vars(il1b_1:tnfa_2),
                       list(~paste(., "_conc", sep=""))) -> conc
    
    # cv
    which(dat$xPONENT == "%CV Replicates") + 2-> start
    which(dat$xPONENT == "% Recovery") -1 -> end
    dat[start:end, 1:4] -> cv
    
    colnames(cv) <-c("id", "il1b", "il6", "tnfa")
    
    cv %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==T) -> cv
    
    cv %>%
      dplyr::rename_at(dplyr::vars(il1b:tnfa),
                       list(~paste(., "_conc_cv", sep=""))) -> cv
    
    # consolidating mean results
    # mfi mean
    which(dat$xPONENT == "Avg Net MFI") + 2-> start
    which(dat$xPONENT == "Avg Result") -1 -> end
    dat[start:end, 1:4] -> mfim
    
    colnames(mfim) <-c("id", "il1b", "il6", "tnfa")
    
    mfim %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==T) %>% 
      dplyr::rename_at(vars(il1b:tnfa),
                       list(~paste(., "_avg_mfi", sep=""))) -> mfim
    
    # conc mean
    which(dat$xPONENT == "Avg Result") + 2-> start
    which(dat$xPONENT == "Avg Range") -1 -> end
    dat[start:end, 1:4] -> concm
    
    colnames(concm) <-c("id", "il1b", "il6", "tnfa")
    
    concm %>%
      dplyr::arrange(id) %>% 
      dplyr::filter(grepl("background|control|standard", id, ignore.case=T)==T) %>% 
      dplyr::rename_at(dplyr::vars(il1b:tnfa),
                       list(~paste(., "_avg_conc", sep=""))) -> concm
    
    # merge them together
    pecan::mergethem(mfi, conc, mfim, concm, cv) %>%
      dplyr::select(., id, contains("il1b"), contains("il6"), contains("tnfa")) -> together
    
    together %>%
      dplyr::mutate(plate = basename(f)) -> together
    
    # together %>%
    #   mutate(plate = NA_character_) -> together
    
    # consolidate
    stanconsol <- readRDS("stanconsolidated.RDS")
    stanconsol <- gtools::smartbind(stanconsol,together)
    saveRDS(stanconsol, "stanconsolidated.RDS")
    
    
  }
  
  print("f h r c  |  done consolidating standards/controls. compiling exports...")
  
  consol <- readRDS("consolidated.RDS")[-1, -1]
  
  consol %>%
    dplyr::mutate_all(~gsub("NaN|#N/A|N/A", "", .)) %>%
    dplyr::mutate_at(dplyr::vars(il1b_conc_cv, il6_conc_cv, tnfa_conc_cv,
                          il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv),
                     list(~as.numeric(as.character(.)))) -> consol
  
  # consol %>%
  #   mutate(plate = NA_character_) -> consol
  
  consol %>%
    dplyr::select(., id,
                  il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv,
                  il1b_1_mfi,il1b_2_mfi,il1b_1_conc,il1b_2_conc,il1b_avg_mfi,
                  il1b_avg_conc,
                  il6_1_mfi,il6_2_mfi,il6_1_conc,il6_2_conc,il6_avg_mfi,
                  il6_avg_conc, tnfa_1_mfi,tnfa_2_mfi,tnfa_1_conc,
                  tnfa_2_conc,tnfa_avg_mfi,tnfa_avg_conc,plate) -> consol
  
  # getting a sheet with cv's > 10
  consol %>%
    dplyr::filter(if_any(c(il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv), ~ abs(.)> 10)) %>% 
    dplyr::select(., id, dplyr::contains("mfi"), dplyr::contains("_conc"), -dplyr::contains("avg"), plate) %>% 
    dplyr::select(., id, dplyr::contains("mfi_cv"), dplyr::everything()) %>%
    dplyr::mutate(us = case_when(grepl("US", id)==T~ 1,
                                 TRUE~0)) %>%
    mutate_at(dplyr::vars(il1b_mfi_cv:tnfa_2_mfi),
              list(~as.numeric(as.character(.))))-> highcv
  
  highcv %>%
    dplyr::mutate(`repeat US as well`=NA_character_,
                  decision=NA_character_,
                  `reason for repeat` = NA_character_,
                  `decision made by` = NA_character_) %>%
    dplyr::select(., id, `repeat US as well`, decision, `reason for repeat`, `decision made by`,
                  il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv,
                  il1b_1_mfi,il1b_2_mfi,il1b_1_conc,il1b_2_conc,
                  il6_1_mfi,il6_2_mfi,il6_1_conc,il6_2_conc,
                  tnfa_1_mfi,tnfa_2_mfi,tnfa_1_conc, tnfa_2_conc, plate, us) -> highcv
  
  
  # getting standards and controls data
  stanconsol <- readRDS("stanconsolidated.RDS")[-1, -1]
  
  stanconsol %>%
    dplyr::mutate_all(~gsub("NaN|#N/A|N/A", "", .)) %>%
    dplyr::mutate_at(vars(il1b_conc_cv, il6_conc_cv, tnfa_conc_cv,
                          il1b_mfi_cv, il6_mfi_cv, tnfa_mfi_cv),
                     list(~as.numeric(as.character(.)))) -> stanconsol
  
  # stanconsol %>% mutate(plate = NA_character_) -> stanconsol
  
  stanconsol %>%
    dplyr::select(., id,il1b_1_mfi,il1b_2_mfi,il1b_1_conc,il1b_2_conc,il1b_avg_mfi,il1b_avg_conc,
                  il1b_mfi_cv,il1b_conc_cv,il6_1_mfi,il6_2_mfi,il6_1_conc,il6_2_conc,il6_avg_mfi,il6_avg_conc,
                  il6_mfi_cv,il6_conc_cv,tnfa_1_mfi,tnfa_2_mfi,tnfa_1_conc,tnfa_2_conc,tnfa_avg_mfi,tnfa_avg_conc,tnfa_mfi_cv,tnfa_conc_cv,plate) -> stanconsol
  
  
  if (file.exists(paste(path, "/consolidated", sep=""))){
    
    openxlsx::write.xlsx(list("for master" = consol,
                              "high cv only" = highcv,
                              "standards & controls" = stanconsol), paste(path, "/consolidated/consolidated.xlsx", sep=""), rowNames=F)
    
   
  } else {
    
    dir.create(paste(path, "/consolidated", sep=""))
    openxlsx::write.xlsx(list("for master" = consol,
                              "high cv only" = highcv,
                              "standards & controls" = stanconsol), paste(path, "/consolidated/consolidated.xlsx", sep=""), rowNames=F)
    
  }
  
  
  print("f h r c  |  done exporting consolidated file.")
  
  
}
