#' consolidate luminex output for mwmh v2 only
#'
#' grab mfi, extrapolated values, and cv from individual outputs
#' 
#' @param path the folder to which all the individual files are saved
#' @examples
#' luminex.v2 (path= "/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data")
#'
#' 
#' @importFrom magrittr "%>%"
#' @export
luminex.v2 <- function(path) {
  
  setwd(path)
  
  filenames = intersect(list.files(path=path,pattern = ".xlsx" ,full.names= TRUE, recursive=FALSE),
                        list.files(path=path, pattern = "MWMH" ,full.names= TRUE, recursive=FALSE))
  
  filenames <- grep("donotuse", filenames, inv=T, value=T)
  
  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, "consolidated.RDS")
  
  for (f in filenames) {
    
    print(f)
    
    #dilution factor
    # dil <- xlsx::read.xlsx("Luminex MWMH 103-110 V2 Culture Sups IL6 IL8 IL1b TNFa 11012019.xlsx",
    #                        sheetName = "Raw Data")
    # dil <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 131-139 V2 Culture Sups IL6 IL8 IL1b TNFa 10022019_edited.xlsx",
    #                          sheetName = "Raw Data")
    dil <- xlsx::read.xlsx(f, sheetName = "Raw Data")
    which(dil$NA. == "Dilution Factor")+1 -> start
    which(dil$xPONENT=="Analysis Types")-1 -> end
    dil[start:end,] %>%
      dplyr::select(xPONENT, NA.) %>%
      dplyr::rename(Sample = xPONENT,
                    dilution = NA.) %>%
      dplyr::distinct(Sample, .keep_all = T) %>%
      dplyr::filter(!(grepl("Standard|Background|CNTR|CTR|TM", Sample)==T)) -> dil
    
    
    #extrapolated values
    # exdat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 100 dilution repeats V2 and OTRV2 test Culture Sups IL6 IL8 IL1b TNFa 12032020.xlsx",
    #                          sheetName = "Avg Result", startRow = 2)
    # exdat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 50 dilution + CV repeats V2 Culture Sups IL6 IL8 IL1b TNFa 11122019_LH.xlsx",
    #                          sheetName = "Avg Result", startRow = 2)
    # exdat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 131-139 V2 Culture Sups IL6 IL8 IL1b TNFa 10022019_edited.xlsx",
    #                          sheetName = "Avg Result", startRow = 2)
    exdat <- xlsx::read.xlsx(f, sheetName = "Avg Result",startRow = 2)
    which(colnames(exdat)=="IL.8")->x
    exdat %>%
      dplyr::select(x-2, x-1,IL.8,IL.1b,IL.6,TNF.a, Sample) %>%
      dplyr::rename (ID=1, 
              ligand=2) %>%
      dplyr::mutate(ID = as.numeric(as.character(ID))) %>%
      dplyr::filter(is.na(ID) == FALSE) %>% 
      dplyr::select(Sample, ID,ligand,IL.8,IL.1b,IL.6,TNF.a) %>%
      dplyr::rename(il8.ext = IL.8,
             il1b.ext = IL.1b,
             il6.ext = IL.6,
             tnfa.ext = TNF.a) %>%
      dplyr::mutate(filename.ext=basename(f))->exdat
    
    #cv
    # cvdat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 131-139 V2 Culture Sups IL6 IL8 IL1b TNFa 10022019_edited.xlsx",
    #                          sheetName = "%CV Replicates", startRow = 2)
    
    cvdat <- xlsx::read.xlsx(f, sheetName = "%CV Replicates",startRow = 2)
    which(colnames(cvdat)=="IL.8")->x
    cvdat %>%
      dplyr::select(x-2, x-1,IL.8,IL.1b,IL.6,TNF.a, Sample) %>%
      dplyr::rename (ID=1, 
                     ligand=2,
                     il8.cv = IL.8,
                     il1b.cv = IL.1b,
                     il6.cv = IL.6,
                     tnfa.cv = TNF.a) %>%
      dplyr::mutate(ID = as.numeric(as.character(ID))) %>%
      dplyr::filter(is.na(ID) == FALSE) %>%
      dplyr::select(Sample, ID, ligand, il8.cv, il1b.cv, il6.cv, tnfa.cv) %>%
      dplyr::mutate(filename.cv=basename(f))-> cvdat
    
    #mfi values
    # mfidat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 100 dilution repeats V2 and OTRV2 test Culture Sups IL6 IL8 IL1b TNFa 12032020.xlsx",
    #                           sheetName = "Avg Net MFI", startRow = 2)
    # mfidat <- xlsx::read.xlsx("Luminex MWMH 103-110 V2 Culture Sups IL6 IL8 IL1b TNFa 11012019.xlsx",
    #                           sheetName = "Avg Net MFI", startRow = 2)
    # mfidat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 131-139 V2 Culture Sups IL6 IL8 IL1b TNFa 10022019_edited.xlsx",
    #                          sheetName = "Avg Net MFI", startRow = 2)
    
    mfidat <- xlsx::read.xlsx(f, sheetName = "Avg Net MFI",startRow = 2)
    mfidat %>%
      dplyr::select(Sample:TNF.a) %>%
      dplyr::filter(is.na(Sample) == FALSE & grepl("Standard", Sample)==F) %>% 
      dplyr::select(Sample,IL.8,IL.1b,IL.6,TNF.a) %>%
      dplyr::rename(il8.mfi = IL.8,
                    il1b.mfi = IL.1b,
                    il6.mfi = IL.6,
                    tnfa.mfi = TNF.a) %>%
      dplyr::mutate(filename.mfi=basename(f))->mfidat
    
    #merge
    temp <- merge(mfidat, exdat, by = "Sample", all = T)
    temp2 <- merge(temp, cvdat, by = "Sample", all = T)
    together<- merge(temp2, dil, by = "Sample", all.x = T)
    
    consol <- readRDS("consolidated.RDS")
    consol <- gtools::smartbind(consol,together)
    saveRDS(consol, "consolidated.RDS")

  }
}

luminex.v2 (path= "/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data")

# manual cleaning
library(dplyr)
library(janitor)
library(xlsx)

setwd("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex")
consol <- readRDS("Raw Data/MWMH V2 Culture Sup Raw Data/consolidated.RDS")[-1,-1]
consol %>% filter(is.na(Sample)==F & is.na(ID.x)==T)%>% View () #checked with lauren. these are all data we did not want (e.g., otr tests or control)
consol %>% filter(is.na(ID.x)==F) -> consol

#check if the ID and ligand columns are redundant. yup. remove one of them.
consol %>% filter(ID.x != ID.y | ligand.x != ligand.y)
consol %>% select(-c(ID.y, ligand.y)) %>%
  rename(id = ID.x,
         ligand = ligand.x) -> consol

#fixing idiosyncrasies in spelling
tabyl (consol$ligand, show_missing_levels = F)
consol$ligand %>%
  gsub("AGE BSA|AGEBSA", "AGE-BSA", .) %>%
  gsub("CRT 6", "CRT-6", .) %>%
  gsub("IL10 0.02", "IL10-0.02", .) %>%
  gsub("IL10 0.1", "IL10-0.1", .) -> consol$ligand

#checking diluton factor
tabyl (consol$dilution, show_missing_levels = F)
consol %>% filter(dilution == 1) %>% View () #lauren said if it's 1 (missing), it's meant to be 20
consol %>%
  mutate(dilution = case_when(dilution == 1~ 20,
                              TRUE~ as.numeric(as.character(dilution)))) -> consol

#remove all dilution factor 50 or 100 (because for these we want first round values)
consol %>% filter(dilution == 20) -> consol 

#now take care of remaining repeats, which should all be cv repeats
#check that cv and mfi came from same file
consol %>% filter(filename.mfi != filename.cv) #check.

#add a tag to wells that are repeated and and a tag 
consol %>%
  group_by(id, ligand) %>% 
  mutate (datdiff = n_distinct(filename.mfi)) %>%
  ungroup() %>%
  group_by(id, ligand, datdiff) %>%
  mutate(count = row_number()) %>%
  ungroup() %>%
  arrange(id,ligand)-> consol

# special cases from lauren:
# 131: mutichannel pipet error on first plate, or 131 we can pull from every file except 123-131 file (no problem here, same as cv run)
# 260 LPS: go with original for this one (need special handling)
consol %>% filter(id == 131) %>% select(id, ligand, filename.mfi, datdiff, count) %>% View ()

consol %>%
  mutate(grab = case_when(id == 260 & ligand == "LPS" & count == 1~ 1,
                          id == 260 & ligand == "LPS" & count == 2~ 0,
                          datdiff == 1~ 1,
                          datdiff == 2 & count == 2~ 1,
                          TRUE~0)) -> consol
tabyl (consol$datdiff)
tabyl (consol$grab) #perfect. half of the repeats.
consol %>% filter(id == 260) %>% View () #good, special case handled. 

consol %>%
  filter(grab == 1) -> consol

# missing unstim for 131
tabyl (consol$id) %>% filter (n!=10)
tabyl(consol$id, show_missing_levels = F, show_na = F) %>% nrow()

saveRDS(consol, "Consolidated Data/V2 consolidated data/consolidated.RDS")
