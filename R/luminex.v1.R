#' consolidate luminex output
#'
#' grab mfi, extrapolated values, and cv from individual outputs
#'
#' @param path the folder to which all the individual files are saved
#' @examples
#' luminex (path= "/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V1 Culture Sup Raw Data")
#'
#' @importFrom magrittr "%>%"
#' @export
luminex <- function (path) {

  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, "consolidated.RDS")

  setwd(path)
  filenames = intersect(list.files(path=path,
                                   pattern = ".xlsx" ,full.names= TRUE, recursive=FALSE),
                        list.files(path=path,
                                   pattern = "MWMH" ,full.names= TRUE, recursive=FALSE))

  for (f in filenames){
    print(f)

    #mfi
    mfidat <- xlsx::read.xlsx(f, sheetName = "Avg Net MFI",startRow = 2)
    mfidat %>%
      dplyr::select(Sample,IL.8,IL.1b,IL.6,TNF.a) %>%
      dplyr::filter(grepl("_",Sample)) %>%
      dplyr::mutate (filename = basename(f)) %>%
      dplyr::rename (il8.mfi = IL.8,
                     il1b.mfi = IL.1b,
                     il6.mfi = IL.6,
                     tnfa.mfi = TNF.a)-> mfidat

    #extrapolated values
    exdat <- xlsx::read.xlsx(f, sheetName = "Avg Result",startRow = 2)
    exdat %>%
      dplyr::select(Sample,IL.8,IL.1b,IL.6,TNF.a) %>%
      dplyr::filter(grepl("_",Sample)) %>%
      dplyr::rename (il8.ext = IL.8,
                     il1b.ext = IL.1b,
                     il6.ext = IL.6,
                     tnfa.ext = TNF.a)-> exdat

    #cv
    cvdat <- xlsx::read.xlsx(f, sheetName = "%CV Replicates",startRow = 2)
    cvdat %>%
      dplyr::select(Sample,IL.8,IL.1b,IL.6,TNF.a) %>%
      dplyr::rename (il8.cv = IL.8,
                     il1b.cv = IL.1b,
                     il6.cv = IL.6,
                     tnfa.cv = TNF.a) %>%
      dplyr::filter(grepl("_",Sample))-> cvdat

    #merge
    temp <- merge(mfidat, exdat, by = "Sample", all = T)
    together <- merge(temp, cvdat, by = "Sample", all = T)

    #basic cleaning
    together %>%
      tidyr::separate(Sample,c('ID','ligand','Repeat'),sep ='_',remove = FALSE) %>%
      dplyr::select(-Sample)-> together

    consol <- readRDS("consolidated.RDS")
    consol <- gtools::smartbind(consol,together)
    saveRDS(consol, "consolidated.RDS")
  }

  print("f h r c  |  done consolidating.")

}


# manual cleaning, too many specifics
library(dplyr)
library(janitor)
library(xlsx)
setwd("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V1 Culture Sup Raw Data")
consol <- readRDS("consolidated.RDS")[-1,-1]

#removing pilot subjects and kids with asthma
consol %>%
  filter(ID>=103 & !ID %in% c(310, 203)) ->consol

#fix idiosyncrasies in ligand naming
consol$ligand %>%
  gsub("IL-10 ", "IL10-", .) %>%
  gsub("IL-10-", "IL10-", .) %>%
  gsub("AGE-BSA", "AGEBSA", .) -> consol$ligand

# dealing with repeats
# 4.9.20 update. because we are now going to use mfi values, we prefer to use the first runs 1:20 dilutions
# so ones that repeated bc of dilutions, we want to use first round
# for ones that repeated bc of cv, we want to use second round

# rachel provided lists of repeats, porting this info in
drep <- read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/OTHER EXCEL FILES/mwmh_v1_list of repeats from rachel.xlsx",
                 sheetIndex = 2)[, 1]
as.character(drep) -> drep

cvrep <- read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/OTHER EXCEL FILES/mwmh_v1_list of repeats from rachel.xlsx",
                  sheetIndex = 1, startRow = 2)[, 1]
as.character(cvrep) -> cvrep

consol %>%
  mutate(id_ligand = paste(ID, ligand, sep="_")) -> consol

#if it's in the drep list, then take original and remove the repeat
#several checks first.
consol %>%
  filter(id_ligand %in% drep) -> temp
drep[!drep %in% temp$id_ligand] #okay, these are all because of pilots and asthma removed
consol %>%
  filter(id_ligand %in% drep & Repeat == "R") -> temp
drep[!drep %in% temp$id_ligand] %>% grep("102|310", ., invert=T, value = T) #these folks don't have repeats. checking with rachel.

#actually rachel's list was not exhaustive. 
#e.g., her list for 104, only has lps and il10, but in reality lps, crt, il10 all got repeated, which makes sense.
#so. new rule:
consol %>%
  mutate (dilute_rachel = case_when(id_ligand %in% drep~ 1)) %>%
  group_by(ID) %>%
  mutate (dilute_keep = case_when(any(dilute_rachel==1, na.rm=T) & Repeat=="R"~ 0,
                                  TRUE~1)) %>%
  ungroup()-> consol

#check the ones in drep. 
substr(drep, 1,3) %>% unique(.) -> id_drep
consol %>% filter (ID %in% id_drep) %>% select(ID, ligand, Repeat, dilute_rachel, dilute_keep) %>% arrange(ID, dilute_keep) %>% View ()
#check the ones not in drep.
consol %>% filter (!ID %in% id_drep) %>% select(ID, ligand, Repeat, dilute_rachel, dilute_keep) %>% arrange(ID, dilute_keep) -> temp
tabyl (temp$dilute_keep) #good.

#ok, now exclude. 
consol %>% filter (dilute_keep == 1) -> consol

#all remaining repeats we want the last round
consol %>%
  group_by(ID) %>%
  add_count(ID) %>%
  ungroup() %>%
  arrange(ID, ligand) %>%
  mutate (averagecv = rowMeans(select(., il8.cv, il1b.cv, il6.cv, tnfa.cv), na.rm=T))-> consol

consol %>%
  group_by (ID) %>%
  mutate (tag = case_when(duplicated(ligand, fromLast=T)==T~ 1),
          tag2 = case_when(duplicated(ligand)==T~ 1)) %>%
  mutate (origrepeat = case_when(is.na(tag)==F~ tag,
                                 is.na(tag2)==F~ tag2)) %>%
  select (-c(tag, tag2)) %>%
  ungroup()-> consol

consol %>%
  group_by (ID, ligand) %>%
  mutate (origrepeat_n = sum(origrepeat)) %>%
  ungroup() %>%
  group_by (ID) %>%
  mutate (diff = case_when(origrepeat == 1~ averagecv - lag(averagecv))) ->consol

#manual check which row we want for the ones with 3 repeats
consol %>%
  filter(origrepeat_n>2)%>% View()

consol %>%
  filter(origrepeat_n==2)%>% View()

consol %>%
  group_by(ID,ligand) %>%
  mutate(repeatnum=case_when(origrepeat_n>2~row_number()))%>%
  ungroup() ->consol

#check that they are on rachel's cv list
#all remaining repeats should be part of cv
consol %>%
  filter(id_ligand %in% cvrep & Repeat == "R")-> temp
cvrep[!cvrep %in% temp$id_ligand] #116crt6, 142lps,272il10.1, 292il10.1 are in both cv and dilute. 203r848 had asthma removed.

#now exclude originals and keep only the last ones
consol %>%
  mutate(keep=case_when(is.na(origrepeat_n)==TRUE~1,
                        origrepeat_n==2 & Repeat=='R'~1,
                        origrepeat_n==3 & repeatnum==3~1))->consol

#keeping the good repeats and no repeats
consol %>%
  filter(keep==1) %>%
  select(ID:tnfa.mfi, il8.ext:tnfa.cv, filename)->consol

colnames(consol) <- tolower(colnames(consol))

setwd("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/")
write.csv(consol, "Consolidated Data/V1 consolidated data/mwmh_v1_prescoreddata_4.10.20.csv", row.names= FALSE)
saveRDS(consol, "Consolidated Data/V1 consolidated data/mwmh_v1_prescored data_4.10.20.RDS")




