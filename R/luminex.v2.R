#' consolidate luminex output for mwmh v2 only
#'
#' grab mfi, extrapolated values, and cv from individual outputs
#' 
#' @param path the folder to which all the individual files are saved
#' @examples
#' luminex.v2 (path= "/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V1 Culture Sup Raw Data")
#'
#' 
#' @importFrom magrittr "%>%"
#' @export
luminex.v2 <- function(path) {
  
  filenames = intersect(list.files(path=path,pattern = ".xlsx" ,full.names= TRUE, recursive=FALSE),
                        list.files(path=path, pattern = "MWMH" ,full.names= TRUE, recursive=FALSE))
  
  filenames <- grep("donotuse", filenames, inv=T, value=T)
  
  consol <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(consol, "consolidated.RDS")
  
  for (f in filenames) {
    
    print(f)
    
    #extrapolated values
    # exdat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 100 dilution repeats V2 and OTRV2 test Culture Sups IL6 IL8 IL1b TNFa 12032020.xlsx",
    #                          sheetName = "Avg Result", startRow = 2)
    # exdat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 50 dilution + CV repeats V2 Culture Sups IL6 IL8 IL1b TNFa 11122019_LH.xlsx",
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
    # cvdat <- xlsx::read.xlsx("/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Wetlab/Immunoassays/Luminex/Raw Data/MWMH V2 Culture Sup Raw Data/Luminex MWMH 100 dilution repeats V2 and OTRV2 test Culture Sups IL6 IL8 IL1b TNFa 12032020.xlsx",
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
    together <- merge(temp, cvdat, by = "Sample", all = T)
    
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
consol <- readRDS("consolidated.RDS")[-1,-1]
consol %>% filter(is.na(Sample)==F & is.na(ID.x)==T) #checked with lauren. these are all data we did not want (e.g., otr tests or control)
consol %>% filter(is.na(ID.x)==F) -> consol

#check if the ID and ligand columns are redundant. yup. remove one of them.
consol %>% filter(ID.x != ID.y | ligand.x != ligand.y)
consol %>% select(-c(ID.y, ligand.y)) %>%
  rename(id = ID.x,
         ligand = ligand.x) -> consol

#checking id
tabyl (consol$id) %>% filter(n !=10) %>% View ()
tabyl(consol$id) %>% 
  filter (n>10) %>%
  as.data.frame() %>%
  rename(idfreq= 1) -> temp
consol %>%
  filter(id %in% temp$idfreq) %>%
  arrange(id, ligand)-> repeats

#create new variable that shows how many files we got cv and ext from
repeats %>%
  group_by(id, ligand) %>%
  mutate(ext_distinct=n_distinct(filename.ext),
         cv_distinct=n_distinct(filename.cv)) %>%
  ungroup()-> repeats

#repeated 3 times
repeats %>%
  filter(ext_distinct>2) %>% View()

#create new variable to ensure that the file for cv and data are the same filecv-filedat should equal 0
repeats %>%
  mutate(diff_cvdat=fromdat_diff-fromcvdat_diff) ->repeats
repeats %>% filter(diff_cvdat!= 0) #check.

#port in the list of dilution repeats from rachel
#hmm doesn't make sense

repeats %>% filter(grepl("repeat", filename.mfi, ignore.case=T)==F) %>%
  group_by(id) %>%
  add_count(id, name = "n_repeated") %>%
  ungroup() -> test

test %>% select(Sample, id, ligand, n_repeated) %>% View ()
tabyl (test$n_repeated)



#from repeats grab anything that has only 1 piece of data, 
#if there are 2 pieces and one is repeat, then grab repeats
#if 2 pieces of data and no repeats grab two pieces of data
#if there are 3 pieces of data grab all 3
repeats %>%
  mutate(repeatyn = case_when(grepl("Repeats", fromdat,ignore.case= TRUE) == TRUE & 
                                grepl("no repeats", fromdat,ignore.case= TRUE) ==FALSE~1,
                              TRUE~0) ) -> repeats

repeats %>%
  select(ID, ligand, IL.8, fromdat, repeatyn, fromdat_diff) %>% View()


repeats %>%
  group_by(ID, ligand) %>% 
  mutate(repeatyn_diff=n_distinct(repeatyn)) -> repeats


repeats %>%
  mutate(grab= case_when(fromdat_diff==1~ 1,
                         fromdat_diff==2 & repeatyn==1~1,
                         fromdat_diff==2 & repeatyn==0 & repeatyn_diff==1~1,
                         fromdat_diff==3~1)) ->repeats

View (repeats)

repeats %>%
  # filter (fromdat_diff==2 & repeatyn==0) %>% 
  select(ID, ligand, IL.8, fromdat, repeatyn, repeatyn_diff, fromdat_diff, grab) %>% View()

repeats %>%
  filter(grab==1) ->goodrepeat

tabyl(goodrepeat$ID) %>% View()

goodrepeat %>%
  filter(ID==131) -> onethirtyone

setwd('C:/Users/lch593/Box/FHRC/NIH R01 My World My Heart Study (MWMH)/Wetlab/Luminex/Raw Data')
write.xlsx(onethirtyone,"C:/Users/lch593/Box/FHRC/NIH R01 My World My Heart Study (MWMH)/Wetlab/Luminex/Raw Data")


#kick out anything with n>10 from consol to create goodconsol
consol %>%
  group_by (ID) %>%
  add_count(ID, name="ID_count") %>%
  ungroup () %>%
  filter (ID_count <= 10) %>%
  arrange(ID, ligand) -> goodconsol

merge(goodconsol,goodrepeat,all=TRUE)->finallong





#check and fish out
tabyl (finallong$ligand)

finallong %>%
  filter(ligand == "AGEBSA") %>% select (ID, fromdat) %>% View()



#create a trimmed dataset that only has the columns you want

finallong %>%
  select(ID, ligand, IL.8, IL.6, IL.1b,TNF.a, IL.8cv, IL.6cv, IL.1bcv, TNF.acv)->trim

#cleaning
#recode >1.58 to 1.57
trim %>%
  mutate(Il.8_cl= case_when(IL.8== "< 1.58"~1.57,
                            TRUE~IL.8))

#tabyl each of the clean to check



#facor-->character-->numeric, cannot do factor-->numeric
final %>%
  mutate(ligand=as.character(ligand),
         IL.8_cl=as.numeric(as.character(IL.8)),
         IL.1b=as.character(IL.1b),
         IL.6=as.character(IL.6),
         TNF.a=as.character(TNF.a),
         IL.8cv=as.character(IL.8cv),
         IL.1bcv=as.character(IL.1bcv),
         IL.6cv=as.character(IL.6cv),
         TNF.acv=as.character(TNF.acv))->final


pivot_wider(final, names_from= 'ligand', values_from = c("IL.8", "IL.1b", "IL.6","TNF.a", "IL.8cv","IL.1bcv",
                                                         "IL.6cv", "TNF.acv"))->wide

setwd('C:/Users/lch593/Box/FHRC/NIH R01 My World My Heart Study (MWMH)/Wetlab/Luminex/Raw Data')

write.xlsx(wide, "wide.xlsx")

write.xlsx(wide, "wide.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = FALSE)


tabyl (final$ligand)

gsub("IL10 0.02", "IL10-0.02", final$ligand) -> final$ligand
gsub("IL10 0.1", "IL10-0.1", consol$ligand) -> final$ligand
gsub("IL-10-0.5", "IL10-0.5", consol$ligand) -> consol$ligand
gsub("IL-10 0.02", "IL10-0.02", consol$ligand) -> consol$ligand
gsub("IL-10 0.1", "IL10-0.1", consol$ligand) -> consol$ligand
gsub("IL-10 0.5", "IL10-0.5", consol$ligand) -> consol$ligand
gsub("AGE-BSA", "AGEBSA", consol$ligand) -> consol$ligand

