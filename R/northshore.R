# library(dplyr)
# library(tidyr)
# library(xlsx)
# library(janitor)
# 
# dat <-read.xlsx("/Users/phoebelam/Northwestern University/FHRC - Documents/NIH R01 On the Rise (OTR)/Wetlab/Northshore/North Shore Results/OTR V1+V2+V3 Northshore Results From 12-23-21.xlsx", 
#                 sheetIndex = 1)
# View(dat)
# 
# tabyl(dat$Patient.Name)
# 
# pivot_wider(select(dat, Patient.Name, Test, Result),
#             id_cols = "Patient.Name",
#             names_from = "Test",
#             values_from = "Result") %>%
#   separate(., "Patient.Name", c("stu", "fullid"), sep=",") %>%
#   separate(., "fullid", c("study", "id", "visit"), sep = " ", remove = F) %>%
#   mutate(visit = substr(visit, 2,2)) %>%
#   select(-c(study, stu, LDLN)) %>%
#   mutate(Notes = NA_real_)-> wide
# 
# View(wide)
# 
# # check against request list
# check <- read.xlsx("/Users/phoebelam/Northwestern University/FHRC - Documents/NIH R01 On the Rise (OTR)/Wetlab/Northshore/North Shore Sample Lists/Account 61336 Sample 12012021.xlsx",
#                    sheetIndex = 1) %>%
#   select(Last.Name) %>%
#   rename(fullid = Last.Name) %>%
#   mutate(check = 1)
# 
# View(check)
# 
# merge (wide, check, by = "fullid", all=T) %>% View() # all good
# 
# View(wide)
# 
# 
# #read the compiled ones.
# # v1 <-read.xlsx("/Users/phoebelam/Northwestern University/FHRC - Documents/NIH R01 On the Rise (OTR)/Wetlab/Northshore/OTR V1 NorthShore Data Compiled.xlsx",
# #                sheetIndex = 1) %>%
# #   filter(is.na(Subject.ID)==F) %>%
# #   mutate (visit = 1)
# # 
# # v2 <-read.xlsx("/Users/phoebelam/Northwestern University/FHRC - Documents/NIH R01 On the Rise (OTR)/Wetlab/Northshore/OTR V2 NorthShore Data Compiled.xlsx",
# #                sheetIndex = 1) %>%
# #   filter(is.na(ATPCM)==F) %>%
# #   mutate (visit = 2)
# # 
# # v3 <-read.xlsx("/Users/phoebelam/Northwestern University/FHRC - Documents/NIH R01 On the Rise (OTR)/Wetlab/Northshore/OTR V3 NorthShore Data Compiled.xlsx",
# #                sheetIndex = 1) %>%
# #   filter(is.na(ATPCM)==F) %>%
# #   mutate (visit = 3)
# # 
# # rbind(v1, v2, v3) -> consol
# # 
# # consol %>%
# #   rename(id = Subject.ID) %>%
# #   select(id, visit, everything()) -> consol
# # 
# # write.csv(consol, "/Users/phoebelam/Northwestern University/FHRC - Documents/NIH R01 On the Rise (OTR)/Wetlab/Northshore/OTR NorthShore Data_2.11.22.csv",
# #           row.names=F, na="")
# # 
# # colnames(v1)
# # colnames(v2)
# # colnames(v3)
# 
# exist <- read.csv("/Users/phoebelam/Northwestern University/FHRC - Documents/NIH R01 On the Rise (OTR)/Wetlab/Northshore/OTR NorthShore Data_2.11.22.csv") %>%
#   mutate(fullid = paste("OTR ", id, " V", visit, sep="")) %>%
#   select(., fullid, everything())
# 
# colnames(exist)
# colnames(wide)
# 
# rbind(exist, wide) %>%
#   arrange(id, visit) -> new
# 
# write.csv(new, "/Users/phoebelam/Northwestern University/FHRC - Documents/NIH R01 On the Rise (OTR)/Wetlab/Northshore/OTR NorthShore Data_2.14.22.csv",
#           row.names=F, na="")
# 
# 
# tabyl(new$CHHD, show_na = T)
# tabyl(new$CHOL) #4
# tabyl(new$CRPH, show_missing_levels = F) # 7
# tabyl(new$GLU, show_missing_levels = F) # 1
# tabyl(new$HDL, show_missing_levels = F) # 0
# tabyl(new$LDL, show_missing_levels = F) # 1
# tabyl(new$NHDL, show_missing_levels = F) # 1
# tabyl(new$TRIG, show_missing_levels = F) # 2
