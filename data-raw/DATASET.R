## code to prepare `DATASET` dataset goes here

# save this data file in the package
thres <- read.csv("/Users/phoebelam/Library/Mobile Documents/com~apple~CloudDocs/Documents/the kitchen/fhrc/inst/extdata/consolidated threshold 2014 to 2020.csv") %>%
  dplyr::mutate_all(., ~as.numeric(as.character(.))) %>%
  dplyr::rename (children = child)
aspe <- read.csv('/Users/phoebelam/Library/Mobile Documents/com~apple~CloudDocs/Documents/the kitchen/fhrc/inst/extdata/aspe consolidated threshold 2017 to 2023.csv')
usethis::use_data(thres, internal = F, overwrite = T)
usethis::use_data(aspe, internal = F, overwrite = T)

# thre20<-xlsx::read.xlsx("/Volumes/GoogleDrive/My Drive/chialam/stats resources/the kitchen/fhrc/inst/extdata/thresh20.xlsx", sheetIndex = 1)
# 
# thre20 %>%
#   rename(familysize = 1,
#          child0=3,
#          child1=4,
#          child2=5,
#          child3=6,
#          child4=7,
#          child5=8,
#          child6=9,
#          child7=10,
#          child8=11) %>%
#   select(familysize, child0:child8) %>% 
#   filter(familysize %in% c("Under age 65", "Householder under age 65") |
#            is.na(familysize)==F ) %>% 
#   filter(!familysize %in% c("Poverty Thresholds for 2020 by Size of Family and Number of Related Children Under 18 Years",
#                             "Size of family unit",
#                             "One person (unrelated individual):",
#                             "Two people:",
#                             "Source:  U.S. Census Bureau.",
#                             "Aged 65 and older",
#                             "Householder aged 65 and older")) %>% 
#   mutate(familysize = row_number()) %>%
#   mutate_at(vars(child0:child8),
#             list(~as.numeric(as.character(.)))) -> wide
# 
# pivot_longer(wide, 
#              cols=-familysize,
#              names_prefix = "child",
#              names_to = "child") %>%
#   filter(is.na(value)==F) %>%
#   rename(threshold = value) %>%
#   mutate(year = 2020) %>%
#   select(year, familysize, child, threshold)-> long20
# 
# rbind(thres, long20) -> thres
# write.csv(thres, "/Volumes/GoogleDrive/My Drive/chialam/stats resources/the kitchen/fhrc/inst/extdata/consolidated threshold 2014 to 2020.csv", 
#           row.names = F)


# for aspe, easy way
# library(rvest)
# 
# # 2023
# as2023 <- read_html("https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2022-poverty-guidelines#threshholds") %>%
#   html_nodes(xpath = '//*[@id="block-aspe-uswds-content"]/article/div[1]/div/table[1]') %>%
#   html_table(.)
# 
# as2023 <- as2023[[1]] %>%
#   as.data.frame(.) %>%
#   rename(familysize = 1,
#          threshold = 2) %>%
#   mutate(threshold = as.numeric(as.character(gsub("\\$|\\,", "", .$threshold)))) %>%
#   filter(is.na(threshold)==F) %>%
#   mutate(year = 2023) %>%
#   select(year, familysize, threshold)
# 
# 
# # 2022
# as2022 <- read_html("https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2022-poverty-guidelines#threshholds") %>%
#   html_nodes(xpath = '//*[@id="block-aspe-uswds-content"]/article/div[1]/div/table[1]') %>%
#   html_table(.)
# 
# as2022 <- as2022[[1]] %>%
#   as.data.frame(.) %>%
#   rename(familysize = 1,
#          threshold = 2) %>%
#   mutate(threshold = as.numeric(as.character(gsub("\\$|\\,", "", .$threshold)))) %>%
#   filter(is.na(threshold)==F) %>%
#   mutate(year = 2022) %>%
#   select(year, familysize, threshold)
# 
# # 2021
#   as2021 <- read_html("https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2021-poverty-guidelines#threshholds") %>%
#     html_nodes(xpath = '//*[@id="block-aspe-uswds-content"]/article/div[1]/div/table[1]') %>%
#     html_table(.)
# 
#   as2021 <- as2021[[1]] %>%
#     as.data.frame(.) %>%
#     rename(familysize = 1,
#            threshold = 2) %>%
#     mutate(threshold = as.numeric(as.character(gsub("\\$|\\,", "", .$threshold)))) %>%
#     filter(is.na(threshold)==F) %>%
#     mutate(year = 2021) %>%
#     select(year, familysize, threshold)
# 
# # 2020
#   as2020 <- read_html("https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2020-poverty-guidelines") %>%
#     html_nodes(xpath = '//*[@id="block-aspe-uswds-content"]/article/div[1]/div/table[1]') %>%
#     html_table(.)
# 
#   as2020 <- as2020[[1]] %>%
#     as.data.frame(.) %>%
#     rename(familysize = 1,
#            threshold = 2) %>%
#     mutate(threshold = as.numeric(as.character(gsub("\\$|\\,", "", .$threshold)))) %>%
#     filter(is.na(threshold)==F) %>%
#     mutate(year = 2020) %>%
#     select(year, familysize, threshold)
# 
#   # 2019
#   as2019 <- read_html("https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2019-poverty-guidelines") %>%
#     html_nodes(xpath = '//*[@id="block-aspe-uswds-content"]/article/div[1]/div/table[1]') %>%
#     html_table(.)
# 
#   as2019 <- as2019[[1]] %>%
#     as.data.frame(.) %>%
#     rename(familysize = 1,
#            threshold = 2) %>%
#     mutate(threshold = as.numeric(as.character(gsub("\\$|\\,", "", .$threshold)))) %>%
#     filter(is.na(threshold)==F) %>%
#     mutate(year = 2019) %>%
#     select(year, familysize, threshold)
# 
#   # 2018
#   as2018 <- read_html("https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2018-poverty-guidelines") %>%
#     html_nodes(xpath = '//*[@id="block-aspe-uswds-content"]/article/div[1]/div/table[1]') %>%
#     html_table(.)
# 
#   as2018 <- as2018[[1]] %>%
#     as.data.frame(.) %>%
#     rename(familysize = 1,
#            threshold = 2) %>%
#     mutate(threshold = as.numeric(as.character(gsub("\\$|\\,", "", .$threshold)))) %>%
#     filter(is.na(threshold)==F) %>%
#     mutate(year = 2018) %>%
#     select(year, familysize, threshold)
# 
#   # 2017
#   as2017 <- read_html("https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2017-poverty-guidelines") %>%
#     html_nodes(xpath = '//*[@id="block-aspe-uswds-content"]/article/div[1]/div/table[1]') %>%
#     html_table(.)
# 
#   as2017 <- as2017[[1]] %>%
#     as.data.frame(.) %>%
#     rename(familysize = 1,
#            threshold = 2) %>%
#     mutate(threshold = as.numeric(as.character(gsub("\\$|\\,", "", .$threshold)))) %>%
#     filter(is.na(threshold)==F) %>%
#     mutate(year = 2017) %>%
#     select(year, familysize, threshold)
# 
#   rbind(as2017, as2018, as2019, as2020, as2021) -> aspe
# 
#   write.csv(aspe, "/Volumes/GoogleDrive/My Drive/chialam/stats resources/the kitchen/fhrc/inst/extdata/aspe consolidated threshold 2017 to 2021.csv", row.names=F)
#   
#   
#   
#   write.csv(aspethres, paste(path, "/aspe.csv", sep=""), row.names=F)