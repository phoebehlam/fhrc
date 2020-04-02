## code to prepare `DATASET` dataset goes here

# save this data file in the package
thres <- read.csv("/Users/phoebelam/Google Drive/fhrc/fhrc/inst/extdata/consolidated threshold 2014 to 2019.csv")
usethis::use_data(thres, internal = F)
