# combines raw threshold files into one in long format
#' @importFrom magrittr "%>%"
#' @export
iprthres <- function (path) {
  library (xlsx)
  library (tidyverse)
  library (gtools)

  setwd(path)
  consol <- data.frame(matrix(ncol = 1, nrow=1))
  saveRDS(consol, "consolidated threshold.RDS")

  filenames = list.files(path=path, pattern= ".xls", full.names=T, recursive = F)

  for (f in filenames) {
    print (f)

    d <- read.xlsx (f, sheetIndex = 1, header = T)

    d [1, 1] %>%
      gsub("Poverty Thresholds for ", "", .) %>%
      substr(., 1, 4) -> year


    which(d == "Weighted average thresholds", arr.ind = TRUE) %>%
      as.data.frame() %>%
      select(col) %>% as.numeric(.) -> col

    if (is.na(col)==T) {
      which(d == "Weighted average poverty thresholds", arr.ind = TRUE) %>%
        as.data.frame() %>%
        select(col) %>% as.numeric(.) -> col
    }

    if (is.na(col)==T) {
      which(d == "Weighted", arr.ind = TRUE) %>%
        as.data.frame() %>%
        select(col) %>% as.numeric(.) -> col
    }

    if (is.na(col)==T){
      colnames(d) <- c("familysize", "child0", "child1", "child2", "child3",
                       "child4", "child5", "child6", "child7", "child8")
      d[ , 1:10] -> d

    } else {
      colnames (d) <- c("familysize", "weightedavg", "child0", "child1", "child2", "child3",
                        "child4", "child5", "child6", "child7", "child8")

      d[ , 1:11] -> d
    }


    d %>%
      dplyr::mutate (familysize = case_when(grepl("Under 65 years", familysize)==T ~ 1,
                                     grepl ("Under age 65", familysize)==T~ 1,
                                     grepl("Householder under 65 years", familysize)==T ~ 2,
                                     grepl("Householder under age 65", familysize)==T ~ 2,
                                     grepl("Three people", familysize)==T ~ 3,
                                     grepl("Four people", familysize)==T ~ 4,
                                     grepl("Five people", familysize)==T ~ 5,
                                     grepl("Six people", familysize)==T ~ 6,
                                     grepl("Seven people", familysize)==T ~ 7,
                                     grepl("Eight people", familysize)==T ~ 8,
                                     grepl("Nine people", familysize)==T ~ 9)) %>%
      dplyr::filter (is.na(familysize)==F) %>%
      dplyr::select (familysize, child0, child1, child2, child3, child4, child5, child6, child7, child8)-> d


    pivot_longer(d,
                 cols = c(child0:child8),
                 names_prefix = "child",
                 names_to = "child",
                 values_to = "threshold",
                 values_drop_na = TRUE) -> dl

    dl %>%
      mutate (year = year) %>%
      select (year, familysize, child, threshold) -> dl

    as.data.frame(dl)-> dl

    consol <- readRDS("consolidated threshold.RDS")
    smartbind(consol, dl) -> consol
    saveRDS(consol, "consolidated threshold.RDS")

  }

  consol <- readRDS ("consolidated threshold.RDS")[-1, -1]
  consol %>%
    mutate (year = as.numeric(as.character(year))) -> consol


  min(consol$year) -> minyear
  max(consol$year) -> maxyear

  write.csv (consol, paste("consolidated threshold ", minyear, " to ", maxyear, ".csv", sep=""), row.names=F)

  file.remove("consolidated threshold.RDS")

}
# iprthres(path= "/Users/phoebelam/Desktop/threshold")


# return ipr based on vectors of year, family size, $ of children, and income
#' @importFrom magrittr "%>%"
#' @export
iprcalc <- function (year, familysize, children, income,
                     result = c("ipr", "threshold")) {

  thres %>%
    dplyr::mutate_all(., ~as.numeric(as.character(.))) %>%
    dplyr::rename (children = child) -> thres

  d <- data.frame(year, familysize, children, income)

  dplyr::inner_join(d, thres, by = c("year", "familysize", "children")) -> joined

  ipr = joined$income/joined$threshold

  if (missing(result)) {
    result = "ipr"
  }

  if (result == "ipr") {
    return(ipr)
  } else if (result == "threshold") {
    return(joined$threshold)
  }

}

# # testing with vectors of values
# iprcalc(year= c(2014, 2015, 2018),
#         familysize = c(4, 5, 2),
#         children = c(1, 3, 1),
#         income = c(32000, 45000, 5000))
#
# iprcalc(path = "/Users/phoebelam/Desktop/threshold",
#         year= c(2014, 2015, 2018),
#         familysize = c(4, 5, 2),
#         children = c(1, 3, 1),
#         income = c(32000, 45000, 5000),
#         result = "threshold")
#
# # testing with single values
# iprcalc(path = "/Users/phoebelam/Desktop/threshold",
#         2015, 4, 2, 10000)
#
# iprcalc(path = "/Users/phoebelam/Desktop/threshold",
#         2015, 4, 2, 10000, result = "threshold")
#
#
# # testing with dataframe with dplyr application
# test <- data.frame(year, familysize, children, income)
# test %>%
#   mutate (ipr = iprcalc("/Users/phoebelam/Desktop/threshold", year, familysize, children, income),
#           threshold = iprcalc("/Users/phoebelam/Desktop/threshold", year, familysize, children, income,
#                               result = "threshold")) -> test
#


# return the ipr based on user-input of year, family size, # of children, and income
# can only take single values, not vectors
# obsolete, the vector one below will handle this
# iprcalc <- function (path, year, familysize, children, income) {
#
#   setwd(path)
#   thres <- read.csv("consolidated threshold 2014 to 2019.csv")
#
#   thres %>%
#     mutate_all(., ~as.numeric(as.character(.))) %>%
#     rename (censusyear = year,
#             size = familysize) -> thres
#
#   thres %>% filter (censusyear %in% year & size %in% familysize & child %in% children) %>%
#     select (threshold) %>% as.numeric(.) -> x
#
#   ipr = income/x
#
#   return(ipr)
#
# }
# iprcalc(path= "/Users/phoebelam/Desktop/threshold",
#          year = 2014,
#          familysize = 5,
#          children = 3,
#          income = 320000)
