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

#
#' ipr calculator
#'
#' return ipr based user input of year, family size, # of children, and income
#'
#' @param year the year (2014 - 2019 only) of for the poverty threshold to use from: https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
#' @param familysize the total number of people in the household
#' @param children the number of individuals under age of 18
#' @param income the family income
#' @param result enter result = "ipr" to return ipr value and result = "threshold" to return threshold. defaults ipr if unspecified.
#'
#'
#'#' @examples
#' # if you have just single values
#' iprcalc(2015, 5, 3, 30000)
#' iprcalc(2015, 5, 3, 30000, result = "threshold")
#'
#' @examples
#' # if you have a vector of values
#' iprcalc(year= c(2014, 2015, 2018),
#'         familysize = c(4, 5, 2),
#'        children = c(1, 3, 1),
#'         income = c(32000, 45000, 5000))
#'
#' @examples
#' # if you have a dataframe with columns for the input and want to use with dplyr
#' # making fake data here
#' numofppl <- c(3, 2, 5, 3)
#' numofchildren <- c(1, 1, 2, 2)
#' totincome <- c(30000, 60000, 15000, 10000)
#' fake <- data.frame (numofppl, numofchildren, totincome)
#'
#' library(dplyr)
#' fake %>%
#'   mutate (ipr = iprcalc(2019, numofppl, numofchildren, totincome),
#'           threshold = iprcalc(2019, numofppl, numofchildren, totincome,
#'                               result = "threshold")) -> fake
#' View (fake)
#'
#'
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













