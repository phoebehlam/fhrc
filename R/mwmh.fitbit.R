# mwmh fitbit updated 4.9.20
#' consolidate individual fitbit data
#'
#' consolidate individual fitbit data and export activity and sleep data in long and wide format
#' relevant for mwmh, but can be adapted for studies that use fitbit
#'
#' @param path the folder that contains all the individual files to be merged (data can be in subfolders)
#'
#' @examples
#' mwmh.fitbit (path = "/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Data/V2/OG & IP Data by Source/Fitbit Data/MWMH Fitbit_Individual Excel Files")
#'
#' @importFrom magrittr "%>%"
#' @export
mwmh.fitbit <- function(path) {

  setwd(path)

  print ("f h r c  |  working on consolidating activity ...")

  # consolidate activity data
  activity <- grep(list.files(path= path, pattern = "edited|activity", full.names = T, recursive=T, ignore.case = T),
       pattern = "sleep|Original|(KV)|donotuse|.csv", inv=T, value = T)
  actdf <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(actdf, "actdf.RDS")

  for (f in activity) {
    print(f)
    temp <- xlsx::read.xlsx (file=f, sheetName = "Activities", header = TRUE)
    which(grepl("Averages", temp$Day)) - 1-> RowEnd
    which(colnames (temp)=="Minutes.very.active") -> ColEnd
    temp <- temp [1:RowEnd, 1:ColEnd]
    temp %>%
      dplyr::rename (Sedentary = Minutes.sedentary,
                     LightActive = Minutes.Lightly.Active,
                     FairActive = Minutes.Fairly.Active,
                     VeryActive = Minutes.very.active) %>%
      dplyr::mutate (Day =  gsub("Day ", "", Day),
                     Date = as.character(Date)) -> temp

    basename(f) %>%
      gsub ("MWMH", "", .) %>%
      substr(., 0, 3) %>% as.numeric ()-> temp$ID
    actdf <- readRDS ("actdf.RDS")
    actdf <- gtools::smartbind (actdf, temp)
    saveRDS(actdf, "actdf.RDS")
  }

  # clean consolidated activity data
  actdf<-readRDS ("actdf.RDS")[-1,-1]

  actdf %>%
    dplyr::mutate_at (vars (Day, Steps, Sedentary, LightActive, FairActive, VeryActive, ID),
                      list (~gsub(",", "", .))) %>%
    dplyr::mutate_at (vars (Day, Steps, Sedentary, LightActive, FairActive, VeryActive, ID),
                      list (~as.numeric(as.character(.)))) %>%
    dplyr::select (ID, Day, Date, Steps:VeryActive) %>%
    dplyr::arrange(ID, Day) -> actdf
  colnames(actdf) <- tolower(colnames(actdf))

  # wide format
  tidyr::pivot_wider(actdf,
                     id_cols = "id",
                     names_from = "day",
                     names_prefix = "day",
                     values_from = date:veryactive) -> wide

  write.csv(wide, "mwmh_fitbit activity_wide_4.9.2020.csv", row.names = F, na = "")
  write.csv(actdf, "mwmh_fitbit activity_long_4.9.2020.csv", row.names = F, na = "")
  file.remove("actdf.RDS")

  print ("f h r c  |  done with activity data, working on sleep data...")

  #consolidate sleep data
  sleep <- grep( intersect(list.files (path, pattern = "_sleep", full.names = TRUE, recursive = TRUE),
                           list.files (path, pattern = ".xlsx", full.names = TRUE, recursive = T)),
       pattern = "activity|Original|(KV)|donotuse|.csv|original", inv=T, value = T)

  sldf <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(sldf, "sldf.RDS")

  for (f in sleep) {
    print(f)

    temp <- xlsx::read.xlsx2 (file=f, sheetName = 1, header = TRUE)
    names(temp)[1]<- "Night"
    temp$Night %>%
      gsub ("Day", "Night", .) %>%
      gsub (" ", "", .) -> temp$Night
    which(grepl("Averages", temp$Night)) - 1-> RowEnd
    which(colnames (temp)=="Time.in.Bed") -> ColEnd
    temp <- temp [1:RowEnd, 1:ColEnd]
    temp %>%
      tidyr::separate (Start.Date.Time, c("StartDate", "StartTime"), " ", fill = "right") %>%
      tidyr::separate (End.Date.Time, c("EndDate", "EndTime"), " ", fill = "right") %>%
      dplyr::rename (Awakenings = Number.of.Awakenings.,
                     Minutes.InBed = Time.in.Bed,
                     SleepDate = StartDate,
                     WakeDate = EndDate,
                     SleepTime = StartTime,
                     WakeTime = EndTime) %>%
      dplyr::mutate (SleepDate = as.character(SleepDate),
                     WakeDate = as.character(WakeDate)) -> temp

    basename(f) %>%
      gsub ("MWMH", "", .) %>%
      substr(., 0, 3) %>% as.numeric () -> temp$ID

    sldf <- readRDS ("sldf.RDS")
    sldf <- gtools::smartbind (sldf, temp)
    saveRDS(sldf, "sldf.RDS")
  }

  #clean sleep data
  sldf <- readRDS ("sldf.RDS")[-1,-1]

  sldf %>% filter(is.na(ID)==T)

  sldf %>%
    dplyr::mutate(Night = gsub("Night", "", Night)) %>%
    dplyr::mutate_at (vars (Minutes.Asleep:ID),
                      list (~gsub(",", "", .))) %>%
    dplyr::mutate_at (vars (Minutes.Asleep:ID),
                      list (~as.numeric(as.character(.)))) %>%
    dplyr::select (ID, Night:Minutes.InBed) %>%
    dplyr::arrange(ID, Night) -> sldf
  colnames(sldf) <- tolower(colnames(sldf))

  # wide format
  tidyr::pivot_wider(sldf,
                     id_cols = "id",
                     names_from = "night",
                     names_prefix = "night",
                     values_from = sleepdate:minutes.inbed) -> wide

  write.csv(wide, "mwmh_fitbit sleep_wide_4.9.2020.csv", row.names = F, na = "")
  write.csv(sldf, "mwmh_fitbit sleep_long_4.9.2020.csv", row.names = F, na = "")
  file.remove("sldf.RDS")

  print ("f h r c  |  done with sleep data.")

}
# mwmh.fitbit (path = "/Users/phoebelam/Box/FHRC (Weinberg Box Admin)/NIH R01 My World My Heart Study (MWMH)/Data/V2/OG & IP Data by Source/Fitbit Data/MWMH Fitbit_Individual Excel Files")

