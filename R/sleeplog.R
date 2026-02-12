#'@importFrom magrittr "%>%"
#'@export
sleeplog <- function(path,
                     filename_common_string,
                     tracker_filename,
                     id, 
                     study, 
                     visit, 
                     daylight = F, 
                     dlstart,
                     dlend) {

  # troubleshoot
  # file <- read.csv("/Users/phoebelam/Desktop/sleep/NIH+CON+V1+Daily+Diary+Days+1-7_January+29,+2026_16.29.csv", header = T) [-c(1:2), ]
  # file <- haven::read_sav('/Users/phoebelam/Desktop/sleep/NIH+CON+V1+Daily+Diary+Days+1-7_February+7,+2026_11.54.sav')
  # file <- haven::read_sav('NIH+CON+V1+Daily+Diary+Day+8_February+7,+2026_11.07.sav')
  # 
  # path = '/Users/phoebelam/Desktop/sleep'
  # filename_common_string = 'NIH+CON+V1+Daily+Diary+Day'
  # study = 'cons'
  # visit = 1
  # id = 3568794
  # tracker_filename = 'NIH CON Actigraphy Tracking'
  
  # using a different init
  # log <- data.frame(matrix(ncol = 1, nrow = 1))
  # other <- data.frame(matrix(ncol = 1, nrow = 1))
  # saveRDS(log, paste(path, "sleeplog.rds", sep="/"))
  # saveRDS(other, paste(path, "otherlog.rds", sep="/"))
  
  log_path   <- file.path(path, "sleeplog.rds")
  other_path <- file.path(path, "otherlog.rds")
  
  saveRDS(NULL, log_path)
  saveRDS(NULL, other_path)
  
  all_files <- list.files(path = path, full.names = T, recursive = F)
  filenames <- all_files[grepl(filename_common_string, all_files, fixed = TRUE)]
  
  for (f in filenames) { 
    print (f)
    
    file <- haven::read_sav(f)
    
    file %>%
      dplyr::mutate (externalref.check = as.numeric(as.character(ExternalReference))) -> file
    
    file %>%
      dplyr::mutate(goodid = dplyr::case_when(is.na(externalref.check)==F~ 
                                                as.numeric(as.character(ExternalReference)))) -> file
    

    if (any (file$goodid == id, na.rm=T) == TRUE) { 
      # troubleshoot
      # basename("/Users/phoebelam/Desktop/sleep/NIH+CON+V1+Daily+Diary+Day+8_February+7,+2026_11.07.sav") %>%
      #   grepl('+8', ., ignore.case = T) -> day8check
      # basename("/Users/phoebelam/Desktop/sleep/NIH+CON+V1+Daily+Diary+Days+1-7_February+7,+2026_11.07.sav") %>%
      #   grepl('+8', ., ignore.case = T) -> day8check
      
      basename(f) %>% grepl('+8', ., ignore.case = T) -> day8check
      
      file %>%
        dplyr::mutate (day8check = day8check) -> file

      # cleaning for days 1-7
      if (day8check ==F) {
        file %>% dplyr::select (., StartDate, EndDate, goodid, BedTime_1_1:med_text, ResponseId) %>%
          dplyr::filter (., goodid == id )-> file
        
        # end date as date completed
        file %>%
          tidyr::separate (EndDate, c("Date", "Time"), " ", fill = "right", remove= FALSE) -> file
        
        # generate the dates for the sleep date participant is reporting about and the date participant reported sleep
        # for "actual"= date participant is reporting about
        # if they did it before midnight, then the reported sleep date = qualtrics timestamp date - 1 (because it reference last night)
        # if they did it after midnight, then the reported sleep date = qualtrics timestamp date - 2 (because it reference last night and they did it past midnight)
        # for "s.rep_actual.adj" = date participant reported sleep (adjusted)
        # if they did it before midnight, then the reported sleep date = qualtrics timestamp date (no adjustment)
        # if they did it after midnight, then the reported sleep date = qualtrics timestamp date - 1 (because they did it past midnight)
        # using hour < 21 to defined past midnight, because each diary sent at 9pm, so no way they can do it before then for any given day
        
        file %>%
          tidyr::separate (Time, c("hour", "min", "sec"), ":", fill ="right", remove = FALSE) %>%
          dplyr::mutate (hour = as.numeric (as.character(hour))) -> file
      
        file %>%
          dplyr::mutate (s.rep_actual.adj = dplyr::case_when (hour < 21 ~ as.Date(Date, "%Y-%m-%d")-1,
                                                              TRUE ~ as.Date(Date, "%Y-%m-%d"))) %>%
          dplyr::mutate (actual = dplyr::case_when (hour < 21 ~ as.Date(Date, "%Y-%m-%d")-2,
                                                    TRUE ~ as.Date(Date, "%Y-%m-%d") - 1)) -> file
        
        file$actual.wd <- weekdays(as.Date(file$actual))
        file$s_rep.actual_weekday <- weekdays(as.Date(file$s.rep_actual.adj))
        
        # readability
        # labelled::val_labels(file$BedTime_3_1) # am=1, pm=2
        file %>%
          dplyr::mutate(
            bedtime_hour   = BedTime_1_1,
            bedtime_minute = BedTime_2_1,
            bedtime_ampm = dplyr::case_when(BedTime_3_1== 1~ 'am',
                                     BedTime_3_1== 2~ 'pm'),
            waketime_hour   = WakeTime_1_1,
            waketime_minute = WakeTime_2_1,
            waketime_ampm = dplyr::case_when(WakeTime_3_1== 1~ 'am',
                                      WakeTime_3_1== 2~ 'pm'),
            
            BedTime  = paste0(bedtime_hour, ":", bedtime_minute, " ", bedtime_ampm),
            WakeTime = paste0(waketime_hour, ":", waketime_minute, " ", waketime_ampm)) -> file

        #remove/puton hr, min, am/pm into one cell
        file %>%
          dplyr::mutate(remove_ampm = case_when(Remove1_3_1 == 1~ 'am',
                                                Remove1_3_1 == 2~ 'pm'),
                        puton_ampm = case_when(PutOn1_3_1 == 1~ 'am',
                                               PutOn1_3_1 == 2~ 'pm')) -> file
        
        file %>%
          dplyr::mutate(
            Remove1 = dplyr::if_else(NumRemove >= 1 & !is.na(Remove1_1_1) & !is.na(Remove1_2_1) & !is.na(remove_ampm),
                                     paste0(Remove1_1_1, ":", Remove1_2_1, " ", remove_ampm),
                                     NA_character_),
            Remove2 = dplyr::if_else(NumRemove >= 2 & !is.na(Remove2_1_1) & !is.na(Remove2_2_1) & !is.na(remove_ampm),
                                     paste0(Remove2_1_1, ":", Remove2_2_1, " ", remove_ampm),
                                     NA_character_),
            Remove3 = dplyr::if_else(NumRemove >= 3 & !is.na(Remove3_1_1) & !is.na(Remove3_2_1) & !is.na(remove_ampm),
                                     paste0(Remove3_1_1, ":", Remove3_2_1, " ", remove_ampm),
                                     NA_character_),
            Remove4 = dplyr::if_else(NumRemove >= 4 & !is.na(Remove4_1_1) & !is.na(Remove4_2_1) & !is.na(remove_ampm),
                                     paste0(Remove4_1_1, ":", Remove4_2_1, " ", remove_ampm),
                                     NA_character_),
            
            PutOn1  = dplyr::if_else(NumRemove >= 1 & !is.na(PutOn1_1_1) & !is.na(PutOn1_2_1) & !is.na(puton_ampm),
                                     paste0(PutOn1_1_1, ":", PutOn1_2_1, " ", puton_ampm),
                                     NA_character_),
            PutOn2  = dplyr::if_else(NumRemove >= 2 & !is.na(PutOn2_1_1) & !is.na(PutOn2_2_1) & !is.na(puton_ampm),
                                     paste0(PutOn2_1_1, ":", PutOn2_2_1, " ", puton_ampm),
                                     NA_character_),
            PutOn3  = dplyr::if_else(NumRemove >= 3 & !is.na(PutOn3_1_1) & !is.na(PutOn3_2_1) & !is.na(puton_ampm),
                                     paste0(PutOn3_1_1, ":", PutOn3_2_1, " ", puton_ampm),
                                     NA_character_),
            PutOn4  = dplyr::if_else(NumRemove >= 4 & !is.na(PutOn4_1_1) & !is.na(PutOn4_2_1) & !is.na(puton_ampm),
                                     paste0(PutOn4_1_1, ":", PutOn4_2_1, " ", puton_ampm),
                                     NA_character_)
          ) %>%
          dplyr::mutate(
            remove1t = as.POSIXct(Remove1, format = "%I:%M %p"),
            remove2t = as.POSIXct(Remove2, format = "%I:%M %p"),
            remove3t = as.POSIXct(Remove3, format = "%I:%M %p"),
            remove4t = as.POSIXct(Remove4, format = "%I:%M %p"),
            puton1t  = as.POSIXct(PutOn1,  format = "%I:%M %p"),
            puton2t  = as.POSIXct(PutOn2,  format = "%I:%M %p"),
            puton3t  = as.POSIXct(PutOn3,  format = "%I:%M %p"),
            puton4t  = as.POSIXct(PutOn4,  format = "%I:%M %p")
          ) -> file
        
        
        
        #categorizing the 4 combinations of am/pm combo
        file %>%
          dplyr::mutate(
            ampmcheck1 = dplyr::case_when(
              as.character(remove_ampm) == "pm" & as.character(puton_ampm) == "am" ~ 1,
              as.character(remove_ampm) == "am" & as.character(puton_ampm) == "pm" ~ 2,
              as.character(remove_ampm) == "pm" & as.character(puton_ampm) == "pm" ~ 3,
              as.character(remove_ampm) == "am" & as.character(puton_ampm) == "am" ~ 4,
              TRUE ~ NA_real_
            ),
            ampmcheck2 = dplyr::case_when(
              as.character(remove_ampm) == "pm" & as.character(puton_ampm) == "am" ~ 1,
              as.character(remove_ampm) == "am" & as.character(puton_ampm) == "pm" ~ 2,
              as.character(remove_ampm) == "pm" & as.character(puton_ampm) == "pm" ~ 3,
              as.character(remove_ampm) == "am" & as.character(puton_ampm) == "am" ~ 4,
              TRUE ~ NA_real_
            ),
            ampmcheck3 = dplyr::case_when(
              as.character(remove_ampm) == "pm" & as.character(puton_ampm) == "am" ~ 1,
              as.character(remove_ampm) == "am" & as.character(puton_ampm) == "pm" ~ 2,
              as.character(remove_ampm) == "pm" & as.character(puton_ampm) == "pm" ~ 3,
              as.character(remove_ampm) == "am" & as.character(puton_ampm) == "am" ~ 4,
              TRUE ~ NA_real_
            ),
            ampmcheck4 = dplyr::case_when(
              as.character(remove_ampm) == "pm" & as.character(puton_ampm) == "am" ~ 1,
              as.character(remove_ampm) == "am" & as.character(puton_ampm) == "pm" ~ 2,
              as.character(remove_ampm) == "pm" & as.character(puton_ampm) == "pm" ~ 3,
              as.character(remove_ampm) == "am" & as.character(puton_ampm) == "am" ~ 4,
              TRUE ~ NA_real_)) -> file
        
        
        #computing duration by each occassion, only treating PM -> AM versions for now
        file %>%  
          dplyr::mutate (duration1 = dplyr::case_when (ampmcheck1 == 1 ~ as.numeric(as.character(difftime(file$puton1t + lubridate::hours (24), file$remove1t, units= "mins"))),
                                                       TRUE ~ as.numeric (as.character (difftime(puton1t, remove1t, units= "mins")))),
                         
                         duration2 = dplyr::case_when (ampmcheck2 == 1 ~ as.numeric(as.character(difftime(file$puton2t + lubridate::hours (24), file$remove2t, units= "mins"))),
                                                       TRUE ~ as.numeric (as.character (difftime(puton2t, remove2t, units= "mins")))),
                         
                         duration3 = dplyr::case_when (ampmcheck3 == 1 ~ as.numeric(as.character(difftime(file$puton3t + lubridate::hours (24), file$remove3t, units= "mins"))),
                                                       TRUE ~ as.numeric (as.character (difftime(puton3t, remove3t, units= "mins")))),
                         
                         duration4 = dplyr::case_when (ampmcheck4 == 1 ~ as.numeric(as.character(difftime(file$puton4t + lubridate::hours (24), file$remove4t, units= "mins"))),
                                                       TRUE ~ as.numeric (as.character (difftime(puton4t, remove4t, units= "mins"))))) -> file
        
        #computing duration for the day
        file %>%
          dplyr::mutate (duration_sum = rowSums (dplyr::select (., duration1, duration2, duration3, duration4), na.rm=TRUE)) -> file
        
        # original is to adjust the Day, but now switching to date instead
        # basename(filename) %>%
        #   gsub ("OTR Daily Diary Day ", "", .) %>%
        #   gsub ("MHS Daily Diary Day ", "", .) %>%
        #   substr(., 0, 1) %>% as.numeric ()-1 -> day
        # as.character (paste ("Day ", day, sep="")) -> file$Day
        
        file %>%
          dplyr::rename (id = goodid,
                         rawcompdate = Date,
                         rawcomptime = Time,
                         rawcompdt = EndDate) %>% 
          dplyr::select(id, actual.wd, actual,
                        BedTime, WakeTime,
                        rawcompdt, rawcompdate, rawcomptime, ResponseId) -> file1
      
        log <- readRDS(file.path(path, "sleeplog.rds"))
        log <- dplyr::bind_rows(log, file1)
        saveRDS(log, file.path(path, "sleeplog.rds"))
        
        file %>% 
          dplyr::rename (id = goodid) %>% 
          dplyr::mutate (hour= as.numeric(as.character(hour))) %>%
          dplyr::mutate (d.rep_actual.adj = dplyr::case_when (hour < 21 ~ as.Date(Date, "%Y-%m-%d")-1,
                                                              TRUE ~ as.Date(Date, "%Y-%m-%d"))) %>%
          dplyr::select (id, NumRemove, Remove1, PutOn1, RemoveReason1,
                         Remove2, PutOn2, RemoveReason2, 
                         Remove3, PutOn3, RemoveReason3,
                         Remove4, PutOn4, RemoveReason4, duration_sum, nap:med_text, d.rep_actual.adj, ResponseId) -> file2
        
        other <- readRDS (file.path(path, "otherlog.rds"))
        other <- dplyr::bind_rows (other, file2)
        saveRDS (other, file.path(path, "otherlog.rds"))
        
      } else { 
        
        file %>% dplyr::select (., StartDate, EndDate, goodid, BedTime_1_1:RemoveReason4, ResponseId) %>%
          dplyr::filter (., goodid == id )-> file
        
        #fixing the date participant do the survey into tidyr::separate date and time cols
        file %>%
          tidyr::separate (EndDate, c("Date", "Time"), " ", fill = "right", remove = FALSE) -> file
        
        file %>%
          tidyr::separate (Time, c("hour", "min", "sec"), ":", fill ="right", remove = FALSE) %>%
          dplyr::mutate (hour = as.numeric (as.character(hour))) -> file
        
        # generate the dates for the sleep date participant is reporting about and the date participant reported sleep
        # day 8 are all done in the AM, so need to undo the rule for adjustement. always just - 1 no matter what. 
        # for "actual"= date participant is reporting about = qualtrics timestamp date - 1 (because it reference last night)
        # for "s.rep_actual.adj" = date participant reported sleep = qualtrics timestamp date (no adjustment)
        
        file %>%
          dplyr::mutate (s.rep_actual.adj = as.Date(Date, "%Y-%m-%d")) %>%
          dplyr::mutate (actual = as.Date(Date, "%Y-%m-%d") - 1) -> file
        
        file$actual.wd <- weekdays(as.Date(file$actual))
        
        file$s_rep.actual_weekday <- weekdays(as.Date(file$s.rep_actual.adj))
        
        file$BedTime <- paste (file$BedTime_1_1, ":", file$BedTime_2_1)
        file$BedTime <- paste (file$BedTime, " ", file$BedTime_3_1)
        file$WakeTime <- paste (file$WakeTime_1_1, ":", file$WakeTime_2_1)
        file$WakeTime <- paste (file$WakeTime, " ", file$WakeTime_3_1)
        
        #remove/puton hr, min, am/pm into one cell
        file %>%
          dplyr::mutate(
            Remove1 = dplyr::case_when(
              NumRemove >= 1 & !is.na(Remove1_1_1) & !is.na(Remove1_2_1) & !is.na(Remove1_3_1) ~
                paste(Remove1_1_1, ":", Remove1_2_1, " ", Remove1_3_1),
              TRUE ~ NA_character_
            ),
            Remove2 = dplyr::case_when(
              NumRemove >= 2 & !is.na(Remove2_1_1) & !is.na(Remove2_2_1) & !is.na(Remove2_3_1) ~
                paste(Remove2_1_1, ":", Remove2_2_1, " ", Remove2_3_1),
              TRUE ~ NA_character_
            ),
            Remove3 = dplyr::case_when(
              NumRemove >= 3 & !is.na(Remove3_1_1) & !is.na(Remove3_2_1) & !is.na(Remove3_3_1) ~
                paste(Remove3_1_1, ":", Remove3_2_1, " ", Remove3_3_1),
              TRUE ~ NA_character_
            ),
            Remove4 = dplyr::case_when(
              NumRemove >= 4 & !is.na(Remove4_1_1) & !is.na(Remove4_2_1) & !is.na(Remove4_3_1) ~
                paste(Remove4_1_1, ":", Remove4_2_1, " ", Remove4_3_1),
              TRUE ~ NA_character_
            ),
            
            PutOn1 = dplyr::case_when(
              NumRemove >= 1 & !is.na(PutOn1_1_1) & !is.na(PutOn1_2_1) & !is.na(PutOn1_3_1) ~
                paste(PutOn1_1_1, ":", PutOn1_2_1, " ", PutOn1_3_1),
              TRUE ~ NA_character_
            ),
            PutOn2 = dplyr::case_when(
              NumRemove >= 2 & !is.na(PutOn2_1_1) & !is.na(PutOn2_2_1) & !is.na(PutOn2_3_1) ~
                paste(PutOn2_1_1, ":", PutOn2_2_1, " ", PutOn2_3_1),
              TRUE ~ NA_character_
            ),
            PutOn3 = dplyr::case_when(
              NumRemove >= 3 & !is.na(PutOn3_1_1) & !is.na(PutOn3_2_1) & !is.na(PutOn3_3_1) ~
                paste(PutOn3_1_1, ":", PutOn3_2_1, " ", PutOn3_3_1),
              TRUE ~ NA_character_
            ),
            PutOn4 = dplyr::case_when(
              NumRemove >= 4 & !is.na(PutOn4_1_1) & !is.na(PutOn4_2_1) & !is.na(PutOn4_3_1) ~
                paste(PutOn4_1_1, ":", PutOn4_2_1, " ", PutOn4_3_1),
              TRUE ~ NA_character_
            )
          ) %>%
          dplyr::mutate(
            remove1t = as.POSIXct(Remove1, format = "%I : %M %p"),
            remove2t = as.POSIXct(Remove2, format = "%I : %M %p"),
            remove3t = as.POSIXct(Remove3, format = "%I : %M %p"),
            remove4t = as.POSIXct(Remove4, format = "%I : %M %p"),
            puton1t  = as.POSIXct(PutOn1,  format = "%I : %M %p"),
            puton2t  = as.POSIXct(PutOn2,  format = "%I : %M %p"),
            puton3t  = as.POSIXct(PutOn3,  format = "%I : %M %p"),
            puton4t  = as.POSIXct(PutOn4,  format = "%I : %M %p")
          ) -> file
        
        
        #categorizing the 4 combinations of am/pm combo
        file %>%
          dplyr::mutate (ampmcheck1 = dplyr::case_when (as.character(Remove1_3_1) == "PM" & as.character(PutOn1_3_1) == "AM" ~ 1,
                                                        as.character(Remove1_3_1) == "AM" & as.character(PutOn1_3_1) == "PM" ~ 2,
                                                        as.character(Remove1_3_1) == "PM" & as.character(PutOn1_3_1) == "PM" ~ 3,
                                                        as.character(Remove1_3_1) == "AM" & as.character(PutOn1_3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck2 = dplyr::case_when (as.character(Remove2_3_1) == "PM" & as.character(PutOn2_3_1) == "AM" ~ 1,
                                                        as.character(Remove2_3_1) == "AM" & as.character(PutOn2_3_1) == "PM" ~ 2,
                                                        as.character(Remove2_3_1) == "PM" & as.character(PutOn2_3_1) == "PM" ~ 3,
                                                        as.character(Remove2_3_1) == "AM" & as.character(PutOn2_3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck3 = dplyr::case_when (as.character(Remove3_3_1) == "PM" & as.character(PutOn3_3_1) == "AM" ~ 1,
                                                        as.character(Remove3_3_1) == "AM" & as.character(PutOn3_3_1) == "PM" ~ 2,
                                                        as.character(Remove3_3_1) == "PM" & as.character(PutOn3_3_1) == "PM" ~ 3,
                                                        as.character(Remove3_3_1) == "AM" & as.character(PutOn3_3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck4 = dplyr::case_when (as.character(Remove4_3_1) == "PM" & as.character(PutOn4_3_1) == "AM" ~ 1,
                                                        as.character(Remove4_3_1) == "AM" & as.character(PutOn4_3_1) == "PM" ~ 2,
                                                        as.character(Remove4_3_1) == "PM" & as.character(PutOn4_3_1) == "PM" ~ 3,
                                                        as.character(Remove4_3_1) == "AM" & as.character(PutOn4_3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_)) -> file
        
        #computing duration by each occassion, only treating PM -> AM versions for now
        file %>%  
          dplyr::mutate (duration1 = dplyr::case_when (ampmcheck1 == 1 ~ as.numeric(as.character(difftime(file$puton1t + lubridate::hours (24), file$remove1t, units= "mins"))),
                                                       TRUE ~ as.numeric (as.character (difftime(puton1t, remove1t, units= "mins")))),
                         
                         duration2 = dplyr::case_when (ampmcheck2 == 1 ~ as.numeric(as.character(difftime(file$puton2t + lubridate::hours (24), file$remove2t, units= "mins"))),
                                                       TRUE ~ as.numeric (as.character (difftime(puton2t, remove2t, units= "mins")))),
                         
                         duration3 = dplyr::case_when (ampmcheck3 == 1 ~ as.numeric(as.character(difftime(file$puton3t + lubridate::hours (24), file$remove3t, units= "mins"))),
                                                       TRUE ~ as.numeric (as.character (difftime(puton3t, remove3t, units= "mins")))),
                         
                         duration4 = dplyr::case_when (ampmcheck4 == 1 ~ as.numeric(as.character(difftime(file$puton4t + lubridate::hours (24), file$remove4t, units= "mins"))),
                                                       TRUE ~ as.numeric (as.character (difftime(puton4t, remove4t, units= "mins"))))) -> file
        
        #computing duration for the day
        file %>%
          dplyr::mutate (duration_sum = rowSums (dplyr::select (., duration1, duration2, duration3, duration4), na.rm=TRUE)) -> file
        
        # basename(filename) %>%
        #   gsub ("OTR Daily Diary Day ", "", .) %>%
        #   gsub ("MHS Daily Diary Day ", "", .) %>%
        #   substr(., 0, 1) %>% as.numeric ()-1 -> day
        # paste ("Day ", day, sep="") -> file$Day
        
        file %>%
          dplyr::rename (id = goodid,
                         rawcompdate = Date,
                         rawcomptime = Time,
                         rawcompdt = EndDate) %>% 
          dplyr::select(id, actual.wd, actual,
                        BedTime, WakeTime,
                        rawcompdt, rawcompdate, rawcomptime, ResponseId) -> file1
        
        log <- readRDS(file.path(path, "sleeplog.rds"))
        log <- dplyr::bind_rows(log, file1)
        saveRDS(log, file.path(path, "sleeplog.rds"))
        
        file %>% 
          dplyr::rename (id = goodid) %>% 
          dplyr::mutate (d.rep_actual.adj = as.Date(Date, "%Y-%m-%d")) %>% #no adjustment for day 8
          dplyr::select (id, NumRemove, Remove1, PutOn1, RemoveReason1,
                         Remove2, PutOn2, RemoveReason2, 
                         Remove3, PutOn3, RemoveReason3,
                         Remove4, PutOn4, RemoveReason4, duration_sum, d.rep_actual.adj) -> file2
        
        other <- readRDS (file.path(path, "otherlog.rds"))
        other <- dplyr::bind_rows (other, file2)
        saveRDS (other, file.path(path, "otherlog.rds"))
        
      }
    }
  }
  
  
  if(visit == 1) {
    track <- openxlsx::read.xlsx(paste0(path, '/', tracker_filename, '.xlsx', sep=''), sheet=1, startRow = 2, detectDates = T)
  } else if (visit == 2) {
    track <- openxlsx::read.xlsx(paste0(path, '/', tracker_filename, '.xlsx', sep=''), sheet=2, startRow = 2, detectDates = T)
  } else if (visit == 3) {
    track <- openxlsx::read.xlsx(paste0(path, '/', tracker_filename, '.xlsx', sep=''), sheet=3, startRow = 2, detectDates = T)
  }
  
  track %>%
    dplyr::mutate(Lab.Visit.Date = as.Date(Lab.Visit.Date)) %>%
    dplyr::rename (should0 = Lab.Visit.Date) %>%
    dplyr::rename_at (dplyr::vars(dplyr::contains("DD.Day.1.", ignore.case = T)),
                      list(~"dd1log")) %>%
    dplyr::filter (ID == id) -> track

    track %>%
      dplyr::mutate (dd1log = as.Date(dd1log)) %>%
      dplyr::mutate (should0 = dd1log-1,
                     should1 = dd1log, 
                     should2 = dd1log + 1,
                     should3 = dd1log + 2,
                     should4 = dd1log + 3,
                     should5 = dd1log + 4,
                     should6 = dd1log + 5,
                     should7 = dd1log + 6,
                     should8 = dd1log + 7) -> track
  
  track %>%
    dplyr::mutate (s_rep.should0 = should1, 
                   s_rep.should1 = should2,
                   s_rep.should2 = should3,
                   s_rep.should3 = should4,
                   s_rep.should4 = should5,
                   s_rep.should5 = should6,
                   s_rep.should6 = should7,
                   s_rep.should7 = should8,
                   s_rep.should8 = NA) %>%
    dplyr::mutate (day0 = 0, #study days
                   day1 = 1,
                   day2 = 2, 
                   day3 = 3,
                   day4 = 4, 
                   day5 = 5,
                   day6 = 6,
                   day7 = 7,
                   day8 = 8) %>%
    dplyr::select(ID, day0:day8, 
                  should0, should1, should2, should3, should4, should5, should6, should7, should8,
                  s_rep.should0:s_rep.should8)-> track
  
  #reshape
  trackl <- reshape (data = track,
                     idvar = "ID",
                     varying = list (day = c(2:10), should=c(11:19), s_rep.should= c(20:28)),
                     direction= "long",
                     v.names = c("day",  "should", "s_rep.should"),
                     sep="")
  
  #check whether there are any entries at all
  log <- readRDS (file.path(path, 'sleeplog.rds'))
  
  if (nrow (log) > 0) {
    
    #add weekday and then dplyr::rename ID to id to match other sheets
    trackl %>%
      dplyr::rename(id = ID) %>%
      dplyr::mutate (should.wd = weekdays(as.Date(should))) %>%
      dplyr::select (-time) %>%
      dplyr::mutate (time = "21:00:00") %>%
      dplyr::mutate (s_rep.shoulddt = as.character(as.POSIXct(paste(.$s_rep.should, .$time), format="%Y-%m-%d %H:%M:%S"))) -> trackl
    
    saveRDS (trackl, file.path(path, 'track.rds'))
    
    log <- readRDS (file.path(path, "sleeplog.rds"))
    other <- readRDS (file.path(path, "otherlog.rds"))
    track <- readRDS (file.path(path, "track.rds"))
    
    log %>%
      dplyr::mutate (match = actual) -> log
    track %>%
      dplyr::mutate (match = should) -> track
    other %>%
      dplyr::mutate (match = d.rep_actual.adj) -> other
    
    merge1 <- merge (track, log, by = c("id", "match"), all=TRUE)
    
    # tag extraneous rows
    merge1 %>%
      arrange(actual) %>%
      mutate(day_filled = case_when(!is.na(day) ~ day, 
                                    is.na(day) & !is.na(actual) ~ max(day, na.rm = TRUE) + cumsum(is.na(day) & !is.na(actual)),
                                    TRUE ~ NA_real_)) -> merge1
    
    merge1 %>%
      mutate(day = day_filled) %>%
      select(-day_filled) -> merge1
    
    merge1 %>%
      arrange(day) -> merge1

    #compliance alert
    # merge1 %>%
    #   dplyr::mutate (s_rep.shoulddt = as.POSIXct(s_rep.shoulddt, format="%Y-%m-%d %H:%M:%S"),
    #                  rawcompdt= as.POSIXct(rawcompdt, format="%Y-%m-%d %H:%M:%S")) %>%
    #   dplyr::mutate (s_rep_diff = difftime(rawcompdt, s_rep.shoulddt, units = "hours")) %>%
    #   dplyr::mutate (sleep_compliance = dplyr::case_when (s_rep_diff < 16 ~ "ok",
    #                                                       s_rep_diff >= 16 & s_rep_diff <= 24.99972 ~ "late:prob no good",
    #                                                       s_rep_diff > 24.99972 ~ "way late:noncompliant",
    #                                                       day == 8 ~ NA_character_,
    #                                                       is.na(day)== TRUE ~ NA_character_,
    #                                                       TRUE~ "missed")) -> merge1
    
    # new definition for lateness based on sept 2020 email exchanges with edith and lauren
    # 9pm to 5am “ok”, 5am to noon “late: before noon”, noon-9pm “late: past noon”
    merge1 %>%
      dplyr::mutate (s_rep.shoulddt = as.POSIXct(s_rep.shoulddt, format="%Y-%m-%d %H:%M:%S"),
                     rawcompdt= as.POSIXct(rawcompdt, format="%Y-%m-%d %H:%M:%S")) %>%
      dplyr::mutate (s_rep_diff = difftime(rawcompdt, s_rep.shoulddt, units = "hours")) %>%
      dplyr::mutate (sleep_compliance = dplyr::case_when (s_rep_diff < 9 ~ "ok",
                                                          s_rep_diff >= 9 & s_rep_diff < 16 ~ "late:before noon",
                                                          s_rep_diff >= 16 ~ "late:past noon",
                                                          day == 8 ~ NA_character_,
                                                          is.na(day)== TRUE ~ NA_character_,
                                                          TRUE~ "missed")) -> merge1
    
    #binging alert
    #binge defined by two entries indexing the same sleep date
    merge1 %>%
      dplyr::mutate (inbed.rowdiff = dplyr::case_when(is.na(should)==F~ should - dplyr::lag(should),
                                                      is.na(should)==T~ actual - dplyr::lag(actual)),
                     inbed.rowdiff2 = dplyr::case_when(is.na(should)==F~ should - dplyr::lead(should),
                                                       is.na(should)==T~ actual - dplyr::lag(actual))) %>%
      dplyr::mutate (binge = dplyr::case_when (inbed.rowdiff == 0 | inbed.rowdiff2 == 0 ~ "binge",
                                               is.na(inbed.rowdiff)==TRUE & is.na(inbed.rowdiff2)==TRUE ~ NA_character_, 
                                               TRUE ~ "ok")) -> merge1
    
    # for the extraneous days
    merge1 %>%
      mutate(binge = case_when(day > 8 & is.na(inbed.rowdiff) & is.na(inbed.rowdiff2) ~ lead(binge),
                               TRUE ~ binge)) -> merge1
    
    # default keep only the first one of a binge sequence 
    merge1 %>%
      mutate(binge_prev = lag(binge),
             bingekeep = case_when(binge == "ok" ~ 1,
                                   binge == "binge" & binge_prev != "binge" ~ 1,
                                   TRUE ~ 0)) %>%
      select(-binge_prev) -> merge1
    
    merge1 %>% dplyr::filter (bingekeep == 0) %>% dplyr::select (ResponseId) -> bingebaddays
    bingebaddays <- bingebaddays[,1]
    
    merge1 %>%
      dplyr::filter (binge== "ok"| is.na(binge)==TRUE|(binge == "binge" & bingekeep == 1)) -> merge1
    
    #combine compliance with binge
    merge1 %>%
      dplyr::mutate (sleep_compliance = dplyr::case_when (binge == "binge" ~ "binge survivor",
                                                          TRUE~ sleep_compliance)) -> merge1
    
    
    #need to apply this treatment to "other" sheet
    other %>% dplyr::filter (!ResponseId %in% bingebaddays) -> other
    
    #merge together
    merge2 <- merge (merge1, select(other, -ResponseId), by = c("id", "match"), all=TRUE)
    
    
      merge2 %>%
        dplyr::mutate (duration_sum_human = duration_sum) %>%
        dplyr::rename (date = match,
                       weekday = should.wd) %>% 
        dplyr::mutate (day = dplyr::case_when(is.na(day)==FALSE~ paste("day", day, sep = " "),
                                              is.na(day)==TRUE~ "day extra")) %>%
        dplyr::select (id, day, date, weekday, sleep_compliance, BedTime, WakeTime, NumRemove:duration_sum, duration_sum_human, nap:med_text) %>%
        dplyr::mutate (day = dplyr::case_when (is.na(day)==TRUE~ "extra",
                                               TRUE~ as.character(day)),
                       weekday = dplyr::case_when (is.na(weekday)==TRUE~ "extra",
                                                   TRUE~ as.character(weekday))) -> merge2
      
    
    # daylight savings
    if (daylight == T) {
      
      #grab year
      year<- as.numeric(substr(as.Date(merge2$date)[1], 0, 4))
      
      # if(year == 2026) {
      #   dlstart = as.Date("2026-03-08")
      #   dlend   = as.Date("2026-11-01")
      # } else if(year == 2027) {
      #   dlstart = as.Date("2027-03-14")
      #   dlend   = as.Date("2027-11-07")
      # } else if(year == 2028) {
      #   dlstart = as.Date("2028-03-12")
      #   dlend   = as.Date("2028-11-05")
      # } else if(year == 2029) {
      #   dlstart = as.Date("2029-03-11")
      #   dlend   = as.Date("2029-11-04")
      # } else if(year == 2030) {
      #   dlstart = as.Date("2030-03-10")
      #   dlend   = as.Date("2030-11-03")
      # }
    
      # for bedtime
      # note: dlend and dlstart done together because mutually exclusive 
        merge2 %>%
        dplyr::mutate(beddl = dplyr::case_when(is.na(BedTime)==F & date == dlend-1 &
                                   (lubridate::am(strptime(BedTime, "%I : %M   %p"))== TRUE & lubridate::hour(strptime(BedTime, "%I : %M   %p"))>=2)~ 1,
                                 is.na(BedTime)==F & date > dlend-1~ 1,
                                 is.na(BedTime)==F & date == dlstart-1 &
                                   (lubridate::am(strptime(BedTime, "%I : %M   %p"))== TRUE & lubridate::hour(strptime(BedTime, "%I : %M   %p"))>=2)~ 1,
                                 is.na(BedTime)==F & lubridate::month(as.Date(date)) == 3 & date > dlstart-1~ 1,
                                 TRUE~0))-> merge2
        
      # for waketime
      merge2 %>%
        dplyr::mutate (wakedl = dplyr::case_when(is.na(WakeTime)==F & date >= dlend-1~1, #assumes didn't wake up before 2am of dl saving date
                                   is.na(WakeTime)==F & lubridate::month(as.Date(date)) == 3 & date >= dlstart-1~1,
                                   TRUE~ 0)) -> merge2
      
      # remove and put on
      merge2 %>%
        dplyr::mutate_at (dplyr::vars(Remove1, Remove2, Remove3, Remove4),
                   list(dl= ~dplyr::case_when(as.character(.) == " :    " | is.na(.)==T~0,
                                       is.na(.)==F & (date == dlend | date == dlstart) & 
                                         (lubridate::pm(strptime(., "%I : %M   %p")) == TRUE | 
                                            lubridate::am(strptime(., "%I : %M   %p"))== TRUE & lubridate::hour(strptime(., "%I : %M   %p"))>=2)~ 1,
                                       is.na(.)==F & (date > dlend | (lubridate::month(as.Date(date)) == 3 & date > dlstart))~ 1,
                                       TRUE~ 0))) -> merge2
      
      merge2 %>%
        dplyr::mutate_at (dplyr::vars(PutOn1, PutOn2, PutOn3, PutOn4),
                   list(dl= ~dplyr::case_when(as.character(.) == " :    " | is.na(.)==T~0,
                                       is.na(.)==F & (date == dlend | date == dlstart) & 
                                         (lubridate::pm(strptime(., "%I : %M   %p")) == TRUE | 
                                            lubridate::am(strptime(., "%I : %M   %p"))== TRUE & lubridate::hour(strptime(., "%I : %M   %p"))>=2)~ 1,
                                       is.na(.)==F & (date > dlend | (lubridate::month(as.Date(date)) == 3 & date > dlstart))~ 1, 
                                       TRUE~ 0))) -> merge2
      
      # depending on dlstart or dlend, +1 or -1 accordingly
      merge2 %>%
        dplyr::mutate (bed_adjdl = dplyr::case_when(beddl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11) ~ strptime(BedTime, "%I : %M   %p") + lubridate::hours(1),
                                      beddl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3) ~ strptime(BedTime, "%I : %M   %p") - lubridate::hours(1),
                                      beddl == 0~ strptime(BedTime, "%I : %M   %p")),
                wake_adjdl = dplyr::case_when(wakedl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(WakeTime, "%I : %M   %p") + lubridate::hours(1),
                                       wakedl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(WakeTime, "%I : %M   %p") - lubridate::hours(1),
                                       wakedl == 0~ strptime(WakeTime, "%I : %M   %p")),
                Remove1_dl_adjdl = dplyr::case_when(Remove1_dl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(Remove1, "%I : %M   %p") + lubridate::hours(1),
                                             Remove1_dl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(Remove1, "%I : %M   %p") - lubridate::hours(1),
                                             Remove1_dl == 0 ~ strptime(Remove1, "%I : %M   %p")),
                Remove2_dl_adjdl = dplyr::case_when(Remove2_dl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(Remove2, "%I : %M   %p") + lubridate::hours(1),
                                             Remove2_dl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(Remove2, "%I : %M   %p") - lubridate::hours(1),
                                             Remove2_dl == 0~ strptime(Remove2, "%I : %M   %p")),
                Remove3_dl_adjdl = dplyr::case_when(Remove3_dl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(Remove3, "%I : %M   %p") + lubridate::hours(1),
                                             Remove3_dl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(Remove3, "%I : %M   %p") - lubridate::hours(1),
                                             Remove3_dl == 0~ strptime(Remove3, "%I : %M   %p")),
                Remove4_dl_adjdl = dplyr::case_when(Remove4_dl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(Remove4, "%I : %M   %p") + lubridate::hours(1),
                                             Remove4_dl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(Remove4, "%I : %M   %p") - lubridate::hours(1),
                                             Remove4_dl == 0~ strptime(Remove4, "%I : %M   %p")),
                PutOn1_dl_adjdl = dplyr::case_when(PutOn1_dl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(PutOn1, "%I : %M   %p") + lubridate::hours(1),
                                            PutOn1_dl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(PutOn1, "%I : %M   %p") - lubridate::hours(1),
                                            PutOn1_dl == 0~ strptime(PutOn1, "%I : %M   %p")),
                PutOn2_dl_adjdl = dplyr::case_when(PutOn2_dl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(PutOn2, "%I : %M   %p") + lubridate::hours(1),
                                            PutOn2_dl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(PutOn2, "%I : %M   %p") - lubridate::hours(1),
                                            PutOn2_dl == 0~ strptime(PutOn2, "%I : %M   %p")),
                PutOn3_dl_adjdl = dplyr::case_when(PutOn3_dl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(PutOn3, "%I : %M   %p") + lubridate::hours(1),
                                            PutOn3_dl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(PutOn3, "%I : %M   %p") - lubridate::hours(1),
                                            PutOn3_dl == 0~ strptime(PutOn3, "%I : %M   %p")),
                PutOn4_dl_adjdl = dplyr::case_when(PutOn4_dl == 1 & lubridate::month(as.Date(date)) %in% c(10, 11)~ strptime(PutOn4, "%I : %M   %p") + lubridate::hours(1),
                                            PutOn4_dl == 1 & lubridate::month(as.Date(date)) %in% c(2, 3)~ strptime(PutOn4, "%I : %M   %p") - lubridate::hours(1),
                                            PutOn4_dl == 0~ strptime(PutOn4, "%I : %M   %p"))) %>%
        dplyr::mutate (bed_adjdl_char = strftime(bed_adjdl, "%I : %M   %p"),
                wake_adjdl_char = strftime(wake_adjdl, "%I : %M   %p"),
                re1_adjdl_char = strftime(Remove1_dl_adjdl, "%I : %M   %p"),
                re2_adjdl_char = strftime(Remove2_dl_adjdl, "%I : %M   %p"),
                re3_adjdl_char = strftime(Remove3_dl_adjdl, "%I : %M   %p"),
                re4_adjdl_char = strftime(Remove4_dl_adjdl, "%I : %M   %p"),
                po1_adjdl_char = strftime(PutOn1_dl_adjdl, "%I : %M   %p"),
                po2_adjdl_char = strftime(PutOn2_dl_adjdl, "%I : %M   %p"),
                po3_adjdl_char = strftime(PutOn3_dl_adjdl, "%I : %M   %p"),
                po4_adjdl_char = strftime(PutOn4_dl_adjdl, "%I : %M   %p")) -> merge2
      
      #rename the adj as bedtime and waketime and remove auxillary columns
      merge2 %>%
        dplyr::mutate(BedTime =  bed_adjdl_char ,
               WakeTime = wake_adjdl_char,
               Remove1 =  re1_adjdl_char ,
               Remove2 =  re2_adjdl_char ,
               Remove3 =  re3_adjdl_char ,
               Remove4 =  re4_adjdl_char ,
               PutOn1 =   po1_adjdl_char ,
               PutOn2 =   po2_adjdl_char ,
               PutOn3 =   po3_adjdl_char ,
               PutOn4 =   po4_adjdl_char  ) %>%
        dplyr::select (id:med_text, beddl, wakedl, Remove1_dl, PutOn1_dl,
                Remove2_dl, PutOn2_dl, Remove3_dl, PutOn3_dl,
                Remove4_dl, PutOn4_dl) -> merge2
      
    }
    
    tmerge2 <- t(merge2)
    
    openxlsx::write.xlsx(tmerge2,
      file = file.path(path, paste0(study, " V", visit, " Sleep Log ", id, ".xlsx")),
      rowNames = TRUE,
      colNames = FALSE,
      keepNA = FALSE)
    
    
    return ("done rolling!")
  } else {
    return ("no sleep log generated: do not have at least one entry")
  }
  
}     

# library(dplyr)
# library(openxlsx)
# sleeplog(path = '/Users/phoebelam/Desktop/sleep',
#          filename_common_string = 'NIH+CON+V1+Daily+Diary+Day',
#          study = 'cons',
#          visit = 1,
#          id = 3568794,
#          tracker_filename = 'NIH CON Actigraphy Tracking')





