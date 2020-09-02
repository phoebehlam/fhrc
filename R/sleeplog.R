#'@importFrom magrittr "%>%"
#'@export
sleeplog <- function(path, id, Study, visit) {
  
  # library (xlsx)
  # library(dplyr)
  # library(tidyr)
  # library(gtools)
  # library (lubridate)
  
  #troubleshoot
  # file <- read.csv("MHS/MHS V1 Daily Diary Day 1_August 19, 2020_16.42.csv", header = T)
  # day8check = 1

  setwd (path)
  log <- data.frame(matrix(ncol = 1, nrow = 1))
  other <- data.frame(matrix(ncol = 1, nrow = 1))
  xlsx::write.xlsx(log, "sleeplog.xlsx", row.names = FALSE)
  xlsx::write.xlsx(other, "otherlog.xlsx", row.names = FALSE)
  
  filenames = intersect(list.files(path = path, pattern = Study, full.names = TRUE, recursive = TRUE), 
                        list.files(path = path, pattern = ".csv", full.names = TRUE, recursive = TRUE))
  
  
  for (f in filenames) { # do not run this line when we troubleshooting
    print (f)
    file <- read.csv (f, header= TRUE) # skip this for troubleshooting
    
    file %>%
      dplyr::mutate (firstname.check = as.numeric(as.character(RecipientFirstName)),
                     lastname.check = as.numeric(as.character(RecipientLastName)),
                     externalref.check = as.numeric(as.character(ExternalReference))) -> file
    
    file %>%
      dplyr::mutate(goodid = dplyr::case_when(is.na(externalref.check)==F~ as.numeric(as.character(ExternalReference)),
                                              is.na(externalref.check)==T & is.na(lastname.check)==F~ as.numeric(as.character(RecipientLastName)),
                                              is.na(externalref.check)==T & is.na(lastname.check)==T & is.na(firstname.check)==F~ as.numeric(as.character(RecipientFirstName)))) -> file
    
    if (any (file$goodid == id, na.rm=T) == TRUE) { #skip for troubleshooting
      
      #grab day number from the file name (skip for troubleshooting)
      basename(f) %>%
        gsub ("OTR V1 Daily Diary Day ", "", .) %>%
        gsub ("OTR V2 Daily Diary Day ", "", .) %>%
        gsub ("V2 OTR Daily Diary Day ", "", .) %>%
        gsub ("OTR Daily Diary Day ", "", .) %>%
        gsub ("MHS V1 Daily Diary Day ", "", .) %>%
        gsub ("MHS V2 Daily Diary Day ", "", .) %>%
        substr(., 0, 1) -> day8check
      
      file %>%
        dplyr::mutate (qualtrics_day = day8check) -> file
      
      #for all days except day 8, do the following (SKIP THIS LINE BUT RUN THE NEXT ONE)
      if (day8check != 8) {
        file %>% dplyr::select (., qualtrics_day, StartDate, EndDate, goodid, BedTime.1_1:med_text) %>%
          dplyr::filter (.,goodid == id )-> file
        
        
        #fixing the date participant do the survey into tidyr::tidyr::separate date and time cols
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
          dplyr::mutate (s.rep_actual.adj = dplyr::case_when (hour < 21 ~ as.Date(Date)-1,
                                                              TRUE ~ as.Date(Date))) %>%
          dplyr::mutate (actual = dplyr::case_when (hour < 21 ~ as.Date(Date)-2,
                                                    TRUE ~ as.Date(Date) - 1)) -> file
        
        file$actual.wd <- weekdays(as.Date(file$actual))
        file$s_rep.actual_weekday <- weekdays(as.Date(file$s.rep_actual.adj))
        
        
        #bedtime/waketime hr, min, am/pm into one cell
        file$BedTime <- paste (file$BedTime.1_1, ":", file$BedTime.2_1)
        file$BedTime <- paste (file$BedTime, " ", file$BedTime.3_1)
        file$WakeTime <- paste (file$WakeTime.1_1, ":", file$WakeTime.2_1)
        file$WakeTime <- paste (file$WakeTime, " ", file$WakeTime.3_1)
        
        #remove/puton hr, min, am/pm into one cell
        file %>%
          dplyr::mutate (Remove1 = paste(Remove1.1_1, ":", Remove1.2_1, " ", Remove1.3_1),
                         Remove2 = paste(Remove2.1_1, ":", Remove2.2_1, " ", Remove2.3_1),
                         Remove3 = paste(Remove3.1_1, ":", Remove3.2_1, " ", Remove3.3_1),
                         Remove4 = paste(Remove4.1_1, ":", Remove4.2_1, " ", Remove4.3_1),
                         PutOn1 = paste(PutOn1.1_1, ":", PutOn1.2_1, " ", PutOn1.3_1),
                         PutOn2 = paste(PutOn2.1_1, ":", PutOn2.2_1, " ", PutOn2.3_1),
                         PutOn3 = paste(PutOn3.1_1, ":", PutOn3.2_1, " ", PutOn3.3_1),
                         PutOn4 = paste(PutOn4.1_1, ":", PutOn4.2_1, " ", PutOn4.3_1)) %>%
          dplyr::mutate (remove1t = as.POSIXct (Remove1, format = "%I : %M %p"),
                         remove2t = as.POSIXct (Remove2, format = "%I : %M %p"),
                         remove3t = as.POSIXct (Remove3, format = "%I : %M %p"),
                         remove4t = as.POSIXct (Remove4, format = "%I : %M %p"), 
                         puton1t = as.POSIXct (PutOn1, format = "%I : %M %p"), 
                         puton2t = as.POSIXct (PutOn2, format = "%I : %M %p"),
                         puton3t = as.POSIXct (PutOn3, format = "%I : %M %p"), 
                         puton4t = as.POSIXct (PutOn4, format = "%I : %M %p")) -> file
        
        #categorizing the 4 combinations of am/pm combo
        file %>%
          dplyr::mutate (ampmcheck1 = dplyr::case_when (as.character(Remove1.3_1) == "PM" & as.character(PutOn1.3_1) == "AM" ~ 1,
                                                        as.character(Remove1.3_1) == "AM" & as.character(PutOn1.3_1) == "PM" ~ 2,
                                                        as.character(Remove1.3_1) == "PM" & as.character(PutOn1.3_1) == "PM" ~ 3,
                                                        as.character(Remove1.3_1) == "AM" & as.character(PutOn1.3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck2 = dplyr::case_when (as.character(Remove2.3_1) == "PM" & as.character(PutOn2.3_1) == "AM" ~ 1,
                                                        as.character(Remove2.3_1) == "AM" & as.character(PutOn2.3_1) == "PM" ~ 2,
                                                        as.character(Remove2.3_1) == "PM" & as.character(PutOn2.3_1) == "PM" ~ 3,
                                                        as.character(Remove2.3_1) == "AM" & as.character(PutOn2.3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck3 = dplyr::case_when (as.character(Remove3.3_1) == "PM" & as.character(PutOn3.3_1) == "AM" ~ 1,
                                                        as.character(Remove3.3_1) == "AM" & as.character(PutOn3.3_1) == "PM" ~ 2,
                                                        as.character(Remove3.3_1) == "PM" & as.character(PutOn3.3_1) == "PM" ~ 3,
                                                        as.character(Remove3.3_1) == "AM" & as.character(PutOn3.3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck4 = dplyr::case_when (as.character(Remove4.3_1) == "PM" & as.character(PutOn4.3_1) == "AM" ~ 1,
                                                        as.character(Remove4.3_1) == "AM" & as.character(PutOn4.3_1) == "PM" ~ 2,
                                                        as.character(Remove4.3_1) == "PM" & as.character(PutOn4.3_1) == "PM" ~ 3,
                                                        as.character(Remove4.3_1) == "AM" & as.character(PutOn4.3_1) == "AM" ~ 4,
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
          dplyr::select(id, qualtrics_day, actual.wd, actual,
                        BedTime, WakeTime,
                        rawcompdt, rawcompdate, rawcomptime) -> file1
        
        log <- xlsx::read.xlsx2 ("sleeplog.xlsx", sheetIndex = 1, startRow=1)
        log <- gtools::smartbind (log, file1)
        xlsx::write.xlsx(log, "sleeplog.xlsx", row.names = FALSE)
        
        file %>% 
          dplyr::rename (id = goodid) %>% 
          dplyr::mutate (hour= as.numeric(as.character(hour))) %>%
          dplyr::mutate (d.rep_actual.adj = dplyr::case_when (hour < 21 ~ as.Date(Date)-1,
                                                              TRUE ~ as.Date(Date))) %>%
          dplyr::select (id, qualtrics_day, NumRemove, Remove1, PutOn1, RemoveReason1,
                         Remove2, PutOn2, RemoveReason2, 
                         Remove3, PutOn3, RemoveReason3,
                         Remove4, PutOn4, RemoveReason4, duration_sum, nap:med_text, d.rep_actual.adj) -> file2
        
        
        other <- xlsx::read.xlsx2 ("otherlog.xlsx", sheetIndex = 1, startRow=1)
        other <- gtools::smartbind (other, file2)
        xlsx::write.xlsx (other, "otherlog.xlsx", row.names = FALSE)
        
      } else { #skip this one line, but do the next one
        
        file %>% dplyr::select (., qualtrics_day, StartDate, EndDate, goodid, BedTime.1_1:RemoveReason4) %>%
          dplyr::filter (.,goodid == id )-> file
        
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
          dplyr::mutate (s.rep_actual.adj = as.Date(Date)) %>%
          dplyr::mutate (actual = as.Date(Date) - 1) -> file
        
        file$actual.wd <- weekdays(as.Date(file$actual))
        
        file$s_rep.actual_weekday <- weekdays(as.Date(file$s.rep_actual.adj))
        
        file$BedTime <- paste (file$BedTime.1_1, ":", file$BedTime.2_1)
        file$BedTime <- paste (file$BedTime, " ", file$BedTime.3_1)
        file$WakeTime <- paste (file$WakeTime.1_1, ":", file$WakeTime.2_1)
        file$WakeTime <- paste (file$WakeTime, " ", file$WakeTime.3_1)
        
        #remove/puton hr, min, am/pm into one cell
        file %>%
          dplyr::mutate (Remove1 = paste(Remove1.1_1, ":", Remove1.2_1, " ", Remove1.3_1),
                         Remove2 = paste(Remove2.1_1, ":", Remove2.2_1, " ", Remove2.3_1),
                         Remove3 = paste(Remove3.1_1, ":", Remove3.2_1, " ", Remove3.3_1),
                         Remove4 = paste(Remove4.1_1, ":", Remove4.2_1, " ", Remove4.3_1),
                         PutOn1 = paste(PutOn1.1_1, ":", PutOn1.2_1, " ", PutOn1.3_1),
                         PutOn2 = paste(PutOn2.1_1, ":", PutOn2.2_1, " ", PutOn2.3_1),
                         PutOn3 = paste(PutOn3.1_1, ":", PutOn3.2_1, " ", PutOn3.3_1),
                         PutOn4 = paste(PutOn4.1_1, ":", PutOn4.2_1, " ", PutOn4.3_1)) %>%
          dplyr::mutate (remove1t = as.POSIXct (Remove1, format = "%I : %M %p"),
                         remove2t = as.POSIXct (Remove2, format = "%I : %M %p"),
                         remove3t = as.POSIXct (Remove3, format = "%I : %M %p"),
                         remove4t = as.POSIXct (Remove4, format = "%I : %M %p"), 
                         puton1t = as.POSIXct (PutOn1, format = "%I : %M %p"), 
                         puton2t = as.POSIXct (PutOn2, format = "%I : %M %p"),
                         puton3t = as.POSIXct (PutOn3, format = "%I : %M %p"), 
                         puton4t = as.POSIXct (PutOn4, format = "%I : %M %p")) -> file
        
        #categorizing the 4 combinations of am/pm combo
        file %>%
          dplyr::mutate (ampmcheck1 = dplyr::case_when (as.character(Remove1.3_1) == "PM" & as.character(PutOn1.3_1) == "AM" ~ 1,
                                                        as.character(Remove1.3_1) == "AM" & as.character(PutOn1.3_1) == "PM" ~ 2,
                                                        as.character(Remove1.3_1) == "PM" & as.character(PutOn1.3_1) == "PM" ~ 3,
                                                        as.character(Remove1.3_1) == "AM" & as.character(PutOn1.3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck2 = dplyr::case_when (as.character(Remove2.3_1) == "PM" & as.character(PutOn2.3_1) == "AM" ~ 1,
                                                        as.character(Remove2.3_1) == "AM" & as.character(PutOn2.3_1) == "PM" ~ 2,
                                                        as.character(Remove2.3_1) == "PM" & as.character(PutOn2.3_1) == "PM" ~ 3,
                                                        as.character(Remove2.3_1) == "AM" & as.character(PutOn2.3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck3 = dplyr::case_when (as.character(Remove3.3_1) == "PM" & as.character(PutOn3.3_1) == "AM" ~ 1,
                                                        as.character(Remove3.3_1) == "AM" & as.character(PutOn3.3_1) == "PM" ~ 2,
                                                        as.character(Remove3.3_1) == "PM" & as.character(PutOn3.3_1) == "PM" ~ 3,
                                                        as.character(Remove3.3_1) == "AM" & as.character(PutOn3.3_1) == "AM" ~ 4,
                                                        TRUE ~ NA_real_),
                         ampmcheck4 = dplyr::case_when (as.character(Remove4.3_1) == "PM" & as.character(PutOn4.3_1) == "AM" ~ 1,
                                                        as.character(Remove4.3_1) == "AM" & as.character(PutOn4.3_1) == "PM" ~ 2,
                                                        as.character(Remove4.3_1) == "PM" & as.character(PutOn4.3_1) == "PM" ~ 3,
                                                        as.character(Remove4.3_1) == "AM" & as.character(PutOn4.3_1) == "AM" ~ 4,
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
          dplyr::select(id, qualtrics_day, actual.wd, actual,
                        BedTime, WakeTime,
                        rawcompdt, rawcompdate, rawcomptime) -> file1
        
        log <- xlsx::read.xlsx2 ("sleeplog.xlsx", sheetIndex = 1, startRow=1)
        log <- gtools::smartbind (log, file1)
        xlsx::write.xlsx(log, "sleeplog.xlsx", row.names = FALSE)
        
        file %>% 
          dplyr::rename (id = goodid) %>% 
          dplyr::mutate (d.rep_actual.adj = as.Date(Date)) %>% #no adjustment for day 8
          dplyr::select (id, qualtrics_day, NumRemove, Remove1, PutOn1, RemoveReason1,
                         Remove2, PutOn2, RemoveReason2, 
                         Remove3, PutOn3, RemoveReason3,
                         Remove4, PutOn4, RemoveReason4, duration_sum, d.rep_actual.adj) -> file2
        
        other <- xlsx::read.xlsx2 ("otherlog.xlsx", sheetIndex = 1, startRow=1)
        other <- gtools::smartbind (other, file2)
        xlsx::write.xlsx (other, "otherlog.xlsx", row.names = FALSE)
        
      }
    }
    
    
    
  }
  
  # how if functions work, key is that "else if" needs to immediately follow the close bracket } of if
  # a=10
  # 
  # if (a == 5) {
  #   print ("yay!")
  # } else if (a == 3) {
  #   print ("nay!")
  # }
  # 
  # if (a==5) {print ("yay")} else if (a==3) {print ("nay!")}
  
  substr(id, 0, 1) -> mhsid #1 = mentors, 2= mentees
  
  #AUBREY'S HOMEWORK IS TO ADAPT PATH TO BE USER-INPUT PATH
  #can try different combinations here as well, just make sure to delete, or turn them into comments, before you run the entire code
  # Study = "OTR"
  # visit = 2
  
  #RUN THE IF FOR THE TROUBLESHOOT TO SEE WHETHER THE "IF" LOGIC WORKS
  #OTRV1
  if (Study == "OTR" & visit == 1)  {
    track <- xlsx::read.xlsx (paste(path, "/OTR/OTR DRI Actigraphy Tracking.xlsx", sep=""), startRow = 3, header = TRUE, sheetName = "V1 Actigraphy")
    
    #OTRV2
  }else if (Study == "OTR" & visit == 2)  {
    track <- xlsx::read.xlsx (paste(path, "/OTR/OTR DRI Actigraphy Tracking.xlsx", sep=""), startRow = 3, header = TRUE, sheetName = "V2 Actigraphy")
    
    #MHS Mentor V1  
  }else if (Study == "MHS" & mhsid==1 & visit == 1 ) {
    track <- xlsx::read.xlsx (paste(path, "/MHS/MHS Actigraphy Tracking.xlsx", sep=""), startRow = 3, header = TRUE, sheetName = "Mentor V1")
    
    #MHS Mentor V2
  }else if (Study == "MHS" & mhsid==1 & visit == 1 ) {
    track <- xlsx::read.xlsx (paste(path, "/MHS/MHS Actigraphy Tracking.xlsx", sep=""), startRow = 3, header = TRUE, sheetName = "Mentor V2")
    
    #MHS Mentee V1
  }else if (Study == "MHS" & mhsid==2 & visit == 1 ) {
    track <- xlsx::read.xlsx (paste(path, "/MHS/MHS Actigraphy Tracking.xlsx", sep=""), startRow = 3, header = TRUE, sheetName = "Mentee V1")
    
    #MHS Mentee V2  
  }else if (Study == "MHS" & mhsid==2 & visit == 2 ) {
    track <- xlsx::read.xlsx (paste(path, "/MHS/MHS Actigraphy Tracking.xlsx", sep=""), startRow = 3, header = TRUE, sheetName = "Mentee V2")
  }
  
  track %>%
    dplyr::rename (should0 = Lab.Visit.Date,
            dd1log = DD.Day.1..Night.) %>%
    dplyr::filter (ID == id) -> track
  
  if (track$should0+1 == track$dd1log) {
    track %>%
      dplyr::mutate (should1 = should0 + 1, #date associated with each study day
                     should2 = should0 + 2,
                     should3 = should0 + 3,
                     should4 = should0 + 4,
                     should5 = should0 + 5,
                     should6 = should0 + 6,
                     should7 = should0 + 7,
                     should8 = should0 + 8) -> track
  } else if (track$should0+1 != track$dd1log) {
    track %>%
      dplyr::mutate (should0 = dd1log-1,
                     should1 = dd1log, 
                     should2 = dd1log + 1,
                     should3 = dd1log + 2,
                     should4 = dd1log + 3,
                     should5 = dd1log + 4,
                     should6 = dd1log + 5,
                     should7 = dd1log + 6,
                     should8 = dd1log + 7) -> track
  }
  
  track %>%
    dplyr::mutate (s_rep.should0 = should1, #date they should have reported sleep for each study day
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
  log <- xlsx::read.xlsx2 ("sleeplog.xlsx", sheetIndex = 1, startRow=1) [-1,-1]
  
  
  if (nrow (log) > 0) {
    
    #add weekday and then dplyr::rename ID to id to match other sheets
    trackl %>%
      dplyr::mutate (should.wd = weekdays(as.Date(should))) %>%
      dplyr::rename (id = ID) %>%
      dplyr::select (-time) %>%
      dplyr::mutate (time = "21:00:00") %>%
      dplyr::mutate (s_rep.shoulddt = as.character(as.POSIXct(paste(.$s_rep.should, .$time), format="%Y-%m-%d %H:%M:%S"))) -> trackl
    
    xlsx::write.xlsx (trackl, "track.xlsx", row.names = FALSE)
    
    
    log <- xlsx::read.xlsx2 ("sleeplog.xlsx", sheetIndex = 1, startRow=1) [-1,-1]
    other <- xlsx::read.xlsx2 ("otherlog.xlsx", sheetIndex = 1, startRow=1) [-1,-1]
    track <- xlsx::read.xlsx ("track.xlsx", sheetIndex = 1)
    
    log %>%
      dplyr::mutate (match = actual) -> log
    track %>%
      dplyr::mutate (match = should) -> track
    other %>%
      dplyr::mutate (match = d.rep_actual.adj) -> other
    
    merge1 <- merge (track, log, by = c("id", "match"), all=TRUE)
    
    #compliance alert
    merge1 %>%
      dplyr::mutate (s_rep.shoulddt = as.POSIXct(s_rep.shoulddt, format="%Y-%m-%d %H:%M:%S"),
                     rawcompdt= as.POSIXct(rawcompdt, format="%Y-%m-%d %H:%M:%S")) %>%
      dplyr::mutate (s_rep_diff = difftime(rawcompdt, s_rep.shoulddt, units = "hours")) %>%
      dplyr::mutate (sleep_compliance = dplyr::case_when (s_rep_diff < 16 ~ "ok",
                                                          s_rep_diff >= 16 & s_rep_diff <= 24.99972 ~ "late:prob no good",
                                                          s_rep_diff > 24.99972 ~ "way late:noncompliant",
                                                          day == 8 ~ NA_character_,
                                                          is.na(day)== TRUE ~ NA_character_,
                                                          TRUE~ "missed")) -> merge1
    
    #binging alert
    #binge defined by two entries indexing the same sleep date
    merge1 %>%
      dplyr::mutate (inbed.rowdiff = should - lag(should),
                     inbed.rowdiff2 = should - lead(should)) %>%
      dplyr::mutate (binge = dplyr::case_when (inbed.rowdiff == 0 | inbed.rowdiff2 == 0 ~ "binge",
                                               is.na(inbed.rowdiff)==TRUE & is.na(inbed.rowdiff2)==TRUE ~ NA_character_, 
                                               TRUE ~ "ok")) -> merge1
    
    #deciding on which binge version to keep
    merge1 %>%
      dplyr::mutate (qualtrics_day = as.numeric(as.character(qualtrics_day))) %>%
      dplyr::mutate (bingekeep = dplyr::case_when(binge == "binge" & qualtrics_day - 1 == day~ 1,
                                                  binge == "binge" & qualtrics_day - 1 != day ~ 0)) -> merge1
    
    merge1 %>% dplyr::filter (bingekeep == 0) %>% dplyr::select (qualtrics_day) -> bingebaddays
    
    bingebaddays <- bingebaddays[,1]
    
    merge1 %>%
      dplyr::filter (binge== "ok"| is.na(binge)==TRUE|(binge == "binge" & bingekeep == 1)) -> merge1
    
    
    #combine compliance with binge
    merge1 %>%
      dplyr::mutate (sleep_compliance = dplyr::case_when (binge == "binge" ~ "binge survivor",
                                                          TRUE~ sleep_compliance)) -> merge1
    
    
    #need to apply this treatment to "other" sheet
    other %>% dplyr::filter (!qualtrics_day %in% bingebaddays) -> other
    
    #merge together
    merge2 <- merge (merge1, other, by = c("id", "match"), all=TRUE)
    
    merge2 %>%
      dplyr::mutate (nap = dplyr::case_when (nap == "ERROR"~ NA_character_,
                                             TRUE~ as.character(nap)), 
                     sick = dplyr::case_when (sick == "ERROR"~ NA_character_,
                                              TRUE~ as.character(sick)),
                     med = dplyr::case_when (med == "ERROR"~ NA_character_,
                                             TRUE~ as.character(med)),
                     med_text = dplyr::case_when (med_text == "ERROR"~ NA_character_,
                                                  TRUE~ as.character(med_text))) %>%
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
    
    tmerge2 <- t(merge2)
    
    xlsx::write.xlsx(tmerge2, paste (Study," V", visit, " Sleep Log ", id, ".xlsx", sep =""), row.names = TRUE, col.names = FALSE, showNA = FALSE)
    
    return ("done rolling!")
  } else {
    return ("no sleep log generated: do not have at least one entry")
  }
  
  
  
  
}



