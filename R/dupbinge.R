#'report entries with duplicated entries
#'
#'@importFrom magrittr "%>%"
#'
#'@examples dup("/Users/phoebelam/Desktop/clean", "MHS")
#'
#'@export
dupbinge <- function (path, study, visit, exclude) {
  
  filenames = intersect(list.files(path = paste(path, "/", study, sep=""), pattern = study, full.names = TRUE, recursive = TRUE),
                        list.files(path = paste(path, "/", study, sep=""), pattern = ".csv", full.names = TRUE, recursive = TRUE))
  
  diary = lapply(filenames, read.csv)
  
  d1 <- as.data.frame(diary[[1]]) %>% tail (., -2)
  d2 <- as.data.frame(diary[[2]]) %>% tail (., -2)
  d3 <- as.data.frame(diary[[3]]) %>% tail (., -2)
  d4 <- as.data.frame(diary[[4]]) %>% tail (., -2)
  d5 <- as.data.frame(diary[[5]]) %>% tail (., -2)
  d6 <- as.data.frame(diary[[6]]) %>% tail (., -2)
  d7 <- as.data.frame(diary[[7]]) %>% tail (., -2)
  d8 <- as.data.frame(diary[[8]]) %>% tail (., -2)
  
  if (study == "OTR" & visit == 1) {
    varnames <- c("startdate", 
                  "enddate", 
                  "status", 
                  "ipaddress", 
                  "progress", 
                  "duration.sec", 
                  "finished", 
                  "recordeddate", 
                  "qualtricsid", 
                  "lastname", 
                  "id", 
                  "email", 
                  "extref", 
                  "latitude", 
                  "longitude", 
                  "channel", 
                  "language", 
                  "bedtime.1_1", 
                  "bedtime.2_1", 
                  "bedtime.3_1", 
                  "waketime.1_1", 
                  "waketime.2_1", 
                  "waketime.3_1", 
                  "numremove", 
                  "remove1.1_1", 
                  "remove1.2_1", 
                  "remove1.3_1", 
                  "puton1.1_1", 
                  "puton1.2_1", 
                  "puton1.3_1", 
                  "removereason1", 
                  "remove2.1_1", 
                  "remove2.2_1", 
                  "remove2.3_1", 
                  "puton2.1_1", 
                  "puton2.2_1", 
                  "puton2.3_1", 
                  "removereason2", 
                  "remove3.1_1", 
                  "remove3.2_1", 
                  "remove3.3_1", 
                  "puton3.1_1", 
                  "puton3.2_1", 
                  "puton3.3_1", 
                  "removereason3", 
                  "remove4.1_1", 
                  "remove4.2_1", 
                  "remove4.3_1", 
                  "puton4.1_1", 
                  "puton4.2_1", 
                  "puton4.3_1", 
                  "removereason4", 
                  "nap", 
                  "sick", 
                  "med_yn", 
                  "med_text", 
                  "atschool_yn", 
                  "atschoolhr_text", 
                  "atwork_yn", 
                  "atworkhr_text", 
                  "homework_yn", 
                  "extra.sport_yn", 
                  "tvmovies_yn", 
                  "phone_yn", 
                  "friends_yn", 
                  "family_yn", 
                  "outing_yn", 
                  "music_yn", 
                  "otheract_yn", 
                  "otheract_text", 
                  "homework_hour", 
                  "homework_min", 
                  "extra.sport_hour", 
                  "extra.sport_min", 
                  "tvmovies_hour", 
                  "tvmovies_min", 
                  "phone_hour", 
                  "phone_min", 
                  "friends_hour", 
                  "friends_min", 
                  "family_hour", 
                  "family_min", 
                  "outing_hour", 
                  "outing_min", 
                  "music_hour", 
                  "music_min", 
                  "otheract_hour", 
                  "otheract_min", 
                  "clean_yn", 
                  "caresib_yn", 
                  "errands_yn", 
                  "helpsib_yn", 
                  "cook_yn", 
                  "parentwork_yn", 
                  "famother_yn", 
                  "nofamob_yn", 
                  "famother_text", 
                  "clean_hour", 
                  "clean_min", 
                  "caresib_hour", 
                  "caresib_min", 
                  "errands_hour", 
                  "errands_min", 
                  "helpsib_hour", 
                  "helpsib_min", 
                  "cook_hour", 
                  "cook_min", 
                  "parentwork_hour", 
                  "parentwork_min", 
                  "famother_hour", 
                  "famother_min", 
                  "gencope1", 
                  "gencope2", 
                  "gencope3", 
                  "gencope4", 
                  "gencope5", 
                  "gencope6", 
                  "gencope7", 
                  "rude_yn", 
                  "ignore_yn", 
                  "punish_yn", 
                  "afraid_yn", 
                  "mean_yn", 
                  "names_yn", 
                  "nodiscrim_yn", 
                  "rude.race", 
                  "rude.class", 
                  "rude.gender", 
                  "rude.other_yn", 
                  "rude.unknown", 
                  "rude.other_text", 
                  "rude.context", 
                  "rude.context.other", 
                  "rude.who", 
                  "rude.who.other", 
                  "rude.cope1", 
                  "rude.cope2", 
                  "rude.cope3", 
                  "rude.cope4", 
                  "rude.cope5", 
                  "rude.cope6", 
                  "rude.cope7", 
                  "rude.cope8", 
                  "ignore.race", 
                  "ignore.class", 
                  "ignore.gender", 
                  "ignore.other_yn", 
                  "ignore.unknown", 
                  "ignore.other_text", 
                  "ignore.context", 
                  "ignore.context.other", 
                  "ignore.who", 
                  "ignore.who.other", 
                  "ignore.cope1", 
                  "ignore.cope2", 
                  "ignore.cope3", 
                  "ignore.cope4", 
                  "ignore.cope5", 
                  "ignore.cope6", 
                  "ignore.cope7", 
                  "ignore.cope8", 
                  "punish.race", 
                  "punish.class", 
                  "punish.gender", 
                  "punish.other_yn", 
                  "punish.unknown", 
                  "punish.other_text", 
                  "punish.context", 
                  "punish.context.other", 
                  "punish.who", 
                  "punish.who.other", 
                  "punish.cope1", 
                  "punish.cope2", 
                  "punish.cope3", 
                  "punish.cope4", 
                  "punish.cope5", 
                  "punish.cope6", 
                  "punish.cope7", 
                  "punish.cope8", 
                  "afraid.race", 
                  "afraid.class", 
                  "afraid.gender", 
                  "afraid.other_yn", 
                  "afraid.unknown", 
                  "afraid.other_text", 
                  "afraid.context", 
                  "afraid.context.other", 
                  "afraid.who", 
                  "afraid.who.other", 
                  "afraid.cope1", 
                  "afraid.cope2", 
                  "afraid.cope3", 
                  "afraid.cope4", 
                  "afraid.cope5", 
                  "afraid.cope6", 
                  "afraid.cope7", 
                  "afraid.cope8", 
                  "mean.race", 
                  "mean.class", 
                  "mean.gender", 
                  "mean.other_yn", 
                  "mean.unknown", 
                  "mean.other_text", 
                  "mean.context", 
                  "mean.context.other", 
                  "mean.who", 
                  "mean.who.other", 
                  "mean.cope1", 
                  "mean.cope2", 
                  "mean.cope3", 
                  "mean.cope4", 
                  "mean.cope5", 
                  "mean.cope6", 
                  "mean.cope7", 
                  "mean.cope8", 
                  "names.race", 
                  "names.class", 
                  "names.gender", 
                  "names.other_yn", 
                  "names.unknown", 
                  "names.other_text", 
                  "names.context", 
                  "names.context.other", 
                  "names.who", 
                  "names.who.other", 
                  "names.cope1", 
                  "names.cope2", 
                  "names.cope3", 
                  "names.cope4", 
                  "names.cope5", 
                  "names.cope6", 
                  "names.cope7", 
                  "names.cope8", 
                  "overload1", 
                  "overload2", 
                  "overload3", 
                  "overload4", 
                  "complete")
  } else if (study == "OTR" & visit ==2) {
    
    varnames <- c("startdate", 
                  "enddate", 
                  "status", 
                  "ipaddress", 
                  "progress", 
                  "duration.sec", 
                  "finished", 
                  "recordeddate", 
                  "qualtricsid", 
                  "lastname", 
                  "id", 
                  "email", 
                  "extref", 
                  "latitude", 
                  "longitude", 
                  "channel", 
                  "language", 
                  "bedtime.1_1", 
                  "bedtime.2_1", 
                  "bedtime.3_1", 
                  "waketime.1_1", 
                  "waketime.2_1", 
                  "waketime.3_1", 
                  "numremove", 
                  "remove1.1_1", 
                  "remove1.2_1", 
                  "remove1.3_1", 
                  "puton1.1_1", 
                  "puton1.2_1", 
                  "puton1.3_1", 
                  "removereason1", 
                  "remove2.1_1", 
                  "remove2.2_1", 
                  "remove2.3_1", 
                  "puton2.1_1", 
                  "puton2.2_1", 
                  "puton2.3_1", 
                  "removereason2", 
                  "remove3.1_1", 
                  "remove3.2_1", 
                  "remove3.3_1", 
                  "puton3.1_1", 
                  "puton3.2_1", 
                  "puton3.3_1", 
                  "removereason3", 
                  "remove4.1_1", 
                  "remove4.2_1", 
                  "remove4.3_1", 
                  "puton4.1_1", 
                  "puton4.2_1", 
                  "puton4.3_1", 
                  "removereason4", 
                  "nap", 
                  "sick", 
                  "med_yn", 
                  "med_text",
                  "school_hour",
                  "school_min",
                  "work_hour",
                  "work_min",
                  "homework_hour",
                  "homework_min",
                  "extra_hour",
                  "extra_min",
                  "relax_hour",
                  "relax_min",
                  "clean_yn", 
                  "caresib_yn", 
                  "errands_yn", 
                  "helpsib_yn", 
                  "cook_yn", 
                  "parentwork_yn", 
                  "famother_yn", 
                  "nofamob_yn", 
                  "famother_text", 
                  "famassist_hour",
                  "famassist_min",
                  "rude",
                  "ignored",
                  "punished",
                  "afraidof",
                  "mean",
                  "names",
                  "overload1", 
                  "overload2", 
                  "overload3", 
                  "overload4", 
                  "complete")
    
  } else if (study == "OTR" & visit == 3) {
    
    varnames <- c("startdate", 
                  "enddate", 
                  "status", 
                  "ipaddress", 
                  "progress", 
                  "duration.sec", 
                  "finished", 
                  "recordeddate", 
                  "qualtricsid", 
                  "lastname", 
                  "id", 
                  "email", 
                  "extref", 
                  "latitude", 
                  "longitude", 
                  "channel", 
                  "language", 
                  "bedtime.1_1", 
                  "bedtime.2_1", 
                  "bedtime.3_1", 
                  "waketime.1_1", 
                  "waketime.2_1", 
                  "waketime.3_1", 
                  "numremove", 
                  "remove1.1_1", 
                  "remove1.2_1", 
                  "remove1.3_1", 
                  "puton1.1_1", 
                  "puton1.2_1", 
                  "puton1.3_1", 
                  "removereason1", 
                  "remove2.1_1", 
                  "remove2.2_1", 
                  "remove2.3_1", 
                  "puton2.1_1", 
                  "puton2.2_1", 
                  "puton2.3_1", 
                  "removereason2", 
                  "remove3.1_1", 
                  "remove3.2_1", 
                  "remove3.3_1", 
                  "puton3.1_1", 
                  "puton3.2_1", 
                  "puton3.3_1", 
                  "removereason3", 
                  "remove4.1_1", 
                  "remove4.2_1", 
                  "remove4.3_1", 
                  "puton4.1_1", 
                  "puton4.2_1", 
                  "puton4.3_1", 
                  "removereason4", 
                  "nap", 
                  "sick", 
                  "med_yn", 
                  "med_text",
                  "school_hour",
                  "school_min",
                  "work_hour",
                  "work_min",
                  "homework_hour",
                  "homework_min",
                  "extra_hour",
                  "extra_min",
                  "relax_hour",
                  "relax_min",
                  "clean_yn", 
                  "caresib_yn", 
                  "errands_yn", 
                  "helpsib_yn", 
                  "cook_yn", 
                  "parentwork_yn", 
                  "famother_yn", 
                  "nofamob_yn", 
                  "famother_text", 
                  "famassist_hour",
                  "famassist_min",
                  "rude.yn",
                  "rude.race", 
                  "rude.class", 
                  "rude.gender", 
                  "ignore.yn",
                  "ignore.race", 
                  "ignore.class", 
                  "ignore.gender", 
                  "punish.yn",
                  "punish.race", 
                  "punish.class", 
                  "punish.gender", 
                  "afraid.yn",
                  "afraid.race", 
                  "afraid.class", 
                  "afraid.gender", 
                  "mean.yn",
                  "mean.race", 
                  "mean.class", 
                  "mean.gender", 
                  "names.yn",
                  "names.race", 
                  "names.class", 
                  "names.gender", 
                  "overload1", 
                  "overload2", 
                  "overload3", 
                  "overload4", 
                  "complete")
    
  }else if (study == "MHS") {
    
    varnames <- c("startdate", 
                  "enddate", 
                  "status", 
                  "ipaddress", 
                  "progress", 
                  "duration.sec", 
                  "finished", 
                  "recordeddate", 
                  "qualtricsid", 
                  "id", 
                  "firstname",
                  "email", 
                  "extref", 
                  "latitude", 
                  "longitude", 
                  "channel", 
                  "language", 
                  "bedtime.1_1", 
                  "bedtime.2_1", 
                  "bedtime.3_1", 
                  "waketime.1_1", 
                  "waketime.2_1", 
                  "waketime.3_1", 
                  "numremove", 
                  "remove1.1_1", 
                  "remove1.2_1", 
                  "remove1.3_1", 
                  "puton1.1_1", 
                  "puton1.2_1", 
                  "puton1.3_1", 
                  "removereason1", 
                  "remove2.1_1", 
                  "remove2.2_1", 
                  "remove2.3_1", 
                  "puton2.1_1", 
                  "puton2.2_1", 
                  "puton2.3_1", 
                  "removereason2", 
                  "remove3.1_1", 
                  "remove3.2_1", 
                  "remove3.3_1", 
                  "puton3.1_1", 
                  "puton3.2_1", 
                  "puton3.3_1", 
                  "removereason3", 
                  "remove4.1_1", 
                  "remove4.2_1", 
                  "remove4.3_1", 
                  "puton4.1_1", 
                  "puton4.2_1", 
                  "puton4.3_1", 
                  "removereason4", 
                  "nap", 
                  "sick", 
                  "med_yn", 
                  "med_text")
  }
  
  colnames(d1) <- varnames
  colnames(d2) <- varnames
  colnames(d3) <- varnames
  colnames(d4) <- varnames
  colnames(d5) <- varnames
  colnames(d6) <- varnames
  colnames(d7) <- varnames
  
  if (study == "OTR") {
    d8varnames <- c("startdate","enddate","status",
                    "ipaddress","progress","duration.sec",
                    "finished","recordeddate","qualtricsid",
                    "lastname","id","email",
                    "extref","latitude","longitude",
                    "channel","language","bedtime.1_1",
                    "bedtime.2_1","bedtime.3_1","waketime.1_1",
                    "waketime.2_1","waketime.3_1","numremove",
                    "remove1.1_1","remove1.2_1","remove1.3_1",
                    "puton1.1_1","puton1.2_1","puton1.3_1",
                    "removereason1","remove2.1_1","remove2.2_1",
                    "remove2.3_1","puton2.1_1","puton2.2_1",
                    "puton2.3_1","removereason2","remove3.1_1",
                    "remove3.2_1","remove3.3_1","puton3.1_1",
                    "puton3.2_1","puton3.3_1","removereason3",
                    "remove4.1_1","remove4.2_1","remove4.3_1",
                    "puton4.1_1","puton4.2_1","puton4.3_1",
                    "removereason4")
  } else if (study == "MHS") {
    d8varnames <- c("startdate","enddate","status",
                    "ipaddress","progress","duration.sec",
                    "finished","recordeddate","qualtricsid",
                    "id","firstname","email",
                    "extref","latitude","longitude",
                    "channel","language","bedtime.1_1",
                    "bedtime.2_1","bedtime.3_1","waketime.1_1",
                    "waketime.2_1","waketime.3_1","numremove",
                    "remove1.1_1","remove1.2_1","remove1.3_1",
                    "puton1.1_1","puton1.2_1","puton1.3_1",
                    "removereason1","remove2.1_1","remove2.2_1",
                    "remove2.3_1","puton2.1_1","puton2.2_1",
                    "puton2.3_1","removereason2","remove3.1_1",
                    "remove3.2_1","remove3.3_1","puton3.1_1",
                    "puton3.2_1","puton3.3_1","removereason3",
                    "remove4.1_1","remove4.2_1","remove4.3_1",
                    "puton4.1_1","puton4.2_1","puton4.3_1",
                    "removereason4", "sum")
  }
  
  colnames(d8) <- d8varnames
  if (study == "MHS") {
    d8 <- dplyr::select(d8, -sum)
    janitor::tabyl (d1$id, show_missing_levels = F)
    janitor::tabyl (d2$id, show_missing_levels = F)
    janitor::tabyl (d3$id, show_missing_levels = F)
    janitor::tabyl (d4$id, show_missing_levels = F)
    janitor::tabyl (d5$id, show_missing_levels = F)
    janitor::tabyl (d6$id, show_missing_levels = F)
    janitor::tabyl (d7$id, show_missing_levels = F)
    janitor::tabyl (d8$id, show_missing_levels = F) #random names for d8?!
    
    d8 %>% dplyr::mutate (id = as.numeric(as.character(id))) %>%
      dplyr::filter (is.na(id)==F) -> d8
  }
  
  #blank = na
  d1 %>% dplyr::mutate_all(., list(~na_if(.,""))) -> d1
  d2 %>% dplyr::mutate_all(., list(~na_if(.,""))) -> d2
  d3 %>% dplyr::mutate_all(., list(~na_if(.,""))) -> d3
  d4 %>% dplyr::mutate_all(., list(~na_if(.,""))) -> d4
  d5 %>% dplyr::mutate_all(., list(~na_if(.,""))) -> d5
  d6 %>% dplyr::mutate_all(., list(~na_if(.,""))) -> d6
  d7 %>% dplyr::mutate_all(., list(~na_if(.,""))) -> d7
  d8 %>% dplyr::mutate_all(., list(~na_if(.,""))) -> d8
  
  #remove pilot/junk entries and junk/identifying variables
  #can we just get these deleted please
  
  if (study == "MHS") {
    d1 %>% dplyr::filter (is.na (id)==FALSE & is.na(firstname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, firstname, extref, email, latitude, longitude, language))-> d1
    d2 %>% dplyr::filter (is.na (id)==FALSE & is.na(firstname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE  & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, firstname, extref, email, latitude, longitude, language))-> d2
    d3 %>% dplyr::filter (is.na (id)==FALSE & is.na(firstname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, firstname, extref, email, latitude, longitude, language))-> d3
    d4 %>% dplyr::filter (is.na (id)==FALSE & is.na(firstname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, firstname, extref, email, latitude, longitude, language))-> d4
    d5 %>% dplyr::filter (is.na (id)==FALSE & is.na(firstname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, firstname, extref, email, latitude, longitude, language))-> d5
    d6 %>% dplyr::filter (is.na (id)==FALSE & is.na(firstname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, firstname, extref, email, latitude, longitude, language))-> d6
    d7 %>% dplyr::filter (is.na (id)==FALSE & is.na(firstname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, firstname, extref, email, latitude, longitude, language))-> d7
    d8 %>% dplyr::filter (is.na (id)==FALSE & is.na(firstname) == FALSE, channel != "preview" & id != 9999 & id != "EC") %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, firstname, extref, email, latitude, longitude, language))-> d8
  } else if (study == "OTR") {
    d1 %>% dplyr::filter (is.na (id)==FALSE & is.na(lastname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, lastname, extref, email, latitude, longitude, language))-> d1
    d2 %>% dplyr::filter (is.na (id)==FALSE & is.na(lastname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE  & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, lastname, extref, email, latitude, longitude, language))-> d2
    d3 %>% dplyr::filter (is.na (id)==FALSE & is.na(lastname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, lastname, extref, email, latitude, longitude, language))-> d3
    d4 %>% dplyr::filter (is.na (id)==FALSE & is.na(lastname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, lastname, extref, email, latitude, longitude, language))-> d4
    d5 %>% dplyr::filter (is.na (id)==FALSE & is.na(lastname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, lastname, extref, email, latitude, longitude, language))-> d5
    d6 %>% dplyr::filter (is.na (id)==FALSE & is.na(lastname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, lastname, extref, email, latitude, longitude, language))-> d6
    d7 %>% dplyr::filter (is.na (id)==FALSE & is.na(lastname) == FALSE, channel != "preview" & id != 9999) %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, lastname, extref, email, latitude, longitude, language))-> d7
    d8 %>% dplyr::filter (is.na (id)==FALSE & is.na(lastname) == FALSE, channel != "preview" & id != 9999 & id != "EC") %>%
      dplyr::filter (grepl("TEST", .$id)== FALSE & grepl("hoebe", .$id)== FALSE & grepl("Edith", .$id)== FALSE & grepl("Test", .$id)== FALSE) %>%
      dplyr::select (-c(ipaddress, recordeddate, lastname, extref, email, latitude, longitude, language))-> d8
  }
  
  
  #make id the first column
  d1 %>% dplyr::select (id, everything()) -> d1
  d2 %>% dplyr::select (id, everything()) -> d2
  d3 %>% dplyr::select (id, everything()) -> d3
  d4 %>% dplyr::select (id, everything()) -> d4
  d5 %>% dplyr::select (id, everything()) -> d5
  d6 %>% dplyr::select (id, everything()) -> d6
  d7 %>% dplyr::select (id, everything()) -> d7
  d8 %>% dplyr::select (id, everything()) -> d8
  
  #add suffix to each diary day
  d1 %>% dplyr::rename_all(paste0, "_d1") %>% dplyr::rename (id = id_d1)-> d1
  d2 %>% dplyr::rename_all(paste0, "_d2") %>% dplyr::rename (id = id_d2)-> d2
  d3 %>% dplyr::rename_all(paste0, "_d3") %>% dplyr::rename (id = id_d3)-> d3
  d4 %>% dplyr::rename_all(paste0, "_d4") %>% dplyr::rename (id = id_d4)-> d4
  d5 %>% dplyr::rename_all(paste0, "_d5") %>% dplyr::rename (id = id_d5)-> d5
  d6 %>% dplyr::rename_all(paste0, "_d6") %>% dplyr::rename (id = id_d6)-> d6
  d7 %>% dplyr::rename_all(paste0, "_d7") %>% dplyr::rename (id = id_d7)-> d7
  d8 %>% dplyr::rename_all(paste0, "_d8") %>% dplyr::rename (id = id_d8)-> d8
  
  ### otr: for handling 1431 only ###
  #keeping only the march entries
  if (study == "OTR" & visit == 1) {
    d1 %>% dplyr::filter (qualtricsid_d1 != "R_eDvHtKkNqsLe9eZ") -> d1
    d7 %>% dplyr::filter (qualtricsid_d7 != "R_1MZhUWUmixF3WIl") -> d7
    d8 %>% dplyr::filter (qualtricsid_d8 != "R_11ZmpdqJ8kOgjfw" & qualtricsid_d8 != "R_QmRrnCdnJVW3iVz") -> d8
  }
  
  
  # merge all days together
  d1 %>% dplyr::group_by (id) %>% dplyr::mutate(count = row_number()) -> d1
  d2 %>% dplyr::group_by (id) %>% dplyr::mutate(count = row_number()) -> d2
  d3 %>% dplyr::group_by (id) %>% dplyr::mutate(count = row_number()) -> d3
  d4 %>% dplyr::group_by (id) %>% dplyr::mutate(count = row_number()) -> d4
  d5 %>% dplyr::group_by (id) %>% dplyr::mutate(count = row_number()) -> d5
  d6 %>% dplyr::group_by (id) %>% dplyr::mutate(count = row_number()) -> d6
  d7 %>% dplyr::group_by (id) %>% dplyr::mutate(count = row_number()) -> d7
  d8 %>% dplyr::group_by (id) %>% dplyr::mutate(count = row_number()) -> d8
  
  janitor::tabyl(d8$id) %>% View()
  
  nonuniqmerge <- function (...){
    
    func <- function(...){
      df1 = list(...)[[1]]
      df2 = list(...)[[2]]
      xxx=merge(..., by=c("id", "count"), all = TRUE)
      return(xxx)
    }
    
    Reduce(func, list(...)) 
    
  }
  
  merge(d1, d2, by=c("id", "count")) %>% View()
  diary <- nonuniqmerge(d1, d2, d3, d4, d5, d6, d7, d8)
  
  #wide to long
  if (study == "OTR") {
      
      diaryl <- data.table::melt (data.table::setDT(dplyr::select(diary, id, matches( c('startdate',
                                                                                 'enddate',
                                                                                 'status',
                                                                                 'progress',
                                                                                 'duration.sec',
                                                                                 'finished',
                                                                                 'qualtricsid',
                                                                                 'channel',
                                                                                 'bedtime.1_1',
                                                                                 'bedtime.2_1',
                                                                                 'bedtime.3_1',
                                                                                 'waketime.1_1',
                                                                                 'waketime.2_1',
                                                                                 'waketime.3_1',
                                                                                 'numremove',
                                                                                 'remove1.1_1',
                                                                                 'remove1.2_1',
                                                                                 'remove1.3_1',
                                                                                 'puton1.1_1',
                                                                                 'puton1.2_1',
                                                                                 'puton1.3_1',
                                                                                 'removereason1',
                                                                                 'remove2.1_1',
                                                                                 'remove2.2_1',
                                                                                 'remove2.3_1',
                                                                                 'puton2.1_1',
                                                                                 'puton2.2_1',
                                                                                 'puton2.3_1',
                                                                                 'removereason2',
                                                                                 'remove3.1_1',
                                                                                 'remove3.2_1',
                                                                                 'remove3.3_1',
                                                                                 'puton3.1_1',
                                                                                 'puton3.2_1',
                                                                                 'puton3.3_1',
                                                                                 'removereason3',
                                                                                 'remove4.1_1',
                                                                                 'remove4.2_1',
                                                                                 'remove4.3_1',
                                                                                 'puton4.1_1',
                                                                                 'puton4.2_1',
                                                                                 'puton4.3_1',
                                                                                 'removereason4',
                                                                                 'nap',
                                                                                 'sick',
                                                                                 'med_yn',
                                                                                 'med_text')))),
                                  measure = patterns( 'startdate',
                                                      'enddate',
                                                      'status',
                                                      'progress',
                                                      'duration.sec',
                                                      'finished',
                                                      'qualtricsid',
                                                      'channel',
                                                      'bedtime.1_1',
                                                      'bedtime.2_1',
                                                      'bedtime.3_1',
                                                      'waketime.1_1',
                                                      'waketime.2_1',
                                                      'waketime.3_1',
                                                      'numremove',
                                                      'remove1.1_1',
                                                      'remove1.2_1',
                                                      'remove1.3_1',
                                                      'puton1.1_1',
                                                      'puton1.2_1',
                                                      'puton1.3_1',
                                                      'removereason1',
                                                      'remove2.1_1',
                                                      'remove2.2_1',
                                                      'remove2.3_1',
                                                      'puton2.1_1',
                                                      'puton2.2_1',
                                                      'puton2.3_1',
                                                      'removereason2',
                                                      'remove3.1_1',
                                                      'remove3.2_1',
                                                      'remove3.3_1',
                                                      'puton3.1_1',
                                                      'puton3.2_1',
                                                      'puton3.3_1',
                                                      'removereason3',
                                                      'remove4.1_1',
                                                      'remove4.2_1',
                                                      'remove4.3_1',
                                                      'puton4.1_1',
                                                      'puton4.2_1',
                                                      'puton4.3_1',
                                                      'removereason4',
                                                      'nap',
                                                      'sick',
                                                      'med_yn',
                                                      'med_text'), 
                                  value.name = c( 'startdate',
                                                  'enddate',
                                                  'status',
                                                  'progress',
                                                  'duration.sec',
                                                  'finished',
                                                  'qualtricsid',
                                                  'channel',
                                                  'bedtime.1_1',
                                                  'bedtime.2_1',
                                                  'bedtime.3_1',
                                                  'waketime.1_1',
                                                  'waketime.2_1',
                                                  'waketime.3_1',
                                                  'numremove',
                                                  'remove1.1_1',
                                                  'remove1.2_1',
                                                  'remove1.3_1',
                                                  'puton1.1_1',
                                                  'puton1.2_1',
                                                  'puton1.3_1',
                                                  'removereason1',
                                                  'remove2.1_1',
                                                  'remove2.2_1',
                                                  'remove2.3_1',
                                                  'puton2.1_1',
                                                  'puton2.2_1',
                                                  'puton2.3_1',
                                                  'removereason2',
                                                  'remove3.1_1',
                                                  'remove3.2_1',
                                                  'remove3.3_1',
                                                  'puton3.1_1',
                                                  'puton3.2_1',
                                                  'puton3.3_1',
                                                  'removereason3',
                                                  'remove4.1_1',
                                                  'remove4.2_1',
                                                  'remove4.3_1',
                                                  'puton4.1_1',
                                                  'puton4.2_1',
                                                  'puton4.3_1',
                                                  'removereason4',
                                                  'nap',
                                                  'sick',
                                                  'med_yn',
                                                  'med_text'))

  }  else if (study == "MHS") {
    
    diaryl <- data.table::melt (data.table::setDT(diary),
                                measure = patterns( 'startdate',
                                                    'enddate',
                                                    'status',
                                                    'progress',
                                                    'duration.sec',
                                                    'finished',
                                                    'qualtricsid',
                                                    'channel',
                                                    'bedtime.1_1',
                                                    'bedtime.2_1',
                                                    'bedtime.3_1',
                                                    'waketime.1_1',
                                                    'waketime.2_1',
                                                    'waketime.3_1',
                                                    'numremove',
                                                    'remove1.1_1',
                                                    'remove1.2_1',
                                                    'remove1.3_1',
                                                    'puton1.1_1',
                                                    'puton1.2_1',
                                                    'puton1.3_1',
                                                    'removereason1',
                                                    'remove2.1_1',
                                                    'remove2.2_1',
                                                    'remove2.3_1',
                                                    'puton2.1_1',
                                                    'puton2.2_1',
                                                    'puton2.3_1',
                                                    'removereason2',
                                                    'remove3.1_1',
                                                    'remove3.2_1',
                                                    'remove3.3_1',
                                                    'puton3.1_1',
                                                    'puton3.2_1',
                                                    'puton3.3_1',
                                                    'removereason3',
                                                    'remove4.1_1',
                                                    'remove4.2_1',
                                                    'remove4.3_1',
                                                    'puton4.1_1',
                                                    'puton4.2_1',
                                                    'puton4.3_1',
                                                    'removereason4',
                                                    'nap',
                                                    'sick',
                                                    'med_yn',
                                                    'med_text'), 
                                value.name = c( 'startdate',
                                                'enddate',
                                                'status',
                                                'progress',
                                                'duration.sec',
                                                'finished',
                                                'qualtricsid',
                                                'channel',
                                                'bedtime.1_1',
                                                'bedtime.2_1',
                                                'bedtime.3_1',
                                                'waketime.1_1',
                                                'waketime.2_1',
                                                'waketime.3_1',
                                                'numremove',
                                                'remove1.1_1',
                                                'remove1.2_1',
                                                'remove1.3_1',
                                                'puton1.1_1',
                                                'puton1.2_1',
                                                'puton1.3_1',
                                                'removereason1',
                                                'remove2.1_1',
                                                'remove2.2_1',
                                                'remove2.3_1',
                                                'puton2.1_1',
                                                'puton2.2_1',
                                                'puton2.3_1',
                                                'removereason2',
                                                'remove3.1_1',
                                                'remove3.2_1',
                                                'remove3.3_1',
                                                'puton3.1_1',
                                                'puton3.2_1',
                                                'puton3.3_1',
                                                'removereason3',
                                                'remove4.1_1',
                                                'remove4.2_1',
                                                'remove4.3_1',
                                                'puton4.1_1',
                                                'puton4.2_1',
                                                'puton4.3_1',
                                                'removereason4',
                                                'nap',
                                                'sick',
                                                'med_yn',
                                                'med_text'))
    
  }
  
  diaryl %>%
    arrange (id) %>%
    dplyr::rename (qualtrics_day = variable) -> diaryl
  
  # exclude duplicates from descriptive purposes for edith
  diaryl %>% dplyr::filter (!id %in% exclude) -> diaryl
  
  # binge alert adding
  #put together the bed time and the wake time
  diaryl %>%
    dplyr::rename (bedtime.hr = bedtime.1_1,
            bedtime.min = bedtime.2_1,
            bedtime.ampm = bedtime.3_1,
            waketime.hr = waketime.1_1,
            waketime.min = waketime.2_1,
            waketime.ampm = waketime.3_1) -> diaryl
  
  diaryl %>%
    dplyr::mutate (bedtime.hr = case_when (as.numeric(bedtime.hr)<10~ paste("0", bedtime.hr, sep=""),
                                    TRUE~ bedtime.hr),
            bedtime.min = case_when (as.numeric(bedtime.min)<10~ paste("0", bedtime.min, sep=""),
                                     TRUE~ bedtime.min),
            bedtime.ampm = case_when (bedtime.ampm=="1"~ "am",
                                      bedtime.ampm=="2"~ "pm")) -> diaryl
  
  diaryl %>%
    dplyr::mutate (waketime.hr = case_when (as.numeric(waketime.hr)<10~ paste("0", waketime.hr, sep=""),
                                     TRUE~ waketime.hr),
            waketime.min = case_when (as.numeric(waketime.min)<10~ paste("0", waketime.min, sep=""),
                                      TRUE~ waketime.min),
            waketime.ampm = case_when (waketime.ampm=="1"~ "am",
                                       waketime.ampm=="2"~ "pm")) -> diaryl
  
  diaryl %>%
    dplyr::mutate (bedtime_raw = case_when (is.na(bedtime.hr)==FALSE~paste(bedtime.hr, ":", bedtime.min, bedtime.ampm, sep=""),
                                     is.na(bedtime.hr)==TRUE~ NA_character_)) %>%
    dplyr::mutate (waketime_raw = case_when (is.na(waketime.hr)==FALSE~paste(waketime.hr, ":", waketime.min, waketime.ampm, sep=""),
                                      is.na(waketime.hr)==TRUE~ NA_character_)) -> diaryl
  
  
  #add sleep ref date based on enddate
  # generate the dates for the sleep date participant is reporting about and the date participant reported sleep
  # for "inbed.date"= date of sleep participant is reporting about
  # if they did it before midnight, then the reported sleep date = qualtrics timestamp date - 1 (because it reference last night)
  # if they did it after midnight, then the reported sleep date = qualtrics timestamp date - 2 (because it reference last night and they did it past midnight)
  # for "inbed.reportdate" = date participant reported sleep (adjusted)
  # if they did it before midnight, then the reported sleep date = qualtrics timestamp date (no adjustment)
  # if they did it after midnight, then the reported sleep date = qualtrics timestamp date - 1 (because they did it past midnight)
  # using hour < 21 to defined past midnight, because each diary sent at 9pm, so no way they can do it before then for any given day
  
  diaryl %>%
    tidyr::separate (enddate, c("qualtrics_date", "qualtrics_time"), " ", fill = "right", remove= FALSE) %>%
    dplyr::mutate (qualtrics_time = as.character(qualtrics_time)) %>%
    tidyr::separate (qualtrics_time, c("hour", "min", "sec"), ":", fill ="right", remove = FALSE) %>%
    dplyr::mutate (hour = as.numeric (as.character(hour))) -> diaryl
  
  diaryl %>%
    dplyr::mutate (qualtrics_day = as.numeric(as.character(qualtrics_day))) %>%
    dplyr::mutate (inbed.reportdate = case_when (qualtrics_day!=8 & hour < 21 ~ as.Date(qualtrics_date)-1,
                                          qualtrics_day!=8 & hour >= 21~ as.Date(qualtrics_date),
                                          qualtrics_day==8~ as.Date(qualtrics_date))) %>%
    dplyr::mutate (inbed.date = case_when (qualtrics_day!=8 & hour < 21 ~ as.Date(qualtrics_date)-2,
                                    qualtrics_day!=8 & hour >= 21 ~ as.Date(qualtrics_date) - 1,
                                    qualtrics_day==8~ as.Date(qualtrics_date) - 1)) %>%
    dplyr::mutate (inbed.report.weekday = weekdays(as.Date(inbed.reportdate)),
            inbed.weekday = weekdays(as.Date(inbed.date))) -> diaryl
  
  #add binge alert here
  #binge defined by two entries indexing the same sleep date
  diaryl %>%
    arrange (id, qualtrics_day) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate (inbed.rowdiff = inbed.date - lag(inbed.date),
            inbed.rowdiff2 = inbed.date - lead(inbed.date)) %>%
    dplyr::mutate (binge = case_when (inbed.rowdiff == 0 | inbed.rowdiff2 == 0 ~ "binge",
                               is.na(inbed.rowdiff)==TRUE & is.na(inbed.rowdiff2)==TRUE ~ NA_character_, 
                               TRUE ~ "ok")) -> diaryl
  
  
  
  
  # output organizing
  # check all days have same # of entries
  check1 <- janitor::tabyl (diaryl$qualtrics_day)
  
  # check that everyone has 8 rows (outputting those who don't)
  check2 <- janitor::tabyl (diaryl$id, show_missing_levels = F) %>% as.data.frame(.) %>% dplyr::filter (n!=8)
  
  # total number of participants
  totp <- janitor::tabyl (diaryl$qualtrics_day, show_missing_levels = F)
  
  # total number of entries of every participants
  toten<- diaryl %>%
    dplyr::filter (is.na(startdate)==F) %>% nrow() 
  
  # total number of participants have at least one entry binged
  diaryl %>%
    dplyr::group_by(id) %>%
    dplyr::mutate (binge_num = case_when(binge == "ok"~ 0,
                                  binge == "binge"~ 1,
                                  TRUE~ NA_real_)) %>%
    dplyr::mutate (binge_tot = sum(binge_num, na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate (binge_yn = case_when(binge_tot == 0~0,
                                 binge_tot >0~ 1)) -> diaryl
  
  # diaryl %>% dplyr::select (id, inbed.date, inbed.rowdiff, inbed.rowdiff2, qualtrics_day, binge, binge_tot, binge_yn) %>% View ()

  diaryl %>% dplyr::filter (qualtrics_day==1) -> temp
  bingeyn<- janitor::tabyl (temp$binge_yn)
  
  # binge broke down by number of entries
  bingebreak<- janitor::tabyl (temp$binge_tot)
  
  # total number of entries tagged as binge
  bingetot <- janitor::tabyl (diaryl$binge)

  
  result <- list("check all days have same # of entries" = check1,
                 "check that everyone has 8 rows (outputting those who don't)" = check2,
                 "total number of participants (excluding user inputted ids)"= totp, 
                 "total number of entries of every participants (excluding user inputted ids" = toten, 
                 "total number of participants have at least one entry binged" = bingeyn, 
                 "total number of entries tagged as binge" = bingetot,
                 "binge broke down by number of entries" = bingebreak)

  return (result)
  
  
}
