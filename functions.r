################################################################################
# Project Name: RSimmerAdaption
# Date Last Updated: 17/01/20
# Summary: List of functions
################################################################################

## DESCIPTION
fnDataManipulate <- function(data){
  ## Remove Duplicates. Mostly seems to be babies with weights being added later.  All are single episodes only
  data$`Episode Number`<-as.numeric(data$`Episode Number`)
  data<-dplyr::distinct(data,`Hospital Provider Spell Number`,`Episode Number`, .keep_all=TRUE)
  data$`_TLSpellDigest`=data$`Hospital Provider Spell Number`
  print("* De-duplicated and digested *")
  
  ## Bring Data and time together as a StrpTime object for Spell Start and End
  data$`_SpellStart_DateTime`<-as.POSIXct(strptime(paste(data$`Start Date (Hospital Provider Spell)`,
                                                         data$`Start Time (Hospital Provider Spell)`,sep=" "),format="%d/%m/%Y %H:%M:%S"))
  data$`_Discharge_DateTime`<-as.POSIXct(strptime(paste(data$`Discharge Date (From Hospital Provider Spell)`,
                                                        data$`Discharge Time (From Hospital Provider Spell)`,sep=" "),format="%d/%m/%Y %H:%M:%S"))
  ## TODO generates some missing data on discharge - several which have impossible discharge times (ie during the "missing hour" on DST change), 
  ## others have a time but no date.  Presently not dealt with! 
  
  print("* Raw data manipulated *")
  return(data)
}

## DESCIPTION
fnBaseData <- function(data){
  ## Find first episodes and create sequential SpellID
  firstepisodes<-filter(data,`Episode Number`=="1")
  ## Give each spell a unique ID - hopefully not required once we have "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER"
  firstepisodes$`_SpellID` <- seq.int(nrow(firstepisodes))
  
  basedata<-dplyr::select(firstepisodes,`_SpellID`,`_TLSpellDigest`,pkid.x,Sex,
                          `Start Date (Hospital Provider Spell)`,`Discharge Date (From Hospital Provider Spell)`,
                          `Start Time (Hospital Provider Spell)`,`Discharge Time (From Hospital Provider Spell)`,
                          `Admission Method (Hospital Provider Spell)`,`Year of Birth`,
                          `Discharge Method (Hospital Provider Spell)`,`Discharge Destination (Hospital Provider Spell)`,
                          `Patient Classification`,`_SpellStart_DateTime`,`_Discharge_DateTime`,`Main Specialty Code`,
                          `Treatment Function Code`,`Spell Core HRG`,`Hospital Provider Spell Discharge Ready Date`)
  
  ## elective if first character is a 1
  basedata$`_Elective`<-(substr(basedata$`Admission Method (Hospital Provider Spell)`,1,1)=="1")
  
  ## Mark patients with diagnostic-only admissions.
  ##  dvw<-filter(firstepisodes,`Last Episode In Spell Indicator`==1) %>% 
  ##    filter_at(vars(ends_with("Procedure (OPCS)")), 
  ##              all_vars(str_detect(.,"(^$|^G(16|45|55|65|80)|^H(22|25)|^Y(413|904|905|97|98)|^Z|^O|^U(?!(191|197|331|5)))"))) %>% 
  ##    filter(`Critical Care Start Date 1`=="")
  ##  dvw_bl<-duplicated(c(dvw$`_TLSpellDigest`,basedata$`_TLSpellDigest`))[-seq_len(length(dvw$`_TLSpellDigest`))]
  ##  basedata$`_dvw_bl`<-dvw_bl
  
  ##DVW codes for Reference
  ##O/Z - miscellaneous locations (secondary codes)
  ##Y97/98 - radiology with/without contrast
  ##Y904/905 - Ba meal/enema
  ##Y413 - endoscopic USS of organ
  ##G16/G45/G55/G65/G80 - oesophagoscopy, gastroscopy, duod/jej/ileoscopy
  ##H22/H25 - colonoscopy, sigmoidoscopy, 
  ##U - all diagnostic except:
  ##U191/197 - implant/remove loop recorder
  ##U331 - polysomnography
  ##U5 - rehab
  
  print("* Base table created *")
  return(basedata)
}

## DESCIPTION
fnRepeatedData <- function(data){  
  repeateddata <- dplyr::select(data,"pkid.x","_TLSpellDigest","Episode Number","Last Episode In Spell Indicator",
                                "Main Specialty Code","Treatment Function Code","Primary Procedure (OPCS)",
                                "Diagnosis Primary (ICD)","Primary Procedure Date (OPCS)","_SpellStart_DateTime",
                                "_Discharge_DateTime","Start Date (Consultant Episode)","Start Time (Episode)",
                                "End Date (Consultant Episode)","End Time (Episode)")
  
  ## Create varaibles for Epi timings 
  repeateddata$`_EpisodeStart_DateTime` <- as.POSIXct(strptime(paste0(repeateddata$`Start Date (Consultant Episode)`," ",
                                                                      repeateddata$`Start Time (Episode)`),format="%Y%m%d %H:%M:%S"))
  repeateddata$`_EpisodeEnd_DateTime` <- as.POSIXct(strptime(paste0(repeateddata$`End Date (Consultant Episode)`," ",
                                                                    repeateddata$`End Time (Episode)`),format="%Y%m%d %H:%M:%S"))
  repeateddata$`_EpisodeStart_Offset` <- difftime(repeateddata$`_EpisodeStart_DateTime`,repeateddata$`_SpellStart_DateTime`,units="secs")
  repeateddata$`_EpisodeEnd_Offset` <- difftime(repeateddata$`_EpisodeEnd_DateTime`,repeateddata$`_SpellStart_DateTime`,units="secs")
  print("* Repeated table created *")
  return(repeateddata)
}

## DESCIPTION
fnCCData <- function(data){   
  critcaredata <- filter(data,`Critical Care Start Date 1`!="") %>% 
    dplyr::select("pkid.x","_TLSpellDigest","Episode Number","Last Episode In Spell Indicator",
                  "Start Date (Hospital Provider Spell)","Start Time (Hospital Provider Spell)",
                  "Start Date (Consultant Episode)","Start Time (Episode)","End Date (Consultant Episode)",
                  "End Time (Episode)","Main Specialty Code","Treatment Function Code","Primary Procedure (OPCS)",
                  "Diagnosis Primary (ICD)","Primary Procedure Date (OPCS)","_SpellStart_DateTime",
                  "_Discharge_DateTime",starts_with("Critical Care") ) %>% 
    gather(var,value,matches("\\d$")) %>%  ## all the ones that end in a digit
    tidyr::separate(var,c("var","ccstay"),sep="[ ](?=[^ ]+$)") %>% ## regex to match last space of a string
    arrange(pkid.x,`Episode Number`,`_SpellStart_DateTime`) %>% 
    spread(var,value) %>% 
    filter(`Critical Care Start Date`!="")
  
  ## Create CC Timings
  critcaredata$`_SegmentStart_DateTime` <- as.POSIXct(strptime(paste0(critcaredata$`Critical Care Start Date`," ",
                                                                      critcaredata$`Critical Care Start Time`),format="%Y%m%d %H:%M:%S"))
  critcaredata$`_SegmentEnd_DateTime` <- as.POSIXct(strptime(paste0(critcaredata$`Critical Care Discharge Date`," ",
                                                                    critcaredata$`Critical Care Discharge Time`),format="%Y%m%d %H:%M:%S"))
  critcaredata$`_EpisodeStart_DateTime` <- as.POSIXct(strptime(paste0(critcaredata$`Start Date (Consultant Episode)`," ",
                                                                      critcaredata$`Start Time (Episode)`),format="%Y%m%d %H:%M:%S"))
  critcaredata$`_EpisodeEnd_DateTime` <- as.POSIXct(strptime(paste0(critcaredata$`End Date (Consultant Episode)`," ",
                                                                    critcaredata$`End Time (Episode)`),format="%Y%m%d %H:%M:%S"))
  critcaredata$`_SegmentStart_DateTime` <- pmax(critcaredata$`_SegmentStart_DateTime`,critcaredata$`_EpisodeStart_DateTime`,na.rm=TRUE)
  critcaredata$`_SegmentEnd_DateTime` <- pmin(critcaredata$`_SegmentEnd_DateTime`,critcaredata$`_EpisodeEnd_DateTime`,na.rm=TRUE)
  ## need pmax,pmin or we get the min/max of the entire dataset
  ## na.rm helps us fill in gaps by using the episode end datetime if there isn't a discharge time (eg if episode changes during a CC stay)
  
  ##set ward 21 flag - times are 00:00-23:59, or local identifier not 8 digits starting with 4 digits of year (ie not 'proper' critical care unit)
  critcaredata$`_RealCritCare` <- nchar(critcaredata$`Critical Care Local Identifier`)>7 ##20190307 - 2748 real crit care admissions out of 4615
  
  critcaredata$ccstay<-as.numeric(critcaredata$ccstay)
  print("* Crit care table created *")
  
  ## transfer = we move directly between critical care beds rather than having a ward bed in the meantime 
  ## (used in model to determine movements as they may not occur at the real times)
  critcaredata$`_CCTransfer`<-0
  critcaredata$`_ccseg`<-0
  
  ## iterate over critical care stay, if overlap between start and discharge times of adjacent segments, 
  ## set both to be the one that's not 23:59 or 00:00 (ie ward 21)
  
  ## Iterate over spells:
  ccspells <- unique(critcaredata$`_TLSpellDigest`)
  for (ccspell in ccspells) {
    ccstays<-filter(critcaredata,`_TLSpellDigest`==ccspell) %>% arrange(`Episode Number`,ccstay)
    for (i_cc in 1:nrow(ccstays)) {
      curseg_ccstay<-ccstays[i_cc,]$ccstay    ## nb multiple ccstays possible per episode
      curseg_episode<-ccstays[i_cc,]$`Episode Number`
      critcaredata$`_ccseg`[(critcaredata$`_TLSpellDigest`==ccspell)&
                              (critcaredata$ccstay==curseg_ccstay)&
                              (critcaredata$`Episode Number`==curseg_episode)]<-i_cc
      if (i_cc>1) {
        if (prevseg_end>=ccstays[i_cc,]$`_SegmentStart_DateTime`) {
          critcaredata$`_CCTransfer`[(critcaredata$`_TLSpellDigest`==ccspell)&
                                       (critcaredata$ccstay==prevseg_ccstay)&
                                       (critcaredata$`Episode Number`==prevseg_episode)]<-1
          if (prevseg_real) {
            critcaredata$`_SegmentStart_DateTime`[(critcaredata$`_TLSpellDigest`==ccspell)&
                                                    (critcaredata$ccstay==curseg_ccstay)&
                                                    (critcaredata$`Episode Number`==curseg_episode)]<-prevseg_end
          } else {
            critcaredata$`_SegmentEnd_DateTime`[(critcaredata$`_TLSpellDigest`==ccspell)&
                                                  (critcaredata$ccstay==prevseg_ccstay)&
                                                  (critcaredata$`Episode Number`==prevseg_episode)]<-ccstays[i_cc,]$`_SegmentStart_DateTime`
            if (prevseg_start>ccstays[i_cc,]$`_SegmentStart_DateTime`){
              critcaredata$`_SegmentStart_DateTime`[(critcaredata$`_TLSpellDigest`==ccspell)&
                                                      (critcaredata$ccstay==prevseg_ccstay)&
                                                      (critcaredata$`Episode Number`==prevseg_episode)]<-ccstays[i_cc,]$`_SegmentStart_DateTime`
            }
          }
        }
      }
      prevseg_end<-ccstays[i_cc,]$`_SegmentEnd_DateTime`
      prevseg_start<-ccstays[i_cc,]$`_SegmentStart_DateTime`
      prevseg_real<-ccstays[i_cc,]$`_RealCritCare`
      prevseg_ccstay<-ccstays[i_cc,]$ccstay
      prevseg_episode<-ccstays[i_cc,]$`Episode Number`
    }
  }
  
  ##make sure the loop didn't leave the last segment in the wrong order 
  critcaredata$`_SegmentEnd_DateTime`<-pmax(critcaredata$`_SegmentStart_DateTime`,critcaredata$`_SegmentEnd_DateTime`)
  print("* Crit care loop finished *")
  
  ## work out when they're fit for discharge - use discharge ready time, or 
  critcaredata$`_SegmentDischReady_DateTime`<-as.POSIXct(strptime(paste0(critcaredata$`Critical Care Discharge Ready Date`," ",
                                                                         critcaredata$`Critical Care Discharge Ready Time`),format="%Y%m%d %H:%M:%S"))
  ##alternative if not available - earlier of 8am on day of departure, or when they actually left
  critcaredata$`_SegmentDischReady_DateTime2`<-pmin(as.POSIXct(strptime(paste0(as.Date(as.character(critcaredata$`_SegmentEnd_DateTime`))," ",
                                                                               "08:00"),format="%Y-%m-%d %H:%M")),critcaredata$`_SegmentEnd_DateTime`)
  ##nb as.character above because of the issue with dates shortly after midnight being put back to previous day
  #critcaredata$`_SegmentDischReady_DateTime2`<-pmax(critcaredata$`_SegmentStart_DateTime`,critcaredata$`_SegmentDischReady_DateTime2`)
  ##estimate discharge times, but they can't be before arrival or after leaving!
  critcaredata$`_SegmentDischReady_DateTime`<- as.POSIXct(ifelse(is.na(critcaredata$`_SegmentDischReady_DateTime`),
                                                                 critcaredata$`_SegmentDischReady_DateTime2`,
                                                                 critcaredata$`_SegmentDischReady_DateTime`),origin="1970-01-01 00:00:00")
  
  ##can't be before we've arrived or after we've left! (just in case of data quality issues)
  critcaredata$`_SegmentDischReady_DateTime`<-pmax(critcaredata$`_SegmentDischReady_DateTime`,critcaredata$`_SegmentStart_DateTime`)
  critcaredata$`_SegmentDischReady_DateTime`<-pmin(critcaredata$`_SegmentDischReady_DateTime`,critcaredata$`_SegmentEnd_DateTime`)
  critcaredata$`_SegmentDischReady_Offset`<-difftime(critcaredata$`_SegmentDischReady_DateTime`,critcaredata$`_SpellStart_DateTime`,units="secs")
  critcaredata$`_SegmentEnd_Offset`<-difftime(critcaredata$`_SegmentEnd_DateTime`,critcaredata$`_SpellStart_DateTime`,units="secs")
  critcaredata$`_SegmentStart_Offset`<-difftime(critcaredata$`_SegmentStart_DateTime`,critcaredata$`_SpellStart_DateTime`,units="secs")
  print("* Crit care offsets calculated *")
  return(critcaredata)
}

## DESCIPTION
fnSpellsFilter <- function(spells){
  spells<-mutate(spells,`_bizday`=is.bizday(as.character(`_SpellStart_DateTime`),'Rmetrics/LONDON'),
                 `_Start_Time`=(as.numeric(`_SpellStart_DateTime`) %% 86400))
  spells<-mutate(spells,`_2KMD_Date`=as.Date(format(`_SpellStart_DateTime`,"2000-%m-%d")))
  return(spells)
}

## DESCIPTION
fnListElective <- function(spells,resolutionTime,searchDateWindow){
  ## bizarrely need to have as.character in there, or BST dates shortly after midnight get put back to the previous day
  dates <- seq.POSIXt(startDate,endDate,resolutionTime)
  bizDay <- is.bizday(dates,'Rmetrics/LONDON') ##nb behaves oddly just after midnight during BST
  
  output <- data.table(`dateTime`=dates,`bizDay`=bizDay)
  output <-mutate(output,`2KMD_Date`=as.Date(format(`dateTime`,"2000-%m-%d")))
  output <- mutate(output,`startDTSearch`=`2KMD_Date`-searchDateWindow,
                   `endDTSearch`=`2KMD_Date`+searchDateWindow)
  output <- mutate(output,`startTime`=(as.numeric(dateTime) %% 86400))
  output <- data.table(output)
  print("* Started output table *")
  
  spells <- data.table(spells)
  mainout<-spells[output,on=.(`_bizday`=`bizDay`,`_2KMD_Date`>=`startDTSearch`,`_2KMD_Date`<=`endDTSearch`),
                  j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
  
  ## add combinations of matching on _Start_Time+24hrs, -24hrs and date+1yr,-1yr  - ie modulo arithmetic
  mainout$N<-0
  for (doffset in c(as.difftime(-365,units="days"),0,as.difftime(365,units="days"))) {
    tempout <- mutate(output,startTime=startTime,`2KMD_Date`=`2KMD_Date`+doffset) %>% 
      mutate(`startDTSearch`=`2KMD_Date`-searchDateWindow,`endDTSearch`=`2KMD_Date`+searchDateWindow)
    tmpcount<-spells[tempout,on=.(`_bizday`=`bizDay`,`_2KMD_Date`>=`startDTSearch`,`_2KMD_Date`<=`endDTSearch`),
                     j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
    mainout$N<-mainout$N+tmpcount$N
  }
  
  ## counts need correcting by how often these things appear in the initial data - eg weekdays are more common, and time across years
  ## some dates are represented different numbers of times because the data isn't perfect years (see spellEarliest,spellLatest)
  ##then dividing by size of window
  spellEarliest<-min(spells$`_SpellStart_DateTime`)
  spellLatest<-max(spells$`_SpellStart_DateTime`)
  spellAllDays<-data.frame(dat=as.Date(spells$`_SpellStart_DateTime`))
  spellEachDay<-as.Date(names(which(table(spellAllDays$dat)>0)))
  spellEachDay_2KMD<-as.Date(format(spellEachDay,"2000-%m-%d"))
  spellDayCount<-data.frame(spellEachDay,spellEachDay_2KMD)
  spellDayCount<-mutate(spellDayCount,`_bizday`=is.bizday(`spellEachDay`,'Rmetrics/LONDON'))
  spellDayCount<-data.table(spellDayCount)
  
  denominatorout<-spellDayCount[output,on=.(`_bizday`=`bizDay`,`spellEachDay_2KMD`>=`startDTSearch`,`spellEachDay_2KMD`<=`endDTSearch`),
                                j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
  ## add combinations of matching on date+1yr,-1yr  - ie modulo arithmetic
  denominatorout$N<-0
  for (doffset in c(as.difftime(-365,units="days"),0,as.difftime(365,units="days"))) {
    tempout <- mutate(output,`2KMD_Date`=`2KMD_Date`+doffset) %>% 
      mutate(`startDTSearch`=`2KMD_Date`-searchDateWindow,`endDTSearch`=`2KMD_Date`+searchDateWindow)
    tmpcount<-spellDayCount[tempout,on=.(`_bizday`=`bizDay`,`spellEachDay_2KMD`>=`startDTSearch`,`spellEachDay_2KMD`<=`endDTSearch`),
                            j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
    
    denominatorout$N<-denominatorout$N+tmpcount$N
  }
  mainout$`_denominatorDays`<-denominatorout$N
  mainout<-as_tibble(mainout,.name_repair="unique") ##temp fix for the duplicate names caused by data.table's join
  mainout<-mutate(mainout,`_correctedDaysN`=N/`_denominatorDays`)
  
  ## now correct to a daily rate (presently depends on length of search time window) - not relevant for electives as we only index by day
  mainout<-mutate(mainout,`_correctedN`=`_correctedDaysN`)
  print("* Corrected for data availability *")
  
  print(plot(mainout$dateTime,mainout$`_correctedN`,type="l"))
  show_winter<-data.table(mainout)
  show_winter<-show_winter[dateTime<as.POSIXct("2020-06-01"), dateTime := dateTime+365*24*3600]
  print(plot(show_winter$dateTime,show_winter$`_correctedN`,type="l"))
  return(mainout)
}

## DESCIPTION
fnListEmergency <- function(spells,resolutionTime,searchDateWindow,searchTimeWindow){
  ## bizarrely need to have as.character in there, or BST dates shortly after midnight get put back to the previous day
  dates <- seq.POSIXt(startDate,endDate,resolutionTime)
  bizDay <- is.bizday(dates,'Rmetrics/LONDON') ##nb behaves oddly just after midnight during BST
  
  output <- data.table(`dateTime`=dates,`bizDay`=bizDay)
  output <-mutate(output,`2KMD_Date`=as.Date(format(`dateTime`,"2000-%m-%d")))
  output <- mutate(output,`startDTSearch`=`2KMD_Date`-searchDateWindow,
                   `endDTSearch`=`2KMD_Date`+searchDateWindow)
  output <- mutate(output,`startTime`=(as.numeric(dateTime) %% 86400))
  output <- mutate(output,`startTSearch`=`startTime`-as.numeric(searchTimeWindow,units="secs"),
                   `endTSearch`=`startTime`+as.numeric(searchTimeWindow,units="secs"))
  output <- data.table(output)
  print("* Started output table *")
  
  spells <- data.table(spells)
  mainout<-spells[output,on=.(`_bizday`=`bizDay`,`_2KMD_Date`>=`startDTSearch`,`_2KMD_Date`<=`endDTSearch`),
                  j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
  
  ## add combinations of matching on _Start_Time+24hrs, -24hrs and date+1yr,-1yr  - ie modulo arithmetic
  mainout$N<-0
  for (toffset in c(-86400,0,86400)) {
    for (doffset in c(as.difftime(-365,units="days"),0,as.difftime(365,units="days"))) {
      tempout <- mutate(output,startTime=startTime+toffset,`2KMD_Date`=`2KMD_Date`+doffset) %>% 
        mutate(`startDTSearch`=`2KMD_Date`-searchDateWindow,`endDTSearch`=`2KMD_Date`+searchDateWindow) %>% 
        mutate(`startTSearch`=`startTime`-as.numeric(searchTimeWindow,units="secs"),
               `endTSearch`=`startTime`+as.numeric(searchTimeWindow,units="secs")) 
      tmpcount<-spells[tempout,on=.(`_bizday`=`bizDay`,`_Start_Time`>=`startTSearch`,`_Start_Time`<=`endTSearch`,
                                    `_2KMD_Date`>=`startDTSearch`,`_2KMD_Date`<=`endDTSearch`),j=.(.N,dateTime),
                       by=.EACHI,allow.cartesian=TRUE]
      mainout$N<-mainout$N+tmpcount$N
    }
  }
  
  ## counts need correcting by how often these things appear in the initial data - eg weekdays are more common, and time across years
  ## some dates are represented different numbers of times because the data isn't perfect years (see spellEarliest,spellLatest)
  ##then dividing by size of window
  spellEarliest<-min(spells$`_SpellStart_DateTime`)
  spellLatest<-max(spells$`_SpellStart_DateTime`)
  spellAllDays<-data.frame(dat=as.Date(spells$`_SpellStart_DateTime`))
  spellEachDay<-as.Date(names(which(table(spellAllDays$dat)>30)))
  spellEachDay_2KMD<-as.Date(format(spellEachDay,"2000-%m-%d"))
  spellDayCount<-data.frame(spellEachDay,spellEachDay_2KMD)
  spellDayCount<-mutate(spellDayCount,`_bizday`=is.bizday(`spellEachDay`,'Rmetrics/LONDON'))
  spellDayCount<-data.table(spellDayCount)
  
  denominatorout<-spellDayCount[output,on=.(`_bizday`=`bizDay`,`spellEachDay_2KMD`>=`startDTSearch`,`spellEachDay_2KMD`<=`endDTSearch`),j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
  ## add combinations of matching on date+1yr,-1yr  - ie modulo arithmetic
  denominatorout$N<-0
  for (doffset in c(as.difftime(-365,units="days"),0,as.difftime(365,units="days"))) {
    tempout <- mutate(output,`2KMD_Date`=`2KMD_Date`+doffset) %>% 
      mutate(`startDTSearch`=`2KMD_Date`-searchDateWindow,`endDTSearch`=`2KMD_Date`+searchDateWindow)
    tmpcount<-spellDayCount[tempout,on=.(`_bizday`=`bizDay`,`spellEachDay_2KMD`>=`startDTSearch`,`spellEachDay_2KMD`<=`endDTSearch`),j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
    
    denominatorout$N<-denominatorout$N+tmpcount$N
  }
  mainout$`_denominatorDays`<-denominatorout$N
  mainout<-as_tibble(mainout,.name_repair="unique") ##temp fix for the duplicate names caused by data.table's join
  mainout<-mutate(mainout,`_correctedDaysN`=N/`_denominatorDays`)
  
  ## now correct to a daily rate (presently depends on length of search time window) - not relevant for electives as we only index by day
  mainout<-mutate(mainout,`_correctedN`=`_correctedDaysN`)
  print("* Corrected for data availability *")
  
  print(plot(mainout$dateTime,mainout$`_correctedN`,type="l"))
  show_winter<-data.table(mainout)
  show_winter<-show_winter[dateTime<as.POSIXct("2020-06-01"), dateTime := dateTime+365*24*3600]
  print(plot(show_winter$dateTime,show_winter$`_correctedN`,type="l"))
  return(mainout)
}

## DESCIPTION
fnCombined <- function(spells,episodes){
  
  spells<-mutate(spells,`_2KMD_Date`=as.Date(format(`_SpellStart_DateTime`,"2000-%m-%d")))
  ## Join spells and episode tables on the digested spell taken from the hospital Provider Spell Number
  combined<-left_join(spells,episodes,by="_TLSpellDigest")
  combined<-dplyr::rename(combined,`Treatment Function Code`=`Treatment Function Code.y`)
  print("* Combined tables *")
  
  combined$age<-as.numeric(combined$`Start Date (Hospital Provider Spell)`)/10000-as.numeric(combined$`Year of Birth`)-0.5
  combined$traj<-0
  
  ## single-sex wards will be dealt with in the simmer code itself rather than trying to do it here (and creating multiple trajectories)
  ## nb don't use double-character or/and here - only evaluates the first row (and applies to all!) - weird effects
  
  ## lots of code 180 = A&E. Could be anything! 190=anaesthesia probably represents ICU admissions
  ##Emergency 0 = generic
  ##Emergencies 1-99 = surgical
  ##Emergencies 100-199 = medical
  ##Emergencies 200-299 = paediatric
  
  ##Elective 1000 = generic
  
  ## Mapping combined table Treatment function codes
  combined<-
    mutate(combined,traj = case_when(
      `_Elective`==FALSE & (`Treatment Function Code`==100 | `Treatment Function Code`==104 | `Treatment Function Code`==102 | `Treatment Function Code`==106) ~ 1, ## 1 - Emergency Gen Surgical
      `_Elective`==FALSE & `Treatment Function Code`==101 ~ 2,## 2 - Emergency Urology
      `_Elective`==FALSE & (`Treatment Function Code`==140 | `Treatment Function Code`==144 | `Treatment Function Code`==145 | `Treatment Function Code`==146) ~ 3,## 3 - Emergency Maxfax
      `_Elective`==FALSE & `Treatment Function Code`==160 ~ 4,## 4 - Emergency Plastics
      `_Elective`==FALSE & `Treatment Function Code`==120 ~ 5,## 5 - Emergency ENT
      `_Elective`==FALSE & `Treatment Function Code`==110 & `age`>70 ~ 6,## 6 - Emergency Orthogeriatrics
      `_Elective`==FALSE & `Treatment Function Code`==110 ~ 7,## 7 - Emergency Ortho
      `_Elective`==FALSE & `Treatment Function Code`==130 ~ 8,## 8 - Emergency Ophth
      `_Elective`==FALSE & `Treatment Function Code`==107 ~ 9,## 9 - Emergency Vascular
      `_Elective`==FALSE & `Treatment Function Code`==103 ~ 10,## 10 - Emergency Breast
      `_Elective`==FALSE & (`Treatment Function Code`==501 |`Treatment Function Code`==502|`Treatment Function Code`==503) ~ 11,## 11 - Emergency Gynae
      `_Elective`==FALSE & (`Treatment Function Code`==180 | `Treatment Function Code`==190) & `Primary Procedure (OPCS)`!="" ~ 99,## 99 - Emergency, unknown surgical
      `_Elective`==FALSE & `Treatment Function Code`==300 ~ 100,## 100 - Emergency General Medicine
      `_Elective`==FALSE & (`Treatment Function Code`==301 | `Treatment Function Code`==306) ~ 101,## 101 - Emergency Gastro/Hepatology
      `_Elective`==FALSE & (`Treatment Function Code`==302 | `Treatment Function Code`==307)~ 102,## 102 - Emergency Endocrine
      `_Elective`==FALSE & `Treatment Function Code`==320 ~ 103,## 103 - Emergency Cardiology
      `_Elective`==FALSE & (`Treatment Function Code`==340 | `Treatment Function Code`==341)~ 104,## 104 - Emergency Respiratory
      `_Elective`==FALSE & `Treatment Function Code`==350 ~ 105,## 105 - Emergency ID
      `_Elective`==FALSE & (`Treatment Function Code`==303 | `Treatment Function Code`==309) ~ 106,## 106 - Emergency Haematology
      `_Elective`==FALSE & (`Treatment Function Code`==328 | `Treatment Function Code`==329| `Treatment Function Code`==400) ~ 107,## 107 - Emergency Stroke/Neuro
      `_Elective`==FALSE & (`Treatment Function Code`==361) ~ 108,## 108 - Emergency Renal
      `_Elective`==FALSE & (`Treatment Function Code`==370| `Treatment Function Code`==800) ~ 109,## 109 - Emergency Oncology
      `_Elective`==FALSE & (`Treatment Function Code`==430| `Treatment Function Code`==314) ~ 110,## 110 - Emergency Geriatrics/Rehab
      `_Elective`==FALSE & (`Treatment Function Code`==180 | `Treatment Function Code`==190) ~ 199,## 199 - Emergency, unknown medical
      `_Elective`==FALSE & ((`Treatment Function Code`>=211 & `Treatment Function Code`<=291) | `Treatment Function Code`==171| `Treatment Function Code`==420| `Treatment Function Code`==321) ~ 200,## 200 - Emergency paediatric
      `_Elective`==FALSE ~ 999,## 999 = Generic Emergency code
      `_Elective`==TRUE & (`Treatment Function Code`==100 | `Treatment Function Code`==104| `Treatment Function Code`==106) ~ 1001,## 1001 = Elective Gen Surgical
      `_Elective`==TRUE & (`Treatment Function Code`==107) ~ 1002,## 1002 = Elective Vascular
      `_Elective`==TRUE & (`Treatment Function Code`==110) ~ 1003,## 1003 = Elective Ortho
      `_Elective`==TRUE & (`Treatment Function Code`==120) ~ 1004,## 1004 = Elective ENT
      `_Elective`==TRUE & (`Treatment Function Code`==140|`Treatment Function Code`==144) ~ 1005,## 1005 = Elective MaxFax
      `_Elective`==TRUE & (`Treatment Function Code`==160) ~ 1006,## 1006 = Elective Plastics
      `_Elective`==TRUE & (`Treatment Function Code`==502) ~ 1007,## 1007 = Elective Gynae
      `_Elective`==TRUE & (`Treatment Function Code`==101) ~ 1008,## 1008 = Elective Urology
      `_Elective`==TRUE & (`Treatment Function Code`==103) ~ 1009,## 1009 = Elective Breast
      `_Elective`==TRUE & (`Treatment Function Code`==130) ~ 1010,## 1010 = Elective Ophth
      `_Elective`==TRUE & (`Treatment Function Code`==191) ~ 1011,## 1011 = Elective Pain
      `_Elective`==TRUE & (`Treatment Function Code`==300) ~ 1100,## 1100 = Elective Gen Medicine
      `_Elective`==TRUE & (`Treatment Function Code`==301|`Treatment Function Code`==306) ~ 1101,## 1101 = Elective Gastro/Hepatology
      `_Elective`==TRUE & (`Treatment Function Code`==303|`Treatment Function Code`==309) ~ 1102,## 1102 = Elective Haem
      `_Elective`==TRUE & (`Treatment Function Code`==320) ~ 1103,## 1103 = Elective Cardiology
      `_Elective`==TRUE & (`Treatment Function Code`==340) ~ 1104,## 1104 = Elective Respiratory
      `_Elective`==TRUE & (`Treatment Function Code`==370 | `Treatment Function Code`==503 | `Treatment Function Code`==800) ~ 1105,## 1105 = Elective Oncology
      `_Elective`==TRUE & (`Treatment Function Code`==400|`Treatment Function Code`==328|`Treatment Function Code`==329) ~ 1106,## 1106 = Elective Neurology
      `_Elective`==TRUE & (`Treatment Function Code`==410) ~ 1107,## 1107 = Elective Rheumatology
      `_Elective`==TRUE & (`Treatment Function Code`==430|`Treatment Function Code`==314) ~ 1108,## 1108 = Elective Geriatrics
      `_Elective`==TRUE & (`Treatment Function Code`==302) ~ 1109,## 1109 = Elective Endocrine
      `_Elective`==TRUE & (`Treatment Function Code`==350) ~ 1110,## 1110 = Elective ID
      `_Elective`==TRUE & (`Treatment Function Code`==361) ~ 1111,## 1111 = Elective Renal
      `_Elective`==TRUE & (`Treatment Function Code`==171|`Treatment Function Code`==321|`Treatment Function Code`==420|(`Treatment Function Code`>=211 & `Treatment Function Code`<=291)) ~ 1200,## 1200 = Elective Paediatrics
      `_Elective`==TRUE ~ 1000## 1000 = Generic Elective code
    ))
  
  ##Second and subsequent episodes may need treating differently
  ## allow a "pre trajectory" which fits before
  combined$pretraj<-NA 
  combined<-
    mutate(combined,pretraj = case_when(
      `Episode Number`==1 & (traj==1 | traj==2 | traj==9  | traj==11 | traj==99) ~ 10001,##10001=SAU
      `Episode Number`==1 & (traj>=100 & traj <=199) ~ 10002,##10002=MAU
      `Episode Number`==1 & traj==200 ~ 10003##10003=paeds
    ))
  combined$epN_row_id<-match(paste0(combined$`_SpellID`,combined$`Episode Number`+1),paste0(combined$`_SpellID`,combined$`Episode Number`))
  print("* Added Trajectories *")
  return(combined)
}  

## DESCIPTION
fnLinkCC <- function(icu_raw_segments){ 
  icu_segments<-dplyr::select(icu_raw_segments,"_TLSpellDigest","Episode Number","Last Episode In Spell Indicator","ccstay","_ccseg",
                              "_SegmentStart_DateTime","_SegmentEnd_DateTime","_SegmentDischReady_DateTime","_SegmentStart_Offset",
                              "_SegmentEnd_Offset","_SegmentDischReady_Offset","_RealCritCare","_CCTransfer","Critical Care Level 2 Days",
                              "Critical Care Level 3 Days")
  icu_segments$row_id<-1:nrow(icu_segments)
  icu_segments$segN_row_id<-match(paste0(icu_segments$`_TLSpellDigest`,icu_segments$`_ccseg`+1),paste0(icu_segments$`_TLSpellDigest`,icu_segments$`_ccseg`))
  return(icu_segments)
}  

## DESCIPTION
fnLinkElec <- function(elective_spells,combined,icu_segments){ 
  
  elective_spells$ep1_row_id<-match(paste0(elective_spells$`_SpellID`,"1"),paste0(combined$`_SpellID`,combined$`Episode Number`))
  elective_spells$cc1_row_id<-match(paste0(elective_spells$`_TLSpellDigest`,"1"),paste0(icu_segments$`_TLSpellDigest`,icu_segments$`_ccseg`))
  elec_joined<-left_join(elective_spells,icu_segments,by=c("cc1_row_id"="row_id"))
  elec_joined$cc_test<-abs(elec_joined$`_SegmentStart_DateTime`-elec_joined$`_SpellStart_DateTime`)
  elective_spells$cc_start<-ifelse(elec_joined$cc_test<24*3600,1,0)
  elective_spells$cc_start[is.na(elective_spells$cc_start)]<-0
  elective_spells$cc1_row_id[is.na(elective_spells$cc1_row_id)]<-0  
  return(elective_spells)
}

## DESCIPTION
fnLinkEmg <- function(emergency_spells,combined,icu_segments){ 
  emergency_spells$ep1_row_id<-match(paste0(emergency_spells$`_SpellID`,"1"),paste0(combined$`_SpellID`,combined$`Episode Number`))
  emergency_spells$cc1_row_id<-match(paste0(emergency_spells$`_TLSpellDigest`,"1"),paste0(icu_segments$`_TLSpellDigest`,icu_segments$`_ccseg`))
  emerg_joined<-left_join(emergency_spells,icu_segments,by=c("cc1_row_id"="row_id"))
  emerg_joined$cc_test<-abs(emerg_joined$`_SegmentStart_DateTime`-emerg_joined$`_SpellStart_DateTime`)
  emergency_spells$cc_start<-ifelse(emerg_joined$cc_test<18*3600,1,0)
  emergency_spells$cc_start[is.na(emergency_spells$cc_start)]<-0
  emergency_spells$cc1_row_id[is.na(emergency_spells$cc1_row_id)]<-0
  return(emergency_spells)
}

## Creates a data frame with each row as a time of arrival.  These times are defined by the 
## frequency tables previosuly created and randomised with an exponetial wait between arrivals.
## At the end of this function the patients have not been defined and so the Spell_ID column is 
## blank.
emergency_gen_table <- function() {
  pb <- progress_bar$new(format="[:bar] :percent eta: :eta",total=nrow(emergency_freq)+1)
  last_time <- 0 ##last time we generated
  next_change <- as.numeric(emergency_freq[1,]$dateTime) ## next rate change
  cur_freq <- 0 ##current frequency (in "per day" units)
  t_skip <- 0 ##time skipped due to zero frequency, or because next admission was after a rate change
  freq_table_index <- 1 ##position in the frequency table
  gaps <- vector()
  time_at <- vector()
  
  repeat {
    while (cur_freq==0) {
      ##if frequency is ever zero (true at the start), keep skipping until we hit a positive frequency
      ##t_skip represents how long we've skipped before the current gap
      t_skip <- next_change - last_time
      cur_freq <- emergency_freq[freq_table_index,]$`_correctedN`
      next_change <- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
      freq_table_index <- freq_table_index+1
      pb$tick()
      if (freq_table_index==nrow(emergency_freq)) { break }
    }
    
    ##main generator here - rates in the files are per day, assume poisson process
    tmp_gap <- rexp(1,cur_freq/86400)+t_skip
    
    ## if the next admission would occur after a rate change, skip it. Take advantage of the fact Poisson processes have no memory to start a new one at the rate change (this has variable mathematical validity!)
    if ((tmp_gap + last_time)>next_change) {
      t_skip <- next_change - last_time
      cur_freq <- emergency_freq[freq_table_index,]$`_correctedN`
      next_change <- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
      freq_table_index <- freq_table_index+1
      pb$tick()
      #print(freq_table_index)
      if (freq_table_index==nrow(emergency_freq)) { break }
    } else {
      t_skip<-0
      last_time<-last_time+tmp_gap
      gaps <- c(gaps,tmp_gap)
      time_at <- c(time_at,last_time)
    }
    
    
  }
  spell_id<-0
  return(data.frame(gaps,time_at,spell_id))
}

## Creates a data frame with each row as a time of arrival.  These times are defined by the 
## frequency tables previosuly created and sampled from using a possion distribution
## At the end of this function the patients have not been defined and so the Spell_ID column is 
## blank. 
elective_gen_table <- function() {
  pb <- progress_bar$new(format="[:bar] :percent eta: :eta",total=nrow(elective_freq)+1)
  cur_time <- 0 ##current time
  last_time <- 0 ##last time we generated
  cur_freq <- 0 ##current frequency (in "per day" units)
  freq_table_index <- 1 ##position in the frequency table
  gaps <- vector()
  time_at <- vector()
  
  repeat {
    cur_freq <- elective_freq[freq_table_index,]$`_correctedN`
    cur_time <- as.numeric(elective_freq[freq_table_index,]$dateTime)+7*3600 ##admissions at 0700
    gen_n <- rpois(1,cur_freq)
    if (gen_n>0) {
      gaps<-c(gaps,cur_time-last_time)
      time_at<-c(time_at,cur_time)
      last_time<-cur_time
      gaps<-c(gaps,rep(0,gen_n-1))
      time_at<-c(time_at,rep(cur_time,gen_n-1))
    }
    freq_table_index <- freq_table_index+1
    pb$tick()
    if (freq_table_index==nrow(elective_freq)) { break }
  }
  print("test")
  spell_id<-0
  return(data.frame(gaps,time_at,spell_id))
}

## To fill the arrivals table, we randomise and sample from the known patients for each time point.
## This is done by looping through the table and searching "randomly" for a patient which fits into 
## a search window.  Emergency patients have both time and date searches applied.
emergency_gen_patients <- function(emergency_table){
  ##expects emergency_spells as-is
  ##emergency_gen_table needs to contain 2KMD_date, time, and bizday
  emergency_table<-mutate(emergency_table,idx_datetime=as.POSIXct(time_at,origin="1970-01-01 00:00.00 UTC"))
  emergency_table<-mutate(emergency_table,bizday=is.bizday(as.character(idx_datetime),'Rmetrics/LONDON')) ##nb needs as.character as usual to avoid BST issues
  emergency_table<-mutate(emergency_table,y2kdate=as.Date(format(idx_datetime,"2000-%m-%d")))
  emergency_table<-mutate(emergency_table,idx_time=(as.numeric(idx_datetime) %% 86400))
  
  searchTimeWindow <- as.numeric(searchTimeWindowEm,units="secs")
  searchDateWindow <- as.numeric(searchDateWindowEm,units="days") 

  gen_spell_ids <- emergency_table$spell_id
  gen_y2kdate <- emergency_table$y2kdate 
  gen_bizday <- emergency_table$bizday
  gen_time <- emergency_table$idx_time 
  
  sp_spell_ids <- emergency_spells$`_SpellID`
  sp_bizday <- emergency_spells$`_bizday`
  sp_y2kdate <- emergency_spells$`_2KMD_Date`
  sp_time <- emergency_spells$`_Start_Time`
  
  n_spells <- dim(emergency_spells)[1]
  rnd_spells <- c(rep(1:n_spells))
  
  for (ii in 1:dim(emergency_table)[1]){
    rnd_spells <- sample(rnd_spells)
    
    startTsearch = gen_time[ii] - searchTimeWindow
    endTsearch = gen_time[ii] + searchTimeWindow
    startDsearch = gen_y2kdate[ii] - searchDateWindow
    endDsearch = gen_y2kdate[ii] + searchDateWindow
    
    for (kk in 1:n_spells){
      sp_rnd_kk <- rnd_spells[kk]
      
      if (sp_bizday[sp_rnd_kk] == gen_bizday[ii]){
        if ((sp_y2kdate[sp_rnd_kk] > startDsearch & sp_y2kdate[sp_rnd_kk] < endDsearch) |
            ((sp_y2kdate[sp_rnd_kk] + 365) > startDsearch & (sp_y2kdate[sp_rnd_kk] + 365) < endDsearch) | 
            ((sp_y2kdate[sp_rnd_kk] - 365) > startDsearch & (sp_y2kdate[sp_rnd_kk] - 365) < endDsearch)) {
          if ((sp_time[sp_rnd_kk] > startTsearch & sp_time[sp_rnd_kk] < endTsearch) | 
              ((sp_time[sp_rnd_kk] + 86400) > startTsearch & (sp_time[sp_rnd_kk] + 86400) < endTsearch) | 
              ((sp_time[sp_rnd_kk] - 86400) > startTsearch & (sp_time[sp_rnd_kk] - 86400) < endTsearch)) {
            gen_spell_ids[ii] = sp_spell_ids[sp_rnd_kk]
            break
          }
        }
      }
    }
  }
  
  emergency_table$spell_id<-gen_spell_ids
  return (emergency_table)
  
}

## To fill the arrivals table, we randomise and sample from the known patients for each time point.
## This is done by looping through the table and searching "randomly" for a patient which fits into 
## a search window. Elective patients have just a date search applied.
elective_gen_patients <- function(elective_table){
  ##expects elective_spells as-is
  ##elective_gen_table needs to contain 2KMD_date and bizday (time not relevant as we're assuming 0700)
  elective_table<-mutate(elective_table,idx_datetime=as.POSIXct(time_at,origin="1970-01-01 00:00.00 UTC"))
  elective_table<-mutate(elective_table,bizday=is.bizday(as.character(idx_datetime),'Rmetrics/LONDON')) ##nb needs as.character as usual to avoid BST issues
  elective_table<-mutate(elective_table,y2kdate=as.Date(format(idx_datetime,"2000-%m-%d")))
  ##elective_table<-mutate(elective_table,idx_time=(as.numeric(idx_datetime) %% 86400))
  
  searchDateWindow <- as.numeric(searchDateWindowEl,units="days")
  
  gen_spell_ids <- elective_table$spell_id
  gen_y2kdate <- elective_table$y2kdate 
  gen_bizday <- elective_table$bizday
  
  sp_spell_ids <- elective_spells$`_SpellID`
  sp_bizday <- elective_spells$`_bizday`
  sp_y2kdate <- elective_spells$`_2KMD_Date`
  sp_time <- elective_spells$`_Start_Time`
  
  n_spells <- dim(elective_spells)[1]
  rnd_spells <- c(rep(1:n_spells))
  
  for (ii in 1:dim(elective_table)[1]){
    rnd_spells <- sample(rnd_spells)
    
    startDsearch = gen_y2kdate[ii] - searchDateWindow
    endDsearch = gen_y2kdate[ii] + searchDateWindow
    
    for (kk in 1:n_spells){
      sp_rnd_kk <- rnd_spells[kk]
      
      if (sp_bizday[sp_rnd_kk] == gen_bizday[ii]){
        if ((sp_y2kdate[sp_rnd_kk] > startDsearch & sp_y2kdate[sp_rnd_kk] < endDsearch) |
            ((sp_y2kdate[sp_rnd_kk] + 365) > startDsearch & (sp_y2kdate[sp_rnd_kk] + 365) < endDsearch) | 
            ((sp_y2kdate[sp_rnd_kk] - 365) > startDsearch & (sp_y2kdate[sp_rnd_kk] - 365) < endDsearch)) {
          gen_spell_ids[ii] = sp_spell_ids[sp_rnd_kk]
            break
        }
      }
    }
  }
  
  elective_table$spell_id <- gen_spell_ids
  return (elective_table)
}

## helper function to pull data - actually only iterate is used
consume <- function(x) {
  i <- 0
  function() {
    i <<- i + 1
    #print(x[[i]])
    x[[i]]
  }
}

## DESCIPTION
iterate  <- function() {
  i <- 0
  function() {
    i <<- i + 1
    if (i %% 1000 == 0) print(i)
    i
  }
}