################################################################################
# Version Control
# Project Name: RSimmerAdaption
# Version Status: Draft - Not for wider use
# Version Number: v0.01
# Date Last Updated: 17/01/19
# Summary: This tool produces a simulation of patient flows through wards
# -----------------------------------------------------------------------------
# Contacts
# Created by: Tom Lawton | Michael McCooe | Dan Mason | Abigail Dutton @ BTHFT
# See GitHub https://github.com/thigger/AA-WinterPressures for original code
# Adapted By: Jonathan Pearson (jonathanpearson@nhs.net)
# -----------------------------------------------------------------------------
################################################################################

Initalize = Sys.time()
set.seed(12346)

## Sources
source("packages.r")
source("functions.r")
source("trajectories.r")
source("simWrap.r")

here::here()

## Hard coded inputs
load_rmetrics_calendars(2000:2022) ##nb we only get these holidays so may need extending in future
startDate <- as.POSIXct("2018-04-01 00:00:00 GMT")
endDate <- as.POSIXct("2018-04-30 23:59:59 GMT") ## two years for now to show winter more clearly as the model takes a few months to wind up
print(paste("Packages, Functions and Hard Paramters Loaded", Sys.time() - Initalize))

## Load Raw Data 
data <- read.csv("CreatedFake.csv", header=TRUE,sep=",",check.names=FALSE)
wards<-read.csv("Wards.csv", header=TRUE,sep=",")
##dataframe - Ward: [Name], Beds: [Number], Restrictions: [F - female only, P - paeds only], Surge: [1 - surge area (opening defined in model, starts shut)]
wards$Ward<-as.character(wards$Ward)
#for some reason we end up with the ward names as a factor variable which causes trouble when it's used in the "put the patient anywhere" rule
print("* Loaded *")
print(paste("Raw data read in", Sys.time() - Initalize))

## Raw data summaries and checks
## TODO

## Manipualte Raw data and create tables for spells, episodes and critical care information
data <- fnDataManipulate(data)  
basedata <- fnBaseData(data)  # basedata will contain each spell
repeateddata <- fnRepeatedData(data) # repeateddata will contain each *episode* (not ward stay)
critcaredata <- fnCCData(data) # critical care data as before.
print(paste("Data Manipulation", Sys.time() - Initalize))

## Create frequency count over time for number of Elective and Emergency Spells
elective_spells <- fnSpellsFilter(filter(basedata,`_Elective`==TRUE))
elective_freq <- fnListElective(elective_spells,
                                resolutionTime <- as.difftime(24,units="hours"),
                                searchDateWindow <- as.difftime(2,units="weeks"))
print(paste("Create Elective List", Sys.time() - Initalize))
emergency_spells <- fnSpellsFilter(filter(basedata,`_Elective`==FALSE))
emergency_freq <- fnListEmergency(emergency_spells,
                                  resolutionTime <- as.difftime(2,units="hours"),
                                  searchDateWindow <- as.difftime(2,units="weeks"),
                                  searchTimeWindow <- searchTimeWindow <- as.difftime(3,units="hours"))
print(paste("Create Emergency List", Sys.time() - Initalize))

## DESCIPTION
combined_episodes <- fnCombined(basedata,repeateddata)
critcare_segments <- fnLinkCC(critcaredata)
emergency_spells <- fnLinkElec(emergency_spells,combined_episodes,critcare_segments)
elective_spells <- fnLinkEmg(elective_spells,combined_episodes,critcare_segments)
print(paste("Link Spells and Episodes", Sys.time() - Initalize))


##fudge 4 day stay for anyone we don't know MOVE ELSEWHERE!
emergency_spells$duration<-as.numeric(emergency_spells$`_Discharge_DateTime`)-as.numeric(emergency_spells$`_SpellStart_DateTime`)
emergency_spells$duration[is.na(emergency_spells$duration)]<-4*86400
elective_spells$duration<-as.numeric(elective_spells$`_Discharge_DateTime`)-as.numeric(elective_spells$`_SpellStart_DateTime`)
elective_spells$duration[is.na(elective_spells$duration)]<-4*86400

combined_episodes$`_EpisodeEnd_Offset`[is.na(combined_episodes$`_EpisodeEnd_Offset`)]<-4*86400

##base a progress bar on the emergency admissions as they're the biggest and most detailed
pb <- progress_bar$new(format="[:bar] :percent eta: :eta",total=nrow(emergency_freq))

## function to produce a patient from an index time will return a patient's entire row presently indexed by - business day,
## time +/- n hours, date +/- n weeks old version worked but is quite slow due to the long and repeated lookups. 

## Create 
searchTimeWindowEm <- as.difftime(3,units="hours") ## two hours before and after
searchDateWindowEm <- as.difftime(2,units="weeks") ## 2 weeks before and after
searchDateWindowEl <- as.difftime(2,units="weeks") ## 2 weeks before and after

## Main simulation
print("* Simulation started (no output) *")
envs<-future_lapply(1:12,simmer_wrapper) ##48
print("* Simulation finished *")


## Out put summaries
resources<-get_mon_resources(envs)
attribs<-get_mon_attributes(envs)
arrivals<-get_mon_arrivals(envs)
print(paste("Run Finished", Sys.time() - Initalize))

saveRDS(resources,"1-resources.rds")
saveRDS(attribs,"1-attribs.rds")
saveRDS(arrivals,"1-arrivals.rds")

## WHAT's THIS?
max_attribs<-attribs %>% group_by(key,replication) %>% summarise(value=max(value))
max_attribs<-filter(max_attribs,!grepl("Open",key))
plot(max_attribs)

print(paste("Code End", Sys.time() - Initalize))
resources$time <- as.POSIXct(resources$time,origin="1970-01-01 00:00.00 UTC")
resources$rdate <- round_date(resources$time,unit="day")
resources$rwdate <- round_date(resources$time,unit="week")

print(ggplot(resources,aes(x=rdate,y=server,color=replication)) +
        geom_point(alpha=0.1,shape=16) +
        scale_color_gradient(low="blue", high="red",guide=FALSE) +
        stat_summary(aes(x=rwdate,y=server),fun.data="mean_sdl",geom="smooth",se=TRUE,size=2)  +
        xlim(startDate,endDate)  +
        labs(x="Date",y="Beds") + theme_bw(base_size=12)+
        ggtitle("testchart"))

get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(elective_patient, fill = get_palette)
plot(emergency_patient, fill = get_palette)
