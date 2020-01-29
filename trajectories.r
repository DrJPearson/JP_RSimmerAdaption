################################################################################
# Project Name: TLSimmerTest
# Date Last Updated: 27/11/19
# Summary: Creation of trajectories.  Each one related to a a set of rules for 
# specific patient types allocated them to first, second and third choice wards.
# Some wards have timeouts and some have defined follow-on wards.
################################################################################

## All trajectories defined in here for reuse purposes

## Data for various behaviours
## pr1 - best wards
## pr2 - OK wards
## pr3 - non-ideal wards
## dur - duration before moving to next stage (0=forever)
## nxt - next stage code number (active at end of duration or possibly if kicked out)

traj_pr1 <- list()
traj_pr2 <- list()
traj_pr3 <- list()
traj_dur <- list()
traj_nxt <- list()

any_medical <- c("9","15","26","23","24","22","29","31","7","8","11","26","12Esc","18Esc","17Esc","ACU","CDU","AMU1","AMU4","AMU4Esc","33","3","6")
any_surgical <- c("8","11","12","14","18","SAU20","21","21CC","26","27","28")

x <- 10002 ## MAU
traj_pr1[[x]] <- c("AMU1","AMU4","AMU4Esc")
traj_pr2[[x]] <- c("ACU","17Esc","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- 48*3600
traj_nxt[[x]] <- x

x <- 10001 ## SAU
traj_pr1[[x]] <- c("SAU20")
traj_pr2[[x]] <- c("11","8","26","21","21CC")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- 48*3600
traj_nxt[[x]] <- x

x <- 10003 ## Paeds
traj_pr1[[x]] <- c("32")
traj_pr2[[x]] <- c("30")
traj_pr3[[x]] <- c("32","30")
traj_dur[[x]] <- 48*3600
traj_nxt[[x]] <- x

x <- 1 ## Emergency Gen Surgical
traj_pr1[[x]] <- c("8","11")
traj_pr2[[x]] <- c("SAU20","21","21CC","12","26")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 2 ## Emergency Urology
traj_pr1[[x]] <- c("14")
traj_pr2[[x]] <- c("SAU20","21","21CC","12","26")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 3 ## Emergency Maxfax
traj_pr1[[x]] <- c("18","18Esc","27")
traj_pr2[[x]] <- any_surgical
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 4 ## Emergency Plastics
traj_pr1[[x]] <- c("27")
traj_pr2[[x]] <- c("28")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 5 ##Emergency ENT
traj_pr1[[x]] <- c("18","18Esc")
traj_pr2[[x]] <- any_surgical
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 6 ## Emergency Orthogeriatrics 
traj_pr1[[x]] <- c("31","29")
traj_pr2[[x]] <- c("3","28","27")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 7 ## Emergency Ortho
traj_pr1[[x]] <- c("28")
traj_pr2[[x]] <- c("27","26")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 8 ## Emergency Ophth
traj_pr1[[x]] <- c("18","18Esc")
traj_pr2[[x]] <- any_surgical
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 9 ## Emergency Vascular
traj_pr1[[x]] <- c("26")
traj_pr2[[x]] <- c("21","21CC","SAU20","27","8","11")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 10 ## Emergency Breast
traj_pr1[[x]] <- c("12","12Esc")
traj_pr2[[x]] <- c("8","11","21","21CC","SAU20")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 11 ## Emergency Gynae
traj_pr1[[x]] <- c("12","12Esc")
traj_pr2[[x]] <- c("SAU20","8","11","21","21CC")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 99 ## Emergency, unknown surgical
traj_pr1[[x]] <- c("SAU20","8","11","26","12","21","21CC")
traj_pr2[[x]] <- any_surgical
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 100 ## Emergency General Medicine
traj_pr1[[x]] <- c("9","ACU","CDU","AMU1","AMU4")
traj_pr2[[x]] <- c("17Esc","AMU4Esc","12Esc","AMU1") ##AMU1 duplicated as all the others may be closed
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 101 ## Emergency Gastro/Hepatologyx
traj_pr1[[x]] <- c("26","11","8")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 102 ## Emergency Endocrine
traj_pr1[[x]] <- c("9","ACU","CDU")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 103 ## Emergency Cardiology
traj_pr1[[x]] <- c("22")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 104 ## Emergency Respiratory
traj_pr1[[x]] <- c("23")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 105 ## Emergency ID
traj_pr1[[x]] <- c("7")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 106 ## Emergency Haematology
traj_pr1[[x]] <- c("33","24")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 107 ## Emergency Stroke/Neuro
traj_pr1[[x]] <- c("6")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 108 ## Emergency Renal
traj_pr1[[x]] <- c("15")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 109 ## Emergency Oncology
traj_pr1[[x]] <- c("24")
traj_pr2[[x]] <- c("33","17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 110 ## Emergency Geriatrics/Rehab
traj_pr1[[x]] <- c("31","29","3")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- 7*24*3600 #move on after a week - could change?
traj_nxt[[x]] <- 111

x <- 111 ## Rehab elderly (not starting trajectory)
traj_pr1[[x]] <- c("F5","F6","31","29") # JP - WHATS F5 AND F6
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 199 ## Emergency, unknown medical
traj_pr1[[x]] <- c("9","ACU","CDU")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x
 
x <- 200 ## Emergency paediatric
traj_pr1[[x]] <- c("30")
traj_pr2[[x]] <- c("32")
traj_pr3[[x]] <- c("30","32")
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x
 
x <- 999 ## Generic Emergency code
traj_pr1[[x]] <- c("9","ACU","CDU")
traj_pr2[[x]] <- any_medical
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1001 ## Elective Gen Surgical
traj_pr1[[x]] <- c("8","11")
traj_pr2[[x]] <- c("SAU20","12","26","21","21CC")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1002 ## Elective Vascular
traj_pr1[[x]] <- c("26")
traj_pr2[[x]] <- c("SAU20","8","11","27","28","21","21CC")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1003 ## Elective Ortho
traj_pr1[[x]] <- c("28","27")
traj_pr2[[x]] <- c("26")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1004 ## Elective ENT
traj_pr1[[x]] <- c("18","18Esc")
traj_pr2[[x]] <- c("27")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1005 ## Elective MaxFax
traj_pr1[[x]] <- c("18","18Esc")
traj_pr2[[x]] <- c("27")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1006 ## Elective Plastics
traj_pr1[[x]] <- c("27","28")
traj_pr2[[x]] <- c("26")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1007 ## Elective Gynae
traj_pr1[[x]] <- c("12","12Esc")
traj_pr2[[x]] <- c("21","21CC","SAU20","8","11","27","28")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1008 ## Elective Urology
traj_pr1[[x]] <- c("14","12","12Esc")
traj_pr2[[x]] <- c("SAU20","21","21CC","12","26")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1009 ## Elective Breast
traj_pr1[[x]] <- c("12","12Esc")
traj_pr2[[x]] <- c("21","21CC","SAU20","8","11","27","28")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1010 ## Elective Ophth
traj_pr1[[x]] <- c("18","18Esc","27")
traj_pr2[[x]] <- c("31","3")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1011 ## Elective Pain
traj_pr1[[x]] <- c("8","11")
traj_pr2[[x]] <- c("SAU20","12","26")
traj_pr3[[x]] <- any_surgical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1100 ## Elective Gen Medicine
traj_pr1[[x]] <- c("9","ACU","CDU")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1101 ## Elective Gastro/Hepatology
traj_pr1[[x]] <- c("26","11","8")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1102 ## Elective Haem
traj_pr1[[x]] <- c("33","24")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1103 ## Elective Cardiology
traj_pr1[[x]] <- c("22")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x
 
x <- 1104 ## Elective Respiratory
traj_pr1[[x]] <- c("23")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1105 ## Elective Oncology
traj_pr1[[x]] <- c("24")
traj_pr2[[x]] <- c("33","17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1106 ## Elective Neurology
traj_pr1[[x]] <- c("6")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1107 ## Elective Rheumatology
traj_pr1[[x]] <- c("F5","F6")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1108 ## Elective Geriatrics
traj_pr1[[x]] <- c("31","29","3")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- 7*24*3600 #move on after a week - could change?
traj_nxt[[x]] <- 111

x <- 1109 ## Elective Endocrine
traj_pr1[[x]] <- c("9","ACU","CDU")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1110 ## Elective ID
traj_pr1[[x]] <- c("7")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1111 ## Elective Renal
traj_pr1[[x]] <- c("15")
traj_pr2[[x]] <- c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU","12Esc")
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1200 ## Elective Paediatrics
traj_pr1[[x]] <- c("30")
traj_pr2[[x]] <- c("32")
traj_pr3[[x]] <- c("30","32")
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

x <- 1000 ## Generic Elective code
traj_pr1[[x]] <- c("8","11","21","21CC")
traj_pr2[[x]] <- any_surgical
traj_pr3[[x]] <- any_medical
traj_dur[[x]] <- Inf
traj_nxt[[x]] <- x

print("* Trajectories Loaded *")

cc <- 0
for (i in 1:length(traj_pr1)) {
  if (!is.null(traj_pr1[[i]])) {
    print(i)
    cc <- cc + 1
  }
}

print(cc)