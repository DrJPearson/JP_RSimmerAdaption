################################################################################
# Project Name: RSimmerAdaption
# Date Last Updated: 17/01/20
# Summary: RSimmer function - NOT ADAPTED YET
################################################################################

simmer_wrapper <- function(i) {
  
  ## Needs when escalation wards are added as a generator near bottom of function
  source("WardController.R",local=TRUE) ## local=TRUE keeps it within the wrapper
  
  ## Generate tables for all time points and populate with patients arriving
  print("*started*")
  emergency_table <- emergency_gen_table()
  elective_table <- elective_gen_table()
  print("* times generated *")
  emergency_table <- emergency_gen_patients(emergency_table)
  elective_table <- elective_gen_patients(elective_table)
  print(nrow(emergency_table))
  print(nrow(elective_table))
  
  ## Filter generated patients into Spells, episodes, cc IDs and CC starts
  emergency_table$sp_row_id <- match(emergency_table$spell_id,emergency_spells$`_SpellID`)
  elective_table$sp_row_id <- match(elective_table$spell_id,elective_spells$`_SpellID`)
  elective_table$ep1_row_id <- elective_spells$ep1_row_id[elective_table$sp_row_id]
  emergency_table$ep1_row_id <- emergency_spells$ep1_row_id[emergency_table$sp_row_id]
  elective_table$cc1_row_id<-elective_spells$cc1_row_id[elective_table$sp_row_id]
  emergency_table$cc1_row_id<-emergency_spells$cc1_row_id[emergency_table$sp_row_id]
  elective_table$cc_start<-elective_spells$cc_start[elective_table$sp_row_id]
  emergency_table$cc_start<-emergency_spells$cc_start[emergency_table$sp_row_id]
  print("* patients generated *")
  
  ## Initialize and name a simulation enviroment 
  env<-simmer("hospital")
  
  ## This path records occupancy of ward 21
  catch_unfinished<-trajectory() %>% 
    log_("***UNFINISHED ARRIVAL - shouldn't see yet***") %>% 
    log_(function(){paste0("21:",get_queue_count(env,"21"),"/",get_queue_size(env,"21"))})
  
  ## Sets ward capacity to cur_ward_idx; in_cc to 1; looks up the patient ID, number of transfers, next ward, start time and discharge time
  set_CC_attributes<-trajectory() %>% ## 3 items in this trajectory (for rollbacks)
    set_attribute("cur_ward_idx", function() {
      match(get_selected(env),wards$Ward)
    }) %>% 
    set_attribute("in_cc",1) %>% 
    set_attribute(c("nxt_cc_row_id","start_time_cc_seg","dcr_time_cc_seg","CCTransfer"), function() {
      cur_cc_row_id<-get_attribute(env,"nxt_cc_row_id")
      CCTransfer<-critcare_segments[cur_cc_row_id,"_CCTransfer"]
      nxt_cc_row_id<-critcare_segments[cur_cc_row_id,"segN_row_id"]
      start_time_cc_seg<-if (!is.na(nxt_cc_row_id)) critcare_segments[nxt_cc_row_id,"_SegmentStart_Offset"]+get_attribute(env,"start_time") else Inf
      dcr_time_cc_seg<-as.numeric(critcare_segments[cur_cc_row_id,"_SegmentDischReady_Offset"]+get_attribute(env,"start_time"))
      return(c(nxt_cc_row_id,start_time_cc_seg,dcr_time_cc_seg,CCTransfer))
    })
  
  ## records an admission delay then goes back 4?
  CC_admission_delayed<-trajectory() %>% 
    set_global(function() {
      return(paste0(get_selected(env),"_Admissions_12H_Delayed"))
    },1,mod="+") %>%
    rollback(4) # to select in CC_admission
  
  ## Admission occurs (but can be delayed any time in thefirst 12 hours)
  CC_admission_seize<-trajectory() %>% 
    renege_in(12*3600,out=CC_admission_delayed, keep_seized=TRUE) %>% ##queue for 12hrs before marking delayed transfer
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10+5,-1,-1) }) %>% ## allow queueing
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    release_selected(id=1) 
  
  CC_admission<-trajectory() %>% 
    select(function() { if (critcare_segments[get_attribute(env,"nxt_cc_row_id"),"_RealCritCare"]) "ICU" else c("21CC","ICU") },"first-available") %>% 
    branch(function() {
      if (get_seized_selected(env)>0) 0 else 1
    },continue=TRUE,CC_admission_seize) %>% 
    join(set_CC_attributes) %>% 
    rollback(8) ##nb log ##nb rollback includes each individual item within the joined trajectory above
  ##rollback to timeout in common_patient
  
  CC_transfer_delayed<-trajectory() %>% 
    set_global(function() {
      return(paste0(get_selected(env),"_Admissions_12H_Delayed"))
    },1,mod="+") %>%
    rollback(2) # to renege_in in CC_transfer_seize
  
  CC_transfer_seize<-trajectory() %>% 
    renege_in(12*3600,out=CC_transfer_delayed, keep_seized=TRUE) %>% ##queue for 12hrs before marking delayed transfer
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10+6,-1,-1) }) %>% ## allow queueing, higher prio as we'll unblock a cc bed
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    release_selected(id=1) 
  
  CC_transfer<-trajectory() %>% 
    select(function() { if (critcare_segments[get_attribute(env,"nxt_cc_row_id"),"_RealCritCare"]) "ICU" else "21CC" },"first-available") %>% 
    branch(function() {
      if (get_seized_selected(env)>0) return(0)
      return(1)
    },continue=TRUE,CC_transfer_seize) %>% 
    ##if we're here, we're happy with current bed
    join(set_CC_attributes)
  
  CC_discharge_delay<-trajectory() %>% 
    set_global("CC_Discharges_24H_Delayed",1,mod="+") %>% 
    rollback(2) ##to renege_in in CC_discharge_to_ward
  
  CC_discharge_to_ward<-trajectory() %>% 
    ##discharge from CC to a pr1 bed
    renege_in(24*3600,out=CC_discharge_delay, keep_seized=TRUE) %>% ##queue for 24hrs before marking delayed discharge
    select(function() {
      traj_pr1[[get_attribute(env,"cur_traj")]]
    },"shortest-queue-available") %>%
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10,-1,-1) }) %>% ## allow queueing
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    release_selected(id=1) %>% 
    set_attribute("cur_ward_idx", function() {   match(get_selected(env),wards$Ward)  }) %>% 
    set_attribute("in_cc",0) %>% 
    set_attribute("dcr_time_cc_seg",Inf)
  ##rolls back into end of CC_discharge
  
  CC_discharge<-trajectory() %>% 
    ##either to a ward, or they're being transferred from one CC area to another
    ##if CCTransfer == 1 then do a transfer (can we use CC_admission?)
    ##if CCTransfer == 0 then transfer to a pr1 ward and queue if needed
    branch(function(){
      get_attribute(env,"CCTransfer")+1
    },continue=TRUE,CC_discharge_to_ward,CC_transfer) %>% 
    ##and update dcr_time_cc_seg to end of spell
    rollback(4) ## to timeout in common_patient
  
  common_seize_new_rollback<-trajectory() %>%
    set_attribute("pr",1,mod="+") %>% 
    rollback(4) ## back to branch in common_patient

  common_seize_new_ward<-trajectory() %>% 
    ##shouldn't get here with pr>3 as the rollback goes back into common_patient
    select(function() {
      pr<-get_attribute(env,"pr")
      switch(pr,
             traj_pr1[[get_attribute(env,"cur_traj")]],
             traj_pr2[[get_attribute(env,"cur_traj")]],
             traj_pr3[[get_attribute(env,"cur_traj")]])
    },"first-available",id=1) %>%
    seize_selected(reject=common_seize_new_rollback,continue=FALSE,id=1) %>% ##no queueing as prio 0
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  }) %>% 
    release_selected() %>% 
    set_attribute("cur_ward_idx", function() {   match(get_selected(env,id=1),wards$Ward)  })
    ##falls back into common_patient just before rollback
  
  ## path that all patients use
  ## when we arrive here, we have a bed already (from elective or emergency)
  ## seize bed resource
  ## calcualte how long the patient will stay
  common_patient<-trajectory() %>% 
    seize("bed") %>% 
    timeout(function() { 
      max(0,
          min(traj_dur[[get_attribute(env,"cur_traj")]],
              get_attribute(env,"end_time_ep")-now(env),
              get_attribute(env,"start_time_cc_seg")-now(env), #will have moved on since admission
              get_attribute(env,"dcr_time_cc_seg")-now(env)
          )
      )
    }) %>% 
    set_attribute(c("cur_traj","nxt_traj","end_time_ep","cur_ep_row_id"),function() {
      now<-now(env)
      start_time_spell<-get_attribute(env,"start_time")
      end_time_ep<-get_attribute(env,"end_time_ep")
      end_time_spell<-get_attribute(env,"end_time_spell")
      if (now>=end_time_ep) {
        if (now<end_time_spell) {
          cur_ep_row_id<-get_attribute(env,"cur_ep_row_id")
          cur_ep_row_id<-as.numeric(combined_episodes[[cur_ep_row_id,"epN_row_id"]])
          if (!is.na(cur_ep_row_id)){
            cur_traj<-as.numeric(combined_episodes[[cur_ep_row_id,"traj"]])
            nxt_traj<-traj_nxt[[cur_traj]]
            end_time_ep<-as.numeric(combined_episodes[[cur_ep_row_id,"_EpisodeEnd_Offset"]])+start_time_spell
          } else {
            cur_traj<-get_attribute(env,"cur_traj")
            nxt_traj<-traj_nxt[[cur_traj]]
            end_time_ep<-end_time_spell
          }
          return(as.numeric(c(cur_traj,nxt_traj,end_time_ep,cur_ep_row_id)))
        } else {
          return(c(0,0,0,0))
        }
      } else {
        cur_traj<-get_attribute(env,"nxt_traj")
        nxt_traj<-traj_nxt[[cur_traj]]
        cur_ep_row_id<-get_attribute(env,"cur_ep_row_id")
        return(as.numeric(c(cur_traj,nxt_traj,end_time_ep,cur_ep_row_id)))
      }
    }) %>% 
    branch(function(){
      start_time_cc_seg<-get_attribute(env,"start_time_cc_seg")
      dcr_time_cc_seg<-get_attribute(env,"dcr_time_cc_seg")
      now<-now(env)
      if (get_attribute(env,"cur_traj")==0) return(0) #probably died on ICU, could have gone straight home
      if (now>=start_time_cc_seg) return(1) #admit 
      if (now>=dcr_time_cc_seg) return(2) #discharge
      return(0)
    },continue=FALSE,CC_admission,CC_discharge) %>% ## join CC_admission or discharge. At the end will have to roll back to set_attribute or timout - depends whether we grab a ward at the end. Probably should grab a pr1 ward.
    set_attribute("pr",1) %>% 
    ## is our current bed good enough?
    branch(function() {
      cur_ward<-wards$Ward[get_attribute(env,"cur_ward_idx")]
      cur_traj<-get_attribute(env,"cur_traj")
      if (cur_traj==0) return(0) ##ie we're quitting so don't look for more
      if (get_attribute(env,"in_cc")==1) return(0) ##we're on critical care so no need for bed search
      pr<-get_attribute(env,"pr") ##nb we only look up to 3 as we already have a bed
      if (pr==4) return(0) ##ie just stay in same bed
      if (is.na(match(cur_ward,
                      switch(pr,
                             traj_pr1[[cur_traj]],
                             traj_pr2[[cur_traj]],
                             traj_pr3[[cur_traj]])            
      ))) 1 else 0
    },continue=TRUE,common_seize_new_ward) %>% 
    rollback(5,Inf,function() {now(env)<get_attribute(env,"end_time_spell")}) %>% ## to timeout
    simmer::select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  }) %>% 
    release_selected() %>% 
    release("bed")
  
  ## Sets patient info
  set_patient_attributes_common<-trajectory() %>% 
    set_attribute("cur_ep_row_id", function() {get_attribute(env,"ep1_row_id")}) %>%
    set_attribute("nxt_cc_row_id", function() {get_attribute(env,"cc1_row_id")}) %>%
    set_attribute("in_cc",0) %>% 
    set_attribute("CCTransfer",0) %>% 
    set_attribute("pr",1) %>% 
    set_prioritization(c(0,-1,-1)) %>% ##no queueing
    set_attribute(c("cur_traj","nxt_traj"), function() {
      pretraj<-combined_episodes[get_attribute(env,"cur_ep_row_id"),"pretraj"]
      traj<-as.numeric(combined_episodes[get_attribute(env,"cur_ep_row_id"),"traj"])
      if (is.na(pretraj))
      {
        return (as.numeric(c(traj,traj_nxt[[traj]])))
      } else {
        return (as.numeric(c(pretraj,traj)))
      }
    })
  
  set_patient_attributes_common_times<-trajectory() %>% 
    set_attribute("end_time_ep", function() { as.numeric(combined_episodes[get_attribute(env,"cur_ep_row_id"),"_EpisodeEnd_Offset"])+now(env) }) %>%
    set_attribute("start_time_cc_seg",function() { 
      if (get_attribute(env,"cc1_row_id")!=0)
        as.numeric(critcare_segments[get_attribute(env,"cc1_row_id"),"_SegmentStart_Offset"])+now(env)
      else
        Inf 
    }) %>% ##ie start of cc seg will be after end of spell if there's no cc seg
    set_attribute("dcr_time_cc_seg",function() { 
      if (get_attribute(env,"cc1_row_id")!=0)
        as.numeric(critcare_segments[get_attribute(env,"cc1_row_id"),"_SegmentDischReady_Offset"])+now(env)
      else
        Inf 
    }) %>% ##ie end of cc seg will be after end of spell if there's no cc seg
    set_attribute("start_time", function() {now(env)})
  
  emergency_delay_and_reset<-trajectory() %>% 
    set_global("Hospital_Full_4hrs",1,mod="+") %>%
    set_attribute("pr",1) %>% 
    rollback(4) 
  
  emergency_patient_rejected<-trajectory() %>% 
    branch(function() {
      if (get_attribute(env,"pr")<4) 0 else 1
    },continue=FALSE,emergency_delay_and_reset) %>% 
    set_attribute("pr",1,mod="+") %>% 
    rollback(3) 
  
  emergency_CC_patient_delayed<-trajectory() %>% 
    set_global(function() {
      return(paste0(get_selected(env),"_Admissions_12H_Delayed"))
    },1,mod="+") %>%
    rollback(2) 
  
  emergency_CC_patient<- trajectory() %>%
    renege_in(12*3600,out=emergency_CC_patient_delayed) %>% ##queue for an hour at each priority
    select(function() { if (critcare_segments[get_attribute(env,"cc1_row_id"),"_RealCritCare"]) "ICU" else c("21CC","ICU") },"first-available") %>% 
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10+5,-1,-1) }) %>% ## allow queueing
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    set_attribute("end_time_spell", function() { as.numeric(emergency_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(set_patient_attributes_common_times) %>% 
    join(set_CC_attributes) %>% 
    join(common_patient)
  
  emergency_patient<- trajectory() %>%
   handle_unfinished(catch_unfinished) %>% 
    join(set_patient_attributes_common) %>% 
    branch(function() {
      get_attribute(env,"cc_start")
    },continue=FALSE,emergency_CC_patient) %>% 
    renege_in(3600,out=emergency_patient_rejected) %>% ##queue for an hour at each priority
    select(function() {
      pr<-get_attribute(env,"pr")
      switch(pr,
             traj_pr1[[get_attribute(env,"cur_traj")]],
             traj_pr2[[get_attribute(env,"cur_traj")]],
             traj_pr3[[get_attribute(env,"cur_traj")]],
             wards$Ward) ##changed to a string vector
    },"shortest-queue-available") %>% 
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10+5,-1,-1) }) %>% ## allow queueing, default prio of +5 for emergency so ahead of electives
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    set_attribute("cur_ward_idx", function() { match(get_selected(env),wards$Ward)}) %>% 
    set_attribute("end_time_spell", function() { as.numeric(emergency_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(set_patient_attributes_common_times) %>% 
    join(common_patient)
  
  elective_patient_come_back_next_week<-trajectory() %>%   
    set_global("Elective_OTD_Cancellations",1,mod="+") %>% 
    timeout(3600*24*7-14*3600-1) %>% ## come back slightly earlier next time, remove 14hrs as that's how long we queued for
    rollback(3) #to renege_in in elective_patient
  
  elective_CC_patient_come_back_next_week<-trajectory() %>%   
    set_global("Elective_OTD_Cancellations",1,mod="+") %>% 
    set_global(function() {
      return(paste0(get_selected(env),"_Elective_OTD_Cancellations"))
    },1,mod="+") %>%
    timeout(3600*24*7-14*3600-1) %>% ## come back slightly earlier next time, remove 14hrs as that's how long we queued for
    rollback(4) #to renege_in in elective_CC_patient
  
  elective_CC_patient<- trajectory() %>%
    renege_in(14*3600,out=elective_CC_patient_come_back_next_week) %>% ##allow a decent length of time as a patient could use discharge lounge etc
    select(function() { if (critcare_segments[get_attribute(env,"cc1_row_id"),"_RealCritCare"]) "ICU" else c("21CC","ICU") },"first-available") %>% 
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10,-1,-1) }) %>% ## allow queueing
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    set_attribute("end_time_spell", function() { as.numeric(elective_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(set_patient_attributes_common_times) %>% 
    join(set_CC_attributes) %>% ##includes ward idx
    join(common_patient)
  
  ## Sets path for elective patient.
  ## First checks ward 21
  ## Allocated patient attributes
  ## Gets the start time for next ward
  ## Ability to cancel if patient waits for more than 14 hours (patient might use discharge lounge in this time)
  ## Looks up possible wards and takes shortest queue 
      ## first-available = pick the first one with a free bed, or first one with a free queue spot, or first one 
          ## with non-zero capacity (errors if all have zero capacity)
      ## shortest-queue-available = pick the one with the lowest queue+being_served - capacity. ie will choose a 
          ## later ward with more free space rather than an earlier with only one
  ## Records ward ID and discharge from ward time using duration + now()
  elective_patient<- trajectory() %>%
    handle_unfinished(catch_unfinished) %>% 
    join(set_patient_attributes_common) %>% 
    branch(function() {
      get_attribute(env,"cc_start")
    },continue=FALSE,elective_CC_patient) %>% 
    renege_in(14*3600,out=elective_patient_come_back_next_week) %>% 
    select(function() { traj_pr1[[get_attribute(env,"cur_traj")]] },"shortest-queue-available") %>% 
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10,-1,-1) }) %>% ## allow queueing
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% ## set attribute so we know what we selected
    set_attribute("cur_ward_idx", function() {
      match(get_selected(env),wards$Ward)
    }) %>% 
    set_attribute("end_time_spell", function() { as.numeric(elective_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(set_patient_attributes_common_times) %>% 
    join(common_patient)

  ## Add Patients to Enviroment
  ## Add escalation wards as a resource generator (delayed start time to 07:00 to allow fill up)
  ## add infinite beds - beds controlled by ward capacity rather than total number of beds.
  env %>% 
    add_dataframe("Emergency Patient",emergency_patient,emergency_table,col_time="gaps",time="interarrival",col_attributes=c("sp_row_id","ep1_row_id","cc1_row_id","cc_start"),mon=1) %>% ##mon=2 for monitoring individual arrival statistics (otherwise just globals)
    add_dataframe("Elective Patient",elective_patient,elective_table,col_time="gaps",time="interarrival",col_attributes=c("sp_row_id","ep1_row_id","cc1_row_id","cc_start"),mon=1) %>% 
    add_generator("Ward Controller",ward_control,at(as.numeric(elective_freq[1,]$dateTime)+7*3600)) %>%  ## start 7am on first day
    add_resource("bed",capacity=Inf,queue_size=0,queue_size_strict=TRUE)
  
  ## Add up all beds in all wards.  
  ## Adds wards to enviroment with set capacity, infinite queue size and a priority
  beds_open<-0  
  for (i in 1:nrow(wards)) {
    add_resource(env,wards[i,"Ward"],capacity=wards[i,"Beds"],queue_size=Inf,queue_priority=c(i*10,i*10+9))
    beds_open<-beds_open+wards[i,"Beds"]
  }
  
  env %>% add_global("Beds_Open",beds_open)
  print("* Starting Run *")
  env %>% 
    simmer::run(until=63072000	) %>%  ## 1632312575
    wrap()
  
}