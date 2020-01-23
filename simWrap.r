################################################################################
# Project Name: RSimmerAdaption
# Date Last Updated: 17/01/20
# Summary: RSimmer function - NOT ADAPTED YET
################################################################################

simmer_wrapper <- function(i) {
  
  emergency_table <- emergency_gen_table()
  elective_table <- elective_gen_table()
  print("*started*")
  print("* times generated *")
  
  emergency_table <- emergency_gen_patients(emergency_table)
  elective_table <- elective_gen_patients(elective_table)
  
  print(nrow(emergency_table))
  print(nrow(elective_table))
  
  ##not sure why spell_id needs treating differently below. Something to do with data types I think.
  ##I've explicitly checked this and it's correct and doesn't reuse the same spell_id over and over
  
  emergency_table$sp_row_id <- match(emergency_table$spell_id[[1]],emergency_spells$`_SpellID`)
  elective_table$sp_row_id <- match(elective_table$spell_id[[1]],elective_spells$`_SpellID`)
  
  elective_table$ep1_row_id <- elective_spells$ep1_row_id[elective_table$sp_row_id]
  emergency_table$ep1_row_id <- emergency_spells$ep1_row_id[emergency_table$sp_row_id]
  ##elective_table$ep1_row_id<-elective_spells[elective_table$sp_row_id,"ep1_row_id"]
  ##above has funny consequences for data type (like spell_id further above)
  
  elective_table$cc1_row_id<-elective_spells$cc1_row_id[elective_table$sp_row_id]
  emergency_table$cc1_row_id<-emergency_spells$cc1_row_id[emergency_table$sp_row_id]
  elective_table$cc_start<-elective_spells$cc_start[elective_table$sp_row_id]
  emergency_table$cc_start<-emergency_spells$cc_start[emergency_table$sp_row_id]
  
  print("* patients generated *")
  ## JP straigth copy downwards
  
  env<-simmer("hospital")
  
  ##nb patient must be in scope for the call to get_attribute
  
  catch_unfinished<-trajectory() %>% 
    log_("***UNFINISHED ARRIVAL - shouldn't see yet***") %>% 
    log_(function(){paste0("21:",get_queue_count(env,"21"),"/",get_queue_size(env,"21"))})
  # set_queue_size("21",0) %>% 
  # rollback(1)
  
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
  
  CC_admission_delayed<-trajectory() %>% 
    #log_("admission delayed 12h") %>% 
    set_global(function() {
      return(paste0(get_selected(env),"_Admissions_12H_Delayed"))
    },1,mod="+") %>%
    #set_prioritization(c(1,-1,-1),mod="+") %>% ##increase priority
    rollback(4) # to select in CC_admission
  
  
  CC_admission_seize<-trajectory() %>% 
    renege_in(12*3600,out=CC_admission_delayed, keep_seized=TRUE) %>% ##queue for 12hrs before marking delayed transfer
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10+5,-1,-1) }) %>% ## allow queueing
    #log_(function() {paste0("cc discharge:",get_selected(env)," ",get_queue_count_selected(env),"/",get_queue_size_selected(env))}) %>% 
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    release_selected(id=1) 
  
  CC_admission<-trajectory() %>% 
    #log_("cc admission") %>% 
    ## may already have a bed if we've been delayed so check
    #log_(function(){paste0("nxt:",get_attribute(env,"nxt_cc_row_id")," cc1",get_attribute(env,"cc1_row_id"))}) %>% 
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
    #set_prioritization(c(1,-1,-1),mod="+") %>% ##increase priority
    rollback(2) # to renege_in in CC_transfer_seize
  
  
  CC_transfer_seize<-trajectory() %>% 
    renege_in(12*3600,out=CC_transfer_delayed, keep_seized=TRUE) %>% ##queue for 12hrs before marking delayed transfer
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10+6,-1,-1) }) %>% ## allow queueing, higher prio as we'll unblock a cc bed
    #log_(function() {paste0("cc discharge:",get_selected(env)," ",get_queue_count_selected(env),"/",get_queue_size_selected(env))}) %>% 
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    release_selected(id=1) 
  
  CC_transfer<-trajectory() %>% 
    ##we're in a CC bed already
    #log_("cc transfer") %>% 
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
    #log_(function() {paste0("cc discharge:",get_selected(env)," ",get_queue_count_selected(env),"/",get_queue_size_selected(env))}) %>% 
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    #log_(function() {paste0("cc discharged: From ",get_selected(env,id=1)," to ",get_selected(env))}) %>% 
    release_selected(id=1) %>% 
    set_attribute("cur_ward_idx", function() {   match(get_selected(env),wards$Ward)  }) %>% 
    set_attribute("in_cc",0) %>% 
    set_attribute("dcr_time_cc_seg",Inf)
  #set_attribute("dcr_time_cc_seg",function() {get_attribute(env,"end_time_spell")+1})
  ##rolls back into end of CC_discharge
  
  CC_discharge<-trajectory() %>% 
    ##either to a ward, or they're being transferred from one CC area to another
    ##if CCTransfer == 1 then do a transfer (can we use CC_admission?)
    ##if CCTransfer == 0 then transfer to a pr1 ward and queue if needed
    branch(function(){
      get_attribute(env,"CCTransfer")+1
    },continue=TRUE,CC_discharge_to_ward,CC_transfer) %>% 
    ##and update dcr_time_cc_seg to end of spell
    #log_("cc discharge") %>% 
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
    #log_(function(){paste0("RELEASE:",get_attribute(env,"cur_ward_idx")," ",wards$Ward[get_attribute(env,"cur_ward_idx")])}) %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  }) %>% 
    release_selected() %>% 
    set_attribute("cur_ward_idx", function() {   match(get_selected(env,id=1),wards$Ward)  })
  ##falls back into common_patient just before rollback
  
  
  common_patient<-trajectory() %>% 
    ## when we arrive here, we have a bed already (from elective or emergency)
    seize("bed") %>% 
    #log_(function() {toString(min(traj_dur[[get_attribute(env,"cur_traj")]],get_attribute(env,"end_time")-now(env)))}) %>% 
    timeout(function() { 
      max(0,
          min(traj_dur[[get_attribute(env,"cur_traj")]],
              get_attribute(env,"end_time_ep")-now(env),
              get_attribute(env,"start_time_cc_seg")-now(env), #will have moved on since admission
              get_attribute(env,"dcr_time_cc_seg")-now(env)
          )
      )
    }) %>% ## important that CC dcr is never before start_time, or we'll get stuck
    ## nb if we've been stuck on ICU for a bit, the above could turn negative (though we will have zero delay in this case). possible solution see below
    ## nb could be here simply because we've hit trajectory changeover (traj_dur) but actually we're in CC (in_cc == 1) 
    set_attribute(c("cur_traj","nxt_traj","end_time_ep","cur_ep_row_id"),function() {
      now<-now(env)
      start_time_spell<-get_attribute(env,"start_time")
      end_time_ep<-get_attribute(env,"end_time_ep")
      end_time_spell<-get_attribute(env,"end_time_spell")
      ## TODO - should we put a while here so we always finish with end_time_ep > now(). nb that we need to allow trajectories to move on still
      if (now>=end_time_ep) {
        if (now<end_time_spell) {
          ##next episode
          #cur_traj - send one from new episode (nb can ignore pretraj)
          #nxt_traj - send traj_nxt[[ new cur_traj ]]
          #end_time_ep - send _EpisodeEnd_Offset from new episode
          #cur_ep_row_id - send new row id (from epN_row_id)
          cur_ep_row_id<-get_attribute(env,"cur_ep_row_id")
          cur_ep_row_id<-as.numeric(combined_episodes[[cur_ep_row_id,"epN_row_id"]])
          if (!is.na(cur_ep_row_id)){
            cur_traj<-as.numeric(combined_episodes[[cur_ep_row_id,"traj"]])
            nxt_traj<-traj_nxt[[cur_traj]]
            end_time_ep<-as.numeric(combined_episodes[[cur_ep_row_id,"_EpisodeEnd_Offset"]])+start_time_spell
          } else {
            ##we've run out of episodes but haven't finished yet - this shouldn't really happen (but means the discharge datetime is after the end of the last episode)
            ##print("NA")
            cur_traj<-get_attribute(env,"cur_traj")
            nxt_traj<-traj_nxt[[cur_traj]]
            #print(paste("c",cur_traj,nxt_traj,get_attribute(env,"end_time_ep"),get_attribute(env,"end_time_spell"),cur_ep_row_id,sep=":"))
            end_time_ep<-end_time_spell
          }
          #print(paste("a",cur_traj,nxt_traj,end_time_ep,cur_ep_row_id,sep=":"))
          #print(paste0("<end ",cur_ep_row_id))
          return(as.numeric(c(cur_traj,nxt_traj,end_time_ep,cur_ep_row_id)))
        } else {
          ## we're at our spell's end - no need for more data as we're about to finish
          #print("fin")
          return(c(0,0,0,0))
        }
      } else {
        #next trajectory, same episode
        #cur_traj - send nxt_traj
        #nxt_traj - send traj_nxt[[ new cur_traj ]]
        #end_time_ep - stay the same
        #cur_ep_row_id - stay the same
        cur_traj<-get_attribute(env,"nxt_traj")
        nxt_traj<-traj_nxt[[cur_traj]]
        #end_time_ep<-get_attribute(env,"end_time_ep") - already set
        cur_ep_row_id<-get_attribute(env,"cur_ep_row_id")
        #print(paste("b",cur_traj,nxt_traj,end_time_ep,cur_ep_row_id,sep=":"))
        #print(paste0("nxt ",cur_ep_row_id," ",get_attribute(env,"start_time_cc_seg")," ",get_attribute(env,"dcr_time_cc_seg")," ",now))
        return(as.numeric(c(cur_traj,nxt_traj,end_time_ep,cur_ep_row_id)))
      }
    }) %>% 
    branch(function(){
      start_time_cc_seg<-get_attribute(env,"start_time_cc_seg")
      dcr_time_cc_seg<-get_attribute(env,"dcr_time_cc_seg")
      now<-now(env)
      #in_cc<-get_attribute(env,"in_cc")
      #print(paste0("ccbr ",start_time_cc_seg," ",dcr_time_cc_seg," ",now))
      ## put multiple branches in here
      #if (now>start_time_cc_seg && in_cc==1) {
      #  print(paste0("late ",start_time_cc_seg,":",dcr_time_cc_seg,":",now)) ##something delayed us beyond the start
      #}
      
      #if (now>dcr_time_cc_seg && in_cc==0) print(paste0("what? ",get_attribute(env,"end_time_spell"),":",dcr_time_cc_seg,":",now)) ##delayed so far we missed the end?
      if (get_attribute(env,"cur_traj")==0) return(0) #probably died on ICU, could have gone straight home
      if (now>=start_time_cc_seg) return(1) #admit 
      if (now>=dcr_time_cc_seg) return(2) #discharge
      # 
      # if (now>=start_time_cc_seg && in_cc==0) return(1) #admit CC from ward
      # if (now>=start_time_cc_seg && in_cc==1) return(1) #we're on CC but need to join? Likely we missed a discharge
      # if (now>=dcr_time_cc_seg && in_cc==1) return(2) #discharge from CC (to ward or more CC)
      # if (now>=dcr_time_cc_seg && in_cc==0) return(2) #we're not on CC but need to leave?? At least this way we'll go find a ward bed
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
    ##nb rollback includes log_ lines (so 7 if log line is included) - need to roll back to set pr back to 1
    simmer::select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  }) %>% 
    ##log_("leaving") %>% 
    release_selected() %>% 
    release("bed")
  #log_("Out of bed")
  
  
  
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
        #print(paste("c",traj,traj_nxt[[traj]],sep=":"))
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
        Inf #get_attribute(env,"end_time_spell")+1
    }) %>% ##ie start of cc seg will be after end of spell if there's no cc seg
    set_attribute("dcr_time_cc_seg",function() { 
      if (get_attribute(env,"cc1_row_id")!=0)
        as.numeric(critcare_segments[get_attribute(env,"cc1_row_id"),"_SegmentDischReady_Offset"])+now(env)
      else
        Inf #get_attribute(env,"end_time_spell")+1
    }) %>% ##ie end of cc seg will be after end of spell if there's no cc seg
    set_attribute("start_time", function() {now(env)})
  
  
  
  emergency_delay_and_reset<-trajectory() %>% 
    #log_("no beds for emergency patient") %>% 
    ##no need for timeout as we queue for our bed, an hour at a time
    set_global("Hospital_Full_4hrs",1,mod="+") %>%
    set_attribute("pr",1) %>% 
    rollback(4) #to renege_in in emergency_patient
  
  
  
  emergency_patient_rejected<-trajectory() %>% 
    #log_("rejected") %>%
    branch(function() {
      if (get_attribute(env,"pr")<4) 0 else 1
    },continue=FALSE,emergency_delay_and_reset) %>% 
    set_attribute("pr",1,mod="+") %>% 
    rollback(3) #to renege_in in emergency_patient
  
  
  emergency_CC_patient_delayed<-trajectory() %>% 
    #log_("no beds for emergency patient") %>% 
    set_global(function() {
      return(paste0(get_selected(env),"_Admissions_12H_Delayed"))
    },1,mod="+") %>%
    rollback(2) # to renege_in in emergency_CC_patient
  
  
  
  
  emergency_CC_patient<- trajectory() %>%
    #log_("cc direct admission") %>% 
    renege_in(12*3600,out=emergency_CC_patient_delayed) %>% ##queue for an hour at each priority
    select(function() { if (critcare_segments[get_attribute(env,"cc1_row_id"),"_RealCritCare"]) "ICU" else c("21CC","ICU") },"first-available") %>% 
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10+5,-1,-1) }) %>% ## allow queueing
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    
    
    
    set_attribute("end_time_spell", function() { as.numeric(emergency_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(set_patient_attributes_common_times) %>% 
    join(set_CC_attributes) %>% 
    #log_("admitted successfully") %>% 
    join(common_patient)
  
  
  
  
  
  
  emergency_patient<- trajectory() %>%
    #log_("arrived") %>% 
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
    ## first-available = pick the first one with a free bed, or first one with a free queue spot, or first one with non-zero capacity (errors if all have zero capacity)
    ## shortest-queue-available = picks the one with the most space
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10+5,-1,-1) }) %>% ## allow queueing, default prio of +5 for emergency so ahead of electives
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    ## set attribute so we know what we selected
    set_attribute("cur_ward_idx", function() {
      match(get_selected(env),wards$Ward)
    }) %>% 
    #log_(function(){paste0("Ward:",get_selected(env)," ",get_seized_selected(env))}) %>% 
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
    #log_("cc direct admission") %>% 
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
  
  
  
  
  elective_patient<- trajectory() %>%
    #log_("arrived") %>% 
    handle_unfinished(catch_unfinished) %>% 
    join(set_patient_attributes_common) %>% 
    # log_(function() {paste0(elective_spells[get_attribute(env,"sp_row_id"),"duration"]," ",combined_episodes[get_attribute(env,"cur_ep_row_id"),"_EpisodeEnd_Offset"])}) %>% 
    branch(function() {
      get_attribute(env,"cc_start")
    },continue=FALSE,elective_CC_patient) %>% 
    renege_in(14*3600,out=elective_patient_come_back_next_week) %>% ##allow a decent length of time as a patient could use discharge lounge etc
    select(function() { traj_pr1[[get_attribute(env,"cur_traj")]] },"shortest-queue-available") %>% 
    ## first-available = pick the first one with a free bed, or first one with a free queue spot, or first one with non-zero capacity (errors if all have zero capacity)
    ## shortest-queue-available = pick the one with the lowest queue+being_served - capacity. ie will choose a later ward with more free space rather than an earlier with only one
    set_prioritization(function() {  c(match(get_selected(env),wards$Ward)*10,-1,-1) }) %>% ## allow queueing
    seize_selected() %>%
    set_prioritization(c(0,-1,-1)) %>% ##cancel queueing
    renege_abort() %>% 
    ## set attribute so we know what we selected
    set_attribute("cur_ward_idx", function() {
      match(get_selected(env),wards$Ward)
    }) %>% 
    #log_(function(){paste0("Ward:",get_selected(env)," ",get_seized_selected(env))}) %>% 
    set_attribute("end_time_spell", function() { as.numeric(elective_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(set_patient_attributes_common_times) %>% 
    join(common_patient)
  
  
  
  source("WardController.R",local=TRUE) ## local=TRUE keeps it within the wrapper
  
  
  
  env %>% 
    add_dataframe("Emergency Patient",emergency_patient,emergency_table,col_time="gaps",time="interarrival",col_attributes=c("sp_row_id","ep1_row_id","cc1_row_id","cc_start"),mon=1) %>% ##mon=2 for monitoring individual arrival statistics (otherwise just globals)
    add_dataframe("Elective Patient",elective_patient,elective_table,col_time="gaps",time="interarrival",col_attributes=c("sp_row_id","ep1_row_id","cc1_row_id","cc_start"),mon=1) %>% 
    add_generator("Ward Controller",ward_control,at(as.numeric(elective_freq[1,]$dateTime)+7*3600)) %>%  ## start 7am on first day
    add_resource("bed",capacity=Inf,queue_size=0,queue_size_strict=TRUE)
  
  beds_open<-0  
  
  for (i in 1:nrow(wards)) {
    add_resource(env,wards[i,"Ward"],capacity=wards[i,"Beds"],queue_size=Inf,queue_priority=c(i*10,i*10+9))
    beds_open<-beds_open+wards[i,"Beds"]
    ## no longer using queue_size_strict, instead we use queue_priority to ensure that the arrival doesn't enter the queue unless we specifically want it to happen
    ## queue priority for each ward is i*10 to i*10+9
    #print(paste0(wards[i,"Ward"],":",wards[i,"Beds"]))
  }
  
  env %>% add_global("Beds_Open",beds_open)
  
  
  # print(plot(elective_patient))
  
  print("* Starting Run *")
  
  env %>% 
    simmer::run(until=1632312575	) %>% 
    wrap()
  
}