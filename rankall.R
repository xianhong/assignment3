rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("Invalid outcome")
  
  ascend<-TRUE
  if (num=="best") rank_pos<-1
  else if (num=="worst") {
    rank_pos<-1
    ascend<-FALSE
  }  else rank_pos<-as.integer(num)
  
  all_outcomes<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  outcomes_by_state<-split(all_outcomes,all_outcomes$State)
  results<-lapply(outcomes_by_state, rankhospitalbystate,outcome,rank_pos,ascend)
  len<-length(results)
  print(len)
  n<-1
  a<-data.frame()
  while (n <=len) {
    a<-rbind(a,as.data.frame(results[[n]]))
    n<-n+1
  }
  a
  }
rankhospitalbystate <- function(records_by_state,outcome, rank_pos,ascend=TRUE) {
 
 
  if (outcome=="heart attack") out_col<-11
  else if (outcome=="heart failure") out_col<-17
  else out_col<-23
  records_by_state<-as.data.frame(records_by_state)
  State<-records_by_state[1,7]
  
  outcomes_in_state<-records_by_state
  
  hospital_name<-outcomes_in_state[[2]]
  suppressWarnings(mortality_rate<-as.numeric(outcomes_in_state[[out_col]]))
  df<- data.frame(hospital_name,mortality_rate)
  
  if (ascend)  df<-df[order(df$mortality_rate,df$hospital_name,na.last=NA),]
  else df<-df[order(-df$mortality_rate,df$hospital_name,na.last=NA),]
  
  data.frame(hospital=as.character(df[rank_pos,1]),state=State)
  
}
