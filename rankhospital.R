
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if (!(state %in% state.abb)) stop("Invalid state")
  if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("Invalid outcome")
  
  if (num=="best") rank_pos<-1
  else if (num=="worst") rank_pos<-NA
  else rank_pos<-as.integer(num)
  
  if (outcome=="heart attack") out_col<-11
  else if (outcome=="heart failure") out_col<-17
  else out_col<-23
  
  all_outcomes<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  outcomes_in_state<-all_outcomes[all_outcomes$State==state,]
  
  hospital_name<-outcomes_in_state[[2]]
  suppressWarnings(mortality_rate<-as.numeric(outcomes_in_state[[out_col]]))
  df<- data.frame(hospital_name,mortality_rate)
  

  df<-df[order(df$mortality_rate,df$hospital_name,na.last=NA),]
  if (is.na(rank_pos)) rank_pos<-nrow(df)
  as.character(df[rank_pos,1])
  
}
