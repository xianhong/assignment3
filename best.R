
best <- function(state, outcome) {
  ## Read outcome data
  
  if (!(state %in% state.abb)) stop("Invalid state")
  if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("Invalid outcome")
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ## 'out_col' records the column number corresponding to the mortality rate of the respective 'outcome'
  if (outcome=="heart attack") out_col<-11
  else if (outcome=="heart failure") out_col<-17
  else out_col<-23
  all_outcomes<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## make a subset of all outcomes in the given 'state'
  outcomes_in_state<-all_outcomes[all_outcomes$State==state,]
  
    hospital_name<-outcomes_in_state[[2]]
    suppressWarnings(mortality_rate<-as.numeric(outcomes_in_state[[out_col]]))
    ## warning messages
    df<- data.frame(hospital_name,mortality_rate)
    df<-df[order(df$mortality_rate,df$hospital_name,na.last=NA),]
    as.character(df[1,1])
  }