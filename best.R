best<-function(state,outcome){
  outcomedata<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  if(!state %in% unique(outcomedata[,7])) 
    stop('invalid state')
  if (!outcome %in% c('heart attack','heart failure','pneumonia') )
    stop('invalid outcome')
  colnumber<-NULL
  if(outcome=="heart attack")
    colnumber<-11
  else if(outcome=="heart failure")
    colnumber<-17
  else colnumber<-23
  outcomedata1<-outcomedata[which(outcomedata$State==state),c(2,colnumber)]
  minvalue<-min(as.numeric(outcomedata1[,2]),na.rm = TRUE)
  outcomedata2<-outcomedata1[as.numeric(outcomedata1[,2])==minvalue,]
  outcomedata3<-outcomedata2[order(outcomedata2[,1]),]
  outcomedata3[1,1]
}