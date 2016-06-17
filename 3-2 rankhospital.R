rankhospital<-function(state,outcome,num){
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
  outcomedata2<-outcomedata1[is.na(as.numeric(outcomedata1[,2]))==FALSE,]
  outcomedata3<-outcomedata2[order(as.numeric(outcomedata2[,2]),outcomedata2[,1]),]

  rownum<-nrow(outcomedata3)
  rankdata<-data.frame(Rank<-1:rownum)
  outcomedata4<-cbind(outcomedata3,rankdata)
  
  if(num=="best")
    outcomedata4[1,1]
  else if(num=="worst")
    outcomedata4[rownum,1]
  else if((as.numeric(num)>0&&as.numeric(num)<rownum))
    outcomedata4[as.numeric(num),1]
  else
    return(NA)
}
