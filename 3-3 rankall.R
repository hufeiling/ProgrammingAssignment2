rankall<-function(outcome,num){
  outcomedata<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  if (!outcome %in% c('heart attack','heart failure','pneumonia') )
    stop('invalid outcome')
  
  colnumber<-NULL
  if(outcome=="heart attack")
    colnumber<-11
  else if(outcome=="heart failure")
    colnumber<-17
  else colnumber<-23
  
  state<-sort(unique(outcomedata[,7]))
  statenum<-length(state)
  outcomedatarank<-data.frame()
  
  for(i in 1:statenum){
    
    outcomedata1<-outcomedata[which(outcomedata$State==state[i]),c
                              (2,7,colnumber)]
    outcomedata2<-outcomedata1[is.na(as.numeric(outcomedata1[,3]))==FALSE,]
    outcomedata3<-outcomedata2[order(as.numeric(outcomedata2[,3]),outcomedata2[,1]),]
    
    rownum<-nrow(outcomedata3)
    rankdata<-data.frame(Rank<-1:rownum)
    outcomedata4<-cbind(outcomedata3,rankdata)
    
    outcomedatarank<-rbind(outcomedatarank,outcomedata4)
  }
  
  allstaterank<-data.frame()
  
  for(i in 1:statenum){
    
    outcomedata5<-outcomedatarank[which(outcomedatarank$State==state[i]),]
    rownum2<-nrow(outcomedata5)
    
    if(num=="best"&&rownum2>0){
      outcomedata6<-outcomedata5[1,c(1,2)]
    }
    else if(num=="worst"&&rownum2>0){
      outcomedata6<-outcomedata5[which(outcomedata5$Rank==rownum2),c(1,2)]
    }
    else if(num<rownum2){
      outcomedata6<-outcomedata5[which(outcomedata5$Rank==num),c(1,2)]
    }
    else{
      outcomedata6<-data.frame(Hospital<-NA, State<-state[i])
    }
    
    allstaterank<-rbind(allstaterank,setNames(outcomedata6,names(allstaterank)))
  }
  allstaterank
}
