complete<-function(dictionary,id=1:332){
  inputfile<-list.files(dictionary, full.names = TRUE)  
  data2<-data.frame()
  for(i in id){
    newframe<-data.frame(id=i,nobs=sum(complete.cases(read.csv(inputfile[i]))))
    data2<-rbind(data2,newframe)
  }
  data2
}