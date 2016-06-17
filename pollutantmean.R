pollutantmean<-function(dictionary, pollutant,id=1:332){
  inputfile<-list.files(dictionary, full.names = TRUE)
  data1<-data.frame()
  for(i in id){
    data1<-rbind(data1,read.csv(inputfile[i]))
  }
    mean(data1[,pollutant],na.rm=TRUE)
}