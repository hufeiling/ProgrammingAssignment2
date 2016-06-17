corr<-function(dictionary,threshold=0){
  inputfile<-list.files(dictionary, full.names = TRUE)  
  data3<-vector()
  for(i in 1:332){
    singlefile<-read.csv(inputfile[i])
    completefile<-singlefile[complete.cases(singlefile),]
    if(length(completefile$Date)>threshold){
    newvector<-cor(completefile$sulfate,completefile$nitrate)
    data3<-append(data3,newvector)
    }
  }
  data3
}