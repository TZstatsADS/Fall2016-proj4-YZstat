library(rhdf5)
library(NLP)
library(tm)
library(lda)
library(LDAvis)


##### Read Data
# 
# mydata<-list.files(path="~/Desktop/Project4_data/data", recursive = TRUE)
# setwd("~/Desktop/Project4_data/data")
# 
# 
# H5close()
# 
# for (i in 1:length(mydata)){
# 
#   X_train[[i]]<-h5read(mydata[i],"/analysis")
# }


mydata<-list.files(path="~/Desktop/TestSongFile100", recursive = TRUE)
H5close()
setwd("~/Desktop/TestSongFile100")

X_test<-list()
for (i in 1:length(mydata)){

  X_test[[i]]<-h5read(mydata[i],"/analysis")
}



create_variable<-function(name,dataset,rank){
  
  name<-dataset[[1]][rank]
  nlength<-length(unlist(dataset[[1]][rank]))
 
  for (i in 2:100){
    name<-rbind(name,dataset[[i]][rank])
    nlength<-rbind(nlength,length(unlist(dataset[[i]][rank])))
  }
  
  final_matrix<-cbind(name,nlength)
  row.names(final_matrix)<-NULL
  colnames(final_matrix)[2]<-c("length")
  
  n<-round(mean(unlist(final_matrix[,2])))
  K<-matrix(data=0, nrow = 100, ncol = n)
  
  for (i in 1:100){
    if (final_matrix[,2][[i]] < n){
      haha<-rep(final_matrix[[i]],n)
      K[i,]<-haha[1:n]
    } else {
      K[i,]<-final_matrix[[i]][1:n]
    }
  }
  K[is.na(K)]<-0
  return(K)
}


variable.feature<-function(variable,num.center){
  
  lala<-kmeans(variable,centers=num.center,iter.max=2,nstart=10)
  
  return(as.factor(lala$cluster))
  
}


create.feature<-function(dataset,num.center){
  
  n<-15
  m<-100
  
  X_bars_confidence <- create_variable(bars_confidence,dataset,1)
  X_bars_start <- create_variable(bars_start,dataset,2)
  X_beats_confidence <- create_variable(beats_confidence,dataset,3)
  X_beats_start <- create_variable(beats_start,dataset,4)
  X_sections_confidence <- create_variable(sections_confidence,dataset,5)
  X_sections_start <- create_variable(sections_start,dataset,6)
  X_segments_confidence <- create_variable(segments_confidence,dataset,7)
  X_segments_loudness_max <- create_variable(segments_loudness_max,dataset,8)
  X_segments_loudness_max_time <- create_variable(segments_loudness_max_time,dataset,9)
  X_segments_loudness_start <- create_variable(segments_loudness_start,dataset,10)
  X_segments_pitches <- create_variable(segments_pitches,dataset,11)
  X_segments_start <- create_variable(segments_start,dataset,12)
  X_segments_timbre <- create_variable(segments_timbre,dataset,13)
  #X_songs <- create_variable(songs,dataset,14)
  X_tatums_confidence <- create_variable(tatums_confidence,dataset,14)
  X_tatums_start <- create_variable(tatums_start,dataset,15)
  
  feature.music<-matrix(NA,nrow = 100, ncol = 15)
  
  feature.music[,1]<-variable.feature(X_bars_confidence,num.center)
  feature.music[,2]<-variable.feature(X_bars_start,num.center)
  feature.music[,3]<-variable.feature(X_beats_confidence,num.center)
  feature.music[,4]<-variable.feature(X_beats_start,num.center)
  feature.music[,5]<-variable.feature(X_sections_confidence,num.center)
  feature.music[,6]<-variable.feature(X_sections_start,num.center)
  feature.music[,7]<-variable.feature(X_segments_confidence,num.center)
  feature.music[,8]<-variable.feature(X_segments_loudness_max,num.center)
  feature.music[,9]<-variable.feature(X_segments_loudness_max_time,num.center)
  feature.music[,10]<-variable.feature(X_segments_loudness_start,num.center)
  feature.music[,11]<-variable.feature(X_segments_pitches,num.center)
  feature.music[,12]<-variable.feature(X_segments_start,num.center)
  feature.music[,13]<-variable.feature(X_segments_timbre,num.center)
  #feature.music[,14]<-variable.feature(X_song,num.center)
  feature.music[,14]<-variable.feature(X_tatums_confidence,num.center)
  feature.music[,15]<-variable.feature(X_tatums_start,num.center)
  
  return(feature.music)
  
}


test_feature<-create.feature(dataset=X_test, num.center=20)


  
