

#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)

mydata<-list.files(path="~/Desktop/Project4_data/data", recursive = TRUE)
setwd("~/Desktop/Project4_data/data")
H5close()

X_train<-list()
##### X_train[[1]] is the rows --> means the observations
##### X_train[[1]][[1]][1] is the first variable of first observation
for (i in 1:length(mydata)){
  
  X_train[[i]]<-h5read(mydata[i],"/analysis")
}

####  Create varibles 
# X_bars_confidence<-X_train[[1]][1]
# 
# 
# for (i in 2:length(mydata)){
#     
#     X_bars_confidence<-rbind(X_bars_confidence,X_train[[i]][1])
#     X_bars_confidence_length<-rbind(X_bars_confidence_length,
#                                     length(unlist(X_train[[i]][1])))
# }
# 
# outliers1<-match(0, X_bars_confidence_length)
# 

create_variable<-function(name,dataset,rank){
  
  name<-dataset[[1]][rank]
  nlength<-length(unlist(dataset[[1]][rank]))
  
  for (i in 2:2350){
    name<-rbind(name,dataset[[i]][rank])
    nlength<-rbind(nlength,length(unlist(dataset[[i]][rank])))
  }

  final_matrix<-cbind(name,nlength)
  row.names(final_matrix)<-NULL
  colnames(final_matrix)[2]<-c("length")
  
  return(final_matrix)
}

##### Construct the variables 

names(X_train[[2]])

X_bars_confidence <- create_variable(bars_confidence,X_train,1)
X_bars_start <- create_variable(bars_start,X_train,2)
X_beats_confidence <- create_variable(beats_confidence,X_train,3)
X_beats_start <- create_variable(beats_start,X_train,4)
X_sections_confidence <- create_variable(sections_confidence,X_train,5)
X_sections_start <- create_variable(sections_start,X_train,6)
X_segments_confidence <- create_variable(segments_confidence,X_train,7)
X_segments_loudness_max <- create_variable(segments_loudness_max,X_train,8)
X_segments_loudness_max_time <- create_variable(segments_loudness_max_time,X_train,9)
X_segments_loudness_start <- create_variable(segments_loudness_start,X_train,10)
X_segments_pitches <- create_variable(segments_pitches,X_train,11)
X_segments_start <- create_variable(segments_start,X_train,12)
X_segments_timbre <- create_variable(segments_timbre,X_train,13)
#X_songs <- create_variable(songs,X_train,14)
X_tatums_confidence <- create_variable(tatums_confidence,X_train,15)
X_tatums_start <- create_variable(tatums_start,X_train,16)

mean(unlist(X_bars_confidence[,2]))
hist(unlist(X_bars_confidence[,2]))

outlier_find<-function(dataset,range){
  for (i in 1:2350){
    if (unlist(dataset[,2])[i]%in% c(0:10))
      print(i)
    else (i=i+1)
  }
}
outlier_find(X_bars_confidence)
plot(1,2)
  
for (i in 1:2350){
  if (unlist(X_bars_confidence[,2])[i]%in% c(0:10))
    print(i)
  else (i=i+1)
}


class(cbind(
  X_bars_confidence[,1] ,
  X_bars_start [,1]
))


X[,1]<-create_variable(name[1],X_train,1)[,1]

# dd<-create_variable(X_bars,X_train,2)
# head(dd)
# colnames(dd)[2]<-"hi"
####  Variable 1

  




# 
# mmm<-rbind(as.numeric(hh[[1]])[1:50],as.numeric(hh[[2]])[1:50])
# 
# mm<-rbind(as.vector(as.numeric(hh[[1]])[1:50]),
#           as.vector(as.numeric(hh[[3]])[1:50]),
#           as.vector(as.numeric(hh[[2]])[1:50])
#           )
# 
# class(mm)
# hh_cluster<-kmeans(mm,2,nstart = 20)

