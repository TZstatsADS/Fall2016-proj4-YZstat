

#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)
library(NLP)
library(tm)
library(lda)
library(LDAvis)

mydata<-list.files(path="~/Desktop/Project4_data/data", recursive = TRUE)
setwd("~/Desktop/Project4_data/data")
H5close()

X_train<-list()
##### X_train[[1]] is the rows --> means the observations
##### X_train[[1]][[1]][1] is the first variable of first observation
for (i in 1:length(mydata)){
  
  X_train[[i]]<-h5read(mydata[i],"/analysis")
}

#### Create variables function



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
  
  n<-round(mean(unlist(final_matrix[,2])))
  K<-matrix(data=0, nrow = 2350, ncol = n)
  
  for (i in 1:2350){
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


##### Construct the variables 

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
X_songs <- create_variable(songs,X_train,14)
X_tatums_confidence <- create_variable(tatums_confidence,X_train,15)
X_tatums_start <- create_variable(tatums_start,X_train,16)


#####  K Means for each variables



variable.feature<-function(variable,num.center){
  
  lala<-kmeans(variable,centers=num.center,iter.max=2,nstart=10)
  
  return(as.factor(lala$cluster))
 
}


create.feature<-function(dataset,num.center){
  
  n<-ncol(dataset)
  m<-nrow(dataset)
  
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
  X_songs <- create_variable(songs,dataset,14)
  X_tatums_confidence <- create_variable(tatums_confidence,dataset,15)
  X_tatums_start <- create_variable(tatums_start,dataset,16)
    
  feature.music<-matrix(NA, nrow = m, ncol = n)
  
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
  feature.music[,14]<-variable.feature(X_song,num.center)
  feature.music[,15]<-variable.feature(X_tatums_confidence,num.center)
  feature.music[,16]<-variable.feature(X_tatums_start,num.center)
  
  
}
  
  
  
# feature.music<-matrix(NA, nrow = 2350, ncol = 16)
# 
# 
# 
# feature.music[,1]<-variable.feature(X_bars_confidence,20)
# feature.music[,2]<-variable.feature(X_bars_start,20)
# feature.music[,3]<-variable.feature(X_beats_confidence,20)
# feature.music[,4]<-variable.feature(X_beats_start,20)
# feature.music[,5]<-variable.feature(X_sections_confidence,20)
# feature.music[,6]<-variable.feature(X_sections_start,20)
# feature.music[,7]<-variable.feature(X_segments_confidence,20)
# feature.music[,8]<-variable.feature(X_segments_loudness_max,20)
# feature.music[,9]<-variable.feature(X_segments_loudness_max_time,20)
# feature.music[,10]<-variable.feature(X_segments_loudness_start,20)
# feature.music[,11]<-variable.feature(X_segments_pitches,20)
# feature.music[,12]<-variable.feature(X_segments_start,20)
# feature.music[,13]<-variable.feature(X_segments_timbre,20)
# feature.music[,14]<-variable.feature(X_song,20)
# feature.music[,15]<-variable.feature(X_tatums_confidence,20)
# feature.music[,16]<-variable.feature(X_tatums_start,20)

# setwd("~/Desktop/Project4_data")
# save(file="feature_music.RData",feature.music)

load("~/Desktop/Project4_data/feature_music.RData") ### feature.music

###################
# 
# var.name<-c("X_bars_confidence","X_bars_start","X_beats_confidence",
#             "X_beats_start","X_sections_confidence","X_sections_start",
#             "X_segments_confidence","X_segments_loudness_max",
#             "X_segments_loudness_max_time","X_segments_loudness_start",
#             "X_segments_pitches","X_segments_start","X_segments_timbre",
#             "X_songs",
#             "X_tatums_confidence","X_tatums_start")





###############################
####  Topic Modeling  #########

load("~/Desktop/Project4_data/lyr.RData")

vocab<-names(lyr[-1])

get.terms <- function(x) {
  index <- match(x, vocab)
  c<-x[-1]        #drop the first track id
  index=which(c!=0)
  c_not0=c[which(c!=0)]
  c_not0=as.matrix(rbind(as.integer(index-1),as.integer(c_not0)))
  return(c_not0)
}


load("~/Desktop/Project4_data/wordlist.RData") ##  word.list

D <- length(word.list)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(word.list, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)


word.ranked<-c()
for (i in 1:2350){
  word.ranked[i]<-sum(as.numeric(lyr[,i+1]))
}
word.ranked<-sort(word.ranked,decreasing = TRUE)
word.ranked<-word.ranked[which(word.ranked!=0)]
word.frequency <- as.integer(word.ranked)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, 


K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02


set.seed(357)
fit<- lda.collapsed.gibbs.sampler(documents = word.list, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

#####  $document_sums : each observation's probability on topics  ###
#####  $topics : each topic's lyr (1,5000)

save(file = "fit.RData",fit)
#####  Create Response Variable  #####

Y<-c()
for (i in 1:2350){
  Y[i]<-which(fit1$document_sums[,i]==max(fit1$document_sums[,i]))
}

names(fit1)
a<-fit1$topics


#############   test.R  ########


#have to load fit model 
#train.feature  


load("~/Desktop/Project4_data/fit.RData") ### fit 
load("~/Desktop/Project4_data/feature_music.RData") ### feature.music==train.feature

Y<-c()
for (i in 1:2350){
  Y[i]<-which(fit1$document_sums[,i]==max(fit1$document_sums[,i]))
}


test.music<-function(train.feature,
                     test.feature,
                     train.topic,
                     model.fit){
  
  hahaha<-knn3Train(train.feature[,-14], test.feature[,-14],
                    train.topic, k = 5, prob = TRUE) 
  heihei<-attributes(hahaha)$prob
  
  n<-nrow(test.feature)
  
  finally<-attributes(hahaha)$prob %*% model.fit$topics
  
  for (i in 1:n){
    finally[i,]<-order(finally[i,])
  }
  
  return(finally)
  
}





# lll<-test.music(feature.music[1:1000,],feature.music[1000:1200,],
#            Y[1:1000],fit)




