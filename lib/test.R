library(caret)

load("~/Desktop/Project4_data/fit.RData") ### fit 
load("~/Desktop/Project4_data/feature_music.RData") ### feature.music==train.feature

Y<-c()
for (i in 1:2350){
  Y[i]<-which(fit$document_sums[,i]==max(fit$document_sums[,i]))
}


test.music<-function(train.feature,
                     test.feature,
                     train.topic,
                     model.fit){
  
  hahaha<-knn3Train(train.feature[,-14], test.feature[,-14],
                    train.topic, k = 5, prob = TRUE) 
  
  
  a<-attributes(hahaha)$prob[,1:10]#1-10
  b<-attributes(hahaha)$prob[,11:17]#12-18
  c<-attributes(hahaha)$prob[,18]#20
  d<-rep(0,100)
  
  heihei<-cbind(a,d,b,d,c)
  
  finally<-heihei %*% model.fit$topics
  
  
  n<-nrow(test.feature)
  
  for (i in 1:n){
    finally[i,]<-order(finally[i,],decreasing = TRUE)
  }
  
  return(finally)
  
}

test_label<-test.music(feature.music,test_feature, Y,fit)

