

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


