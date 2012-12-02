imdb <- read.csv("imdb-unigrams.csv",header = T,sep=";")
imdb <- imdb[281 : dim(imdb)[1],]
count <- matrix(as.vector(imdb[1 : 1048330 , 3]), ncol = 10, byrow = TRUE)
index<-seq(0,104832,)*10+1
word <- imdb[index,1]
k <- which(is.na(count[, 1]))
count <- count[-k, ]
word <- word[-k]
total.count<-imdb[1:10,4]
rating.prior<-total.count/sum(total.count)
prob<-sweep(count,MARGIN=2,STATS=1/total.count,FUN="*")

twit.mat <- feature.matrix.full
# Match features to emotional dictionary
wordmatch<-match(colnames(twit.mat),word)

prior<-prob[wordmatch,]
prior[is.na(prior)] <- 1
prior[prior==0] <- 1

# Find the 10-catogory score for each twitts
# This is the log prob
score<-as.matrix(twit.mat) %*% log(prior)
score<-score+log(rating.prior)
score<-exp(score)
score[is.na(score)]<-0
score.norm<-t(scale(t(score), center=FALSE, scale = colSums(t(score))))
score.norm[is.na(score.norm)] <- 0

index=which(sentiment==1)
plot(score.norm[index[1],],type="l",ylim=c(0,1))
for(i in index){
  lines(score.norm[i,],type="l")
}

index=which(sentiment==-1)
plot(score.norm[index[1],],type="l",ylim=c(0,1))
for(i in index){
  lines(score.norm[i,],type="l")
}
index=which(sentiment==0)
plot(score.norm[index[1],],type="l",ylim=c(0,1))
for(i in index){
  lines(score.norm[i,],type="l")
}


diag.rate=rep(0,10)
for(i in 1:10){
  test <- sample(seq(1:length(sentiment)),size=length(sentiment)/5, replace=FALSE)
  train <- seq(1:length(sentiment))[-test]
  modelKNN <- knn(train=cbind(twit.mat[train,],score.norm[train,]),
                  cl=sentiment[train],
                  test=cbind(twit.mat[test,],score.norm[test,]),k=3)
  #   modelKNN <- knn(train=score.norm[train,],
  #                   cl=sentiment[train],
  #                   test=score.norm[test,],k=3)
  tab <- table(modelKNN, sentiment[test])
  diag.rate[i] <- sum(diag(tab))/length(test)
}
# Using only score matrix --> Around 40 % - 50 %
# Using both score matrix and unigram feature matrix --> Around 60%

# SVM, provides  around 60 % accuracy, similar as KNN
diag.rate=rep(0,5)
for(i in 1:5){
  feature.temp <- twit.mat
  test <- sample(seq(1:length(sentiment)),size=length(sentiment)/5, replace=FALSE)
  train <- seq(1:length(sentiment))[-test]
  count <- apply(feature.temp[train,],2,sum)
  if(min(count) == 0){
    feature.temp <- twit.mat[,-which(count == 0)]
  } 
  modelSVM<-svm(x=cbind(feature.temp[train,],score[train,]),
                y=sentiment[train],type="C-classification")
  pred<-predict(modelSVM,cbind(feature.temp[test,],score[test,]))
  tab <- table(pred, sentiment[test])
  diag.rate[i] <- sum(diag(tab))/length(test) 
}
###############################################################################################
# modelKNN<-knn(train=score[train,],cl=sentiment[train],test=score[test,],k=3)
# table(modelKNN, sentiment[test])

# Normalize each row of 10-category distribution

# nomatch <- which(is.na(score.norm[,1]))
# score.norm<-score.norm[-nomatch,]
# sentiment<-sentiment[-nomatch]

# Megnify polarity
smooth <- (seq(1:10)-5)^2/5
score.smooth <- score.norm
for(i in 1:dim(score.norm)[1]){
  score.smooth[i,] <- score.norm[i,] * smooth
  score.smooth[i,] <- score.smooth[i,]/sum(score.smooth[i,])
}

index=which(sentiment==1)
plot(score.norm[index[1],],type="l",ylim=c(0,1))
for(i in index){
  lines(score.smooth[i,],type="l")
}

index=which(sentiment==-1)
plot(score.norm[index[1],],type="l",ylim=c(0,1))
for(i in index){
  lines(score.smooth[i,],type="l")
}


# Building Similarity Matrix
S <- matrix(0,dim(score)[1],dim(score)[1])
for(i in 1:dim(score)[1]){
  for(j in 1:dim(score)[1]){
    S[i,j]<-sum(score.smooth[i,]*log(score.smooth[i,]/score.smooth[j,]))
  }
}
S <- (S+t(S))/2
# Apply K nearest neigbour on the graph
for(i in 1:dim(S)[1]){
  S[i,which(S[i,]<quantile(S[i,],0.66))]<-0
}
#library(lattice)
#levelplot(S)

# Laplacian Matrix Transformation

Dinv=diag(1/rowSums(S))
L=diag(rep(1,nrow(S)))-Dinv %*% S
# D <- diag(apply(S, 2, sum))
# L <- D - S
eigen <- eigen(L,symmetric=TRUE)
eigenK <- eigen$vectors[,1:20]
eigenK.norm<-scale(eigenK, center=FALSE, scale = colSums(score))
modelK <- KmeansPower(eigenK.norm,3)
table(modelK[[1]],sentiment)
modelK <- kmeans(eigenK.norm, centers=3)
table(modelK$cluster,sentiment)


modelSP<-specc(score.norm,centers=3)
table(as.numeric(modelSP),sentiment)