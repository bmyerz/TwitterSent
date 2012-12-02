TestSentiment<-function (textColumns, algorithm = "bayes", pstrong = 0.5, pweak = 1, prior = 1, verbose = FALSE, ...) 
{
  require(sentiment)
  matrix <- create_matrix(textColumns, ...)
  lexicon <- read.csv(system.file("data/subjectivity.csv.gz", 
                                  package = "sentiment"), header = FALSE)
  counts <- list(positive = length(which(lexicon[, 3] == "positive")), 
                 negative = length(which(lexicon[, 3] == "negative")), 
                 total = nrow(lexicon))
  documents <- c()
  for (i in 1:nrow(matrix)) {
    if (verbose) 
      print(paste("DOCUMENT", i))
    scores <- list(positive = 0, negative = 0)
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    for (word in words) {
      index <- pmatch(word, lexicon[, 1], nomatch = 0)
      if (index > 0) {
        entry <- lexicon[index, ]
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        score <- pweak
        if (polarity == "strongsubj") 
          score <- pstrong
        if (algorithm == "bayes") 
          score <- abs(log(score * prior/count))
        if (verbose) {
          print(paste("WORD:", word, "CAT:", category, 
                      "POL:", polarity, "SCORE:", score))
        }
        scores[[category]] <- scores[[category]] + score
      }
    }
    if (algorithm == "bayes" && scores[[1]]+scores[[2]]!=0) {  # Change here!
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    }
    else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    if(scores[[1]]+scores[[2]]!=0)    ratio <- as.integer(abs(scores$positive/scores$negative))
    if(scores[[1]]+scores[[2]]==0)    ratio <-1
    if (ratio == 1) 
      best_fit <- "neutral"
    documents <- rbind(documents, c(scores$positive, scores$negative, 
                                    abs(scores$positive/scores$negative), best_fit))
    if (verbose) {
      print(paste("POS:", scores$positive, "NEG:", scores$negative, 
                  "RATIO:", abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  colnames(documents) <- c("POS", "NEG", "POS/NEG", "BEST_FIT")
  return(documents)
}
CleanText <- function(x, stops, neutralize=F, remove=NULL){
  
  # convert to lowercase
  x <- tolower(x)
  
  # Remove numbers
  x <- gsub("\\d+", "", x)
  
  # Remove punctiation
  x <- gsub("[']+(?!ll)", "", x, perl = T)
  x <- gsub("[^[:alpha:]'[:space:]]", " ", x, perl = T)
  
  # Insert apostraphes in all contactions
  
  x <- gsub("\\b(that|there|where|who)(d|ll|s)\\b", "\\1'\\2", x)
  x <- gsub("\\b(she|he)(s)\\b", "\\1'\\2", x)
  x <- gsub("\\bi(m|ve)", "i'\\1", x)
  x <- gsub("\\b(what|they|you)(d|ll|ve|re)", "\\1'\\2", x)
  x <- gsub("\\bweve", "we've", x)
  
  # Insert NOT
  x <- gsub("\\b(are|ca|could|did|does|do|had|has|have|ai)nt\\b", " not", x)
  x <- gsub("\\b(is|might|mus|sha|should|was|were|would|wo)nt\\b", 
            " not", x)
  x <- gsub("\\b(are|ca|could|did|does|do|had|has|have|ai)n't\\b", " not", x)
  x <- gsub("\\b(is|might|mus|sha|should|was|were|would|wo)n't\\b", 
            " not", x)
  
  # Remove stops words
  x <- removeWords(x, stops)
  
  # Neutralize
  if(neutralize == T){
    x <- gsub("^[[:print:]]*([[:punct:]]+|no idea|don.?t know|\\bn[[:punct:]]?a[[:punct:]]?\\b)[[:print:]]*$",
              "", x, ignore.case=T)
    x <- gsub("^(.)\\1*$", "", x)
    x[x == "" | is.na(x)] <- "neutral"
  }
  
  if(!is.null(remove)){
    x <- gsub(paste("\\b(", paste(remove, collapse = "|"), ")\\w*?\\b", 
                    sep=""), "", x)
    if(neutralize == T){
      x[x == "" | grepl("^\\s+$", x)] <- "neutral"
    }else{
      x[grepl("^\\s+$", x)] <- ""
    }
  }
  
  # remove empty spaces
  x <- gsub("(?<=\\s)\\s+|^\\s+|\\s+(?=$)", "", x, perl = T)
  
  x
}
################################################################################
install.packages(c("plyr","Snowball","tm","RWeka","e1071","chron"))
library(plyr)
library(Snowball)
library(tm)
library(RWeka)
library(e1071)
library(chron)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
#install package RStem from file
#install.packages("~/Dropbox/UW/STAT 502/Walking in Tweeter/Using/Rstem_0.4-1.tar.gz", repos = NULL, 
#                type = "source", 
#                lib="/Library/Frameworks/R.framework/Versions/2.15/Resources/library")

#################################################################################
# data<-read.csv("LabeledSetBrandon.csv",header=T);text.raw<-as.character(data[,3])
# data<-read.csv("twits_11_7_2012.csv",header=T);text.raw<-as.character(data[,3])
# data<-read.table("textextracted.txt",header=T);text.raw<-as.character(data[,2])
#################################################################################
data1<-read.csv("1500_2999_Labeled.csv",header=T)
data2<-read.csv("3000_4499_Labeled.csv",header=T)
names(data1)[15]<-"sentiment"
data<-rbind(data2,data1)
text.raw<-as.character(data[,3])

sentiment<-data[,15]
nonEng<-which(!sentiment %in% c(-1,0,1))
data<-data[-nonEng,]
N<-dim(data)[1]
text.raw<-as.character(data[,3])
time<-data[,4]
sentiment<-data[,15]
Sys.setlocale(locale="C")

at.exp<-"(@)([[:graph:]]+)"
topic.exp<-"(#)([[:graph:]]+)"
http.exp<-"(http:)([[:graph:]]*)"
###################################  First level features
Exclamation<-rep(0,N)
Question<-rep(0,N)        
RT<-rep(0,N)                #  indicator of RT
URL<-rep(0,N)
#####################
eyes <-"(:|;|=|8)"           # Construct emoticons in the form of :-) or ;-P, etc...
noses <- "(|  |-|o|O)"
smiles <- "(\\)|\\]|\\}|D|P)"
smiles.rev<-"(\\[|\\{|\\()"
emo.pos<-paste(eyes,noses,smiles,sep="")
emo.pos.false<-"(8|=)(\\)|\\]|\\}|D|P)"
emo.neg<-paste(eyes,noses,smiles.rev,sep="")
emo.neg.false<-"(8|=)(\\[|\\{|\\()"
Ne<-2                        # Assume all negative/positive emoticons are not identical
Emoticon<-matrix(0,N,Ne)
###################################
text<-text.raw
for(i in 1:N){
  if(length(text.raw[i])==0) next
  Exclamation[i]<-length(gregexpr("!",text.raw[i])[[1]])
  Question[i]<-length(gregexpr("\\?",text.raw[i])[[1]])   
  Emoticon[i,1]<-length(grep(emo.pos,text.raw[i])) - length(grep(emo.pos.false,text.raw[i])) 
  Emoticon[i,2]<-length(grep(emo.neg,text.raw[i])) -length(grep(emo.neg.false,text.raw[i]))
  URL <- length(grep(http.exp,text.raw[i]))
  text[i]<-enc2native(text.raw[i])
  text[i]<-gsub(pattern=at.exp," ",text[i])
  text[i]<-gsub(pattern=http.exp," ",text[i])
  text[i]<-gsub(pattern=topic.exp," ",text[i])
  text[i]<-CleanText(text[i],stops=NULL,neutralize=F)
  # CleanText Change all n't or nt into not; convert to lower case; remove number & punctiation
  RT[i]<-length(grep("rt",text[i]))
}
###################################
text.str<-NULL
for(i in 1:length(text)) { # remove one letter strings that are left over from splits
  if(length(text[i])==0) {text.str[i]=NA;next}
  current.string <- strsplit(text[i], split = " ")[[1]]
  string.length <-length(current.string)
  if(string.length<1) {text.str[i]=NA;next}
  for(j in 1:string.length){
    if(nchar(current.string[(string.length - j + 1)]) == 1){
      current.string <- current.string[-(string.length - j + 1)]
    }
  }
  current.string <- paste(current.string, collapse = ' ')
  text.str[i] <- current.string  
}

twits.corpus <- Corpus(VectorSource(text.str))
twits.corpus.full <- twits.corpus 
#inspect(twits.corpus[3])
stopwords <- c("www","gt","lt","amp","rt","via","wanna")
#test.stopwords<-c("android","app","apple","galaxy","nexus","google","ios","iphone","microsoft","samsung","twitter","siri","windows")
en.remove<-c(55,56,136,141,168,169,170,184,185,186,187,207,212,213,217,219,220,221,251,252,253,313,314,318,348,355,356,357,479,480,481)
my.stopwords <- c(stopwords('english')[-en.remove],stopwords) 
twits.corpus <- tm_map(twits.corpus, removeWords, my.stopwords)
#twits.corpus <- tm_map(twits.corpus, removeWords, test.stopwords)
MyTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
twits.tdm    <- TermDocumentMatrix(twits.corpus, control = list(weighting = weightTf,tokenize = MyTokenizer )) #weightBin or weightTfIdf
twits.tdm.full <- TermDocumentMatrix(twits.corpus.full, control = list(weighting = weightTf,tokenize = MyTokenizer ))
# par(mfrow=c(1,1))
# plot(twits.tdm, terms = findFreqTerms(twits.tdm, lowfreq = 15), corThreshold = 0.1)

freq.term<-findFreqTerms(twits.tdm, lowfreq = 5)
feature.matrix    <- as.data.frame(t(as.matrix(twits.tdm[freq.term])))
feature.matrix<-cbind(feature.matrix,Exclamation,Question,RT,Emoticon, URL)
feature.matrix.full    <- as.data.frame(t(as.matrix(twits.tdm.full[freq.term])))
feature.matrix.full    <- cbind(feature.matrix.full,Exclamation,Question,RT,Emoticon, URL)
################################################################################################################
test<-seq(1:round(dim(feature.matrix)[1]/5))
train<-seq(round(dim(feature.matrix)[1]/5),dim(feature.matrix)[1])

#Naive Bayes Failed somehow??
modelNB<-naiveBayes(x=feature.matrix[train,], y=as.factor(sentiment[train]))
pred<-predict(modelNB,feature.matrix[test,])
table(pred,sentiment[test])

modelBN <- naive.bayes(feature.matrix[train,],explanatory=sentiment[train])


diagKNN.rate=rep(0,10)
diagSVM.rate=rep(0,10)
# weight=table(as.factor(sentiment))
# weight=weight/sum(weight)
for(i in 1:5){
  feature.temp <- feature.matrix
  test <- sample(seq(1:length(sentiment)),size=length(sentiment)/5, replace=FALSE)
  train <- seq(1:length(sentiment))[-test]
  count <- apply(feature.temp[train,],2,sum)
  if(min(count) == 0){
    feature.temp <- feature.temp[,-which(count == 0)]
  } 
  
  # SVM, provides 50 % - 60 % accuracy
  modelSVM<-svm(x=feature.temp[train,], y=as.factor(sentiment[train]),type="C-classification")
  pred<-predict(modelSVM,feature.temp[test,])
  tab <- table(pred, sentiment[test])
  diagSVM.rate[i] <- sum(diag(tab))/length(test) 
  
  #KNN, provides around 50 % - 60 % accuracy
  modelKNN<-knn(train=feature.matrix[train,],cl=sentiment[train],test=feature.matrix[test,],k=3)
  tab<-table(modelKNN, sentiment[test])
  diagKNN.rate[i] <- sum(diag(tab))/length(test) 
}



# PCA and K-Means, bad prediction 20 % - 30 %
modelPCA<-princomp(feature.matrix,cor=T)
loading<-modelPCA$loadings[,1:20]
pca.feature<-as.matrix(feature.matrix) %*% as.matrix(loading)
modelPCA.K<-kmeans(pca.feature,center=3)
table(modelPCA.K$cluster,sentiment)
modelPCA.K<-KmeansPower(pca.feature,3)
table(modelPCA.K[[1]],sentiment)

# Using someone else's dictionary
modelSentiment<-TestSentiment(text.str)
tab <- table(modelSentiment[,4],sentiment)
sum(diag(tab))/N
################################################################################################################
time<-strsplit(as.character(time),split=" ")
time.new<-matrix(0,length(time),3)
for( i in 1:length(time)){
  time.new[i,1]<-paste(time[[i]][2],time[[i]][3],time[[i]][6])
  time.new[i,2]<-time[[i]][4]
  time.new[i,3]<-time[[i]][1]
}

time.num <- chron(dates=time.new[,1],times=time.new[,2], format=c('mon d Y','h:m:s'))
week <-time.new[,3]
tab <- table(week,sentiment)
#######################################################################################################
install.packages(c("kernlab"))
library(kernlab)
cov<-cov(t(feature.matrix))
modelSP<-specc(cov,centers=3)