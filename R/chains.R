setwd("C:/cygwin/home/bdmyers/twitter_results/")

users <- read.csv(file="users_v1.csv",sep=",")
chains <- read.csv(file="chains_v1.csv",sep=",")

histogram_properties <- function() {
  hist(users$statuses_count,probability=TRUE)
  hist(users$followers_count,probability=TRUE)
  hist(users$friends_count,probability=TRUE)
  hist(table(users$lang))
}

chain_length <- function(chain_id) {
  return(dim(chains[chains$chain==chain_id,])[1])
}

# join on chains.user_id=users.id
chains.joined <- merge(chains, users, by.x="user_id", by.y="id")

# order by chain then by position
chains.joined.sort <- chains.joined[with(chains.joined, order(chain,position)),]

chain_property <- function(property_str, chain_id, range) {
  return (chains.joined.sort[chains.joined.sort$chain==chain_id, property_str][range])
}

# distribution test
broken_tests <- function() {
  pvalues <- vector(mode="numeric", length=5000/200)
  i <- 1
  for (start in seq(1,5000,200)) {
    #pvalues[i] <- wilcox.test(chain_property("statuses_count",0,start:(start+5000)), 
    #          chain_property("statuses_count",1,start:(start+5000)),
    #          alternative="two.sided",
    #          paired=FALSE)$p.value
    #   pvalues[i] <- ks.test(chain_property("statuses_count",0,start:(start+5000)), 
    #             chain_property("statuses_count",1,start:(start+5000)),
    #             alternative="two.sided",
    #             paired=FALSE)$p.value
    i <- i+1
  }
}



window_stats <- function(smpl, window_size=2000, step=20) {
  ll <- length(seq(window_size+1, length(smpl), step))
  means <- vector(mode="numeric",length=ll)
  vars <- vector(mode="numeric",length=ll)
  medians <- vector(mode="numeric", length=ll)
  index <- 0
  for (i in seq(window_size+1, length(smpl), step)) {
    means[index] <- mean(smpl[i-window_size:i+window_size)])
    vars[index] <-    var(smpl[i-window_size:i+window_size)])
    medians[index] <- median(smpl[i-window_size:i+window_size)])
    index <- index+1
  }
  
  return (list(means,vars,medians))
}

library(zoo)
window_stats2 <- function(smpl, window_size=1999) {
  means <- rollmean(x=smpl, k=window_size)
  medians <- rollmedian(x=smpl, k=window_size)
  vars <- rollapply(data=smpl, width=window_size, FUN=var)
  return( list( means, medians, vars ) )
}

# takes the stat for last half of each set of iterations
window_stats3 <- function(smpl) {
  means <- vector(mode="numeric", length=length(smpl))
  for (i in 1:length(smpl)) {
    means[i] <- mean(smpl[(i/2):i])
  }
  
  medians <- vector(mode="numeric", length=length(smpl))
  for (i in 1:length(smpl)) {
    medians[i] <- median(smpl[(i/2):i])
  }
  
  vars <- vector(mode="numeric", length=length(smpl))
  for (i in 1:length(smpl)) {
    vars[i] <- var(smpl[(i/2):i])
  }
  
  return (list (means, medians, vars))
}

plot_metric_stats3 <- function(ci, smpl, name, take_log=TRUE) {
  par(mfrow=c(2,2))
  mv0 <- window_stats3(smpl)
  print(sprintf("plotting %d", ci))
  ll <- length(smpl)
  plot(1:ll, mv0[[1]], col="red", ylab="mean", xlab="iter",ylim=c(0,400))
  plot(1:ll, mv0[[2]], col="green", ylab="median", xlab="iter", ylim=c(0,20))
  plot(1:ll, mv0[[3]], col="blue", ylab="variance", xlab="iter",ylim=c(0,6e+06))
  
  if (take_log) {
    plot(1:ll, log(base=2,x=smpl), col="black", ylab=name, xlab="iter")
    lines(1:ll, log(base=2,mv0[[1]]), col="red")
  } else {
    plot(1:ll, smpl, col="black", ylab=name, xlab="iter")
    lines(1:ll, mv0[[1]], col="red")
  }
  
  return (mv0)
}


plot_chain_windowed_stats <- function(ci, property_name, window_size=2000, step=20, take_log=TRUE) {
  par(mfrow=c(2,2))
  mv0 <- window_stats(chain_property(property_name, ci, 1:chain_length(ci)), window_size=window_size, step=step)
  print(length(mv0[[1]]))
  print(length(seq(window_size+1, chain_length(ci), step)))
  plot(seq(window_size+1, chain_length(ci)-window_size, step), mv0[[1]], col="red", ylab="mean", xlab="iter")
  plot(seq(window_size+1, chain_length(ci)-window_size, step), mv0[[2]], col="blue", ylab="variance", xlab="iter")
  plot(seq(window_size+1, chain_length(ci)-window_size, step), mv0[[3]], col="green", ylab="median", xlab="iter")
  
  if (take_log) {
    plot(1:chain_length(ci), log(base=2,x=chain_property(property_name, ci, 1:chain_length(ci))), col="black", ylab=property_name, xlab="iter")
    lines(seq(window_size+1, chain_length(ci)-window_size, step), log(base=2,mv0[[1]]), col="red")
  }
}

plot_metric_stats2 <- function(ci, smpl, name, window_size=1999, take_log=TRUE) {
  par(mfrow=c(2,2))
  mv0 <- window_stats2(smpl, window_size=window_size)
  print(sprintf("plotting %d", ci))
  plot(window_size:chain_length(ci), mv0[[1]], col="red", ylab="mean", xlab="iter")
  plot(window_size:chain_length(ci), mv0[[2]], col="green", ylab="median", xlab="iter")
  plot(window_size:chain_length(ci), mv0[[3]], col="blue", ylab="variance", xlab="iter")
  
  if (take_log) {
    plot(1:chain_length(ci), log(base=2,x=smpl), col="black", ylab=name, xlab="iter")
    lines(window_size:chain_length(ci), log(base=2,mv0[[1]]), col="red")
  } else {
    plot(1:chain_length(ci), smpl, col="black", ylab=name, xlab="iter")
    lines(window_size:chain_length(ci), mv0[[1]], col="red")
  }
}

plot_chain_windowed_stats2 <- function(ci, property_name, window_size=1999, take_log=TRUE) {
  plot_metric_stats2(ci, chain_property(property_name, ci, 1:chain_length(ci)), property_name, window_size, take_log)
}



output_property <- function(property_name, window_size=1999, take_log=TRUE, ids=unique(chains.joined.sort[,"chain"])) {
  for (chain_id in ids) {
    png(filename=sprintf("chain%d_%s_.png", chain_id, property_name), width=1024, height=768)
    plot_chain_windowed_stats2(chain_id, property_name, window_size, take_log)
    dev.off()
  }
}

#output_property("followers_count")
#output_property("friends_count")
#output_property("statuses_count")

charcount <- function(s) {
  #return (length(strsplit(s,split="")))
  return (length(charToRaw(s)))
}

charcount_all <- function(strseq) {
  v <- vector(mode="character",length(strseq))
  for (i in 1:length(strseq)) {
    v[i] <- charcount(strseq[i])
    print(i)
  }
  return (v)
}

cca7 <- as.integer(charcount_all(as.character(chain_property("screen_name", 7, 1:chain_length(7)))))

# TODO some plot of quartiles
# e.g. chain10, what are those low strings? check where the chain was restarted due to error
## for chain10, flat areas are not restart points

# user ids
par(mfrow=c(1,1))
hist(users$id)

ids <- read.csv(file="sample0v1.csv",sep=",")
users <- read.csv(file="users_v2.csv",sep=",")
hist(ids$user_id)
hist(ids$statuses_count)

property_cdf <- function(property_name, data) {
  plot.ecdf(data, main=sprintf("ecdf(%s)",property_name)
                , xlab=sprintf("%s",property_name)
                , ylab="")
}

plot_cdfs <- function(dataset) {
  par(mfrow=c(2,2))
  with(dataset, property_cdf("statuses_count", statuses_count))
  with(dataset, property_cdf("followers_count", followers_count))
  with(dataset, property_cdf("friends_count", friends_count))
  with(dataset, property_cdf("screen_name", as.numeric(charcount_all(as.character(screen_name)))))
}

# language stats
language_stats <- function() {
  # percent marked lang="en"
  dim(ids[ids$lang=="en",])[1]/dim(ids)[1]
  
  # number of language entities observed
  length(levels(ids$lang))
}

# test convergence
library(coda)

plot_gelman <- function(prop, chainsv=7:9, layout=TRUE) {
  len_to_use <- min(sapply(X=chainsv, FUN=chain_length))
  ll <- list()
  for (i in chainsv) {
    # add one to all values in case log transformation is required in gelman diagnostic
    ll <- append(ll, values=list(mcmc(data=chain_property(prop,i,1:len_to_use)+1)))
  }
#   mc7 <- mcmc(data=chain_property(prop,7,1:len_to_use)+1)
#   mc8 <- mcmc(data=chain_property(prop,8,1:len_to_use)+1)
#   mc9 <- mcmc(data=chain_property(prop,9,1:len_to_use)+1)
  
#   gelman.plot(x=mcmc.list(mc7,mc8,mc9),transform=TRUE,autoburnin=TRUE,auto.layout=layout,
#               main=sprintf("%s", prop))

  gelman.plot(x=mcmc.list(ll),transform=TRUE,autoburnin=TRUE,auto.layout=layout,
              main=sprintf("%s", prop))
}

autocorr_plot <- function(prop, chainnum) {
  len_to_use <- chain_length(chainnum)
  autocorr.plot(x=mcmc(data=chain_property(prop,chainnum,1:len_to_use)+1), main=sprintf("%s, chain %d", prop, chainnum), auto.layout=FALSE)
}

plot_gelman_snl <- function(layout=TRUE) {
  len_to_use <- min(chain_length(7),chain_length(8),chain_length(9))
  prop <- "screen_name"
  mc7 <- mcmc(data=as.integer(charcount_all(as.character(chain_property(prop,7,1:len_to_use)))))
  mc8 <- mcmc(data=as.integer(charcount_all(as.character(chain_property(prop,7,1:len_to_use)))))
  mc9 <- mcmc(data=as.integer(charcount_all(as.character(chain_property(prop,7,1:len_to_use)))))
  
  gelman.plot(x=mcmc.list(mc7,mc8,mc9),transform=TRUE,autoburnin=TRUE,auto.layout=layout)
}


# 2D convergence visual
library(graphics)
twod_convergence <- function(max_length) {
  len_to_use <- min(chain_length(7),chain_length(8),chain_length(9),max_length)

  plot(x=c(0), main=sprintf("after %d iterations", len_to_use),
       ylab="log(Followers count)",
       xlab="log(Statuses count)",
       xlim=c(0,12),
       ylim=c(0,14),
       type="n")
  
  for (i in (floor(len_to_use/2)):len_to_use) {
    j <- i
    points(jitter(log(st7[j:i]+1)), jitter(log(fo7[j:i]+1)), col=rgb(0,100,0,150,maxColorValue=255), pch=16, cex=0.1)
    points(jitter(log(st8[j:i]+1)), jitter(log(fo8[j:i]+1)), col=rgb(100,0,0,150,maxColorValue=255), pch=16, cex=0.1)
    points(jitter(log(st9[j:i]+1)), jitter(log(fo9[j:i]+1)), col=rgb(0,0,100,150,maxColorValue=255), pch=16, cex=0.1)
  }
  
  # can also use jitter()
  st7 <- chain_property("statuses_count",7,1:len_to_use)
  fo7 <- chain_property("followers_count",7,1:len_to_use)
  points(log(st7), log(fo7), col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  lines(log(st7),log(fo7), col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  
  st8 <- chain_property("statuses_count",8,1:len_to_use)
  fo8 <- chain_property("followers_count",8,1:len_to_use)
  points(log(st8), log(fo8), col=rgb(100,0,0,50,maxColorValue=255), pch=16)
  lines(log(st8), log(fo8), col=rgb(100,0,0,50,maxColorValue=255), pch=16)
  
  st9 <- chain_property("statuses_count",9,1:len_to_use)
  fo9 <- chain_property("followers_count",9,1:len_to_use)
  points(log(st9), log(fo9), col=rgb(0,0,100,50,maxColorValue=255), pch=16)
  lines(log(st9), log(fo9), col=rgb(0,0,100,50,maxColorValue=255), pch=16)
  
  points(log(c(st7[1],st8[1],st9[1])),
         log(c(fo7[1],fo8[1],fo9[1])), pch="S",col=c("green","red","blue"))
  
  points(log(c(mean(st7[(len_to_use/2):len_to_use]),
               mean(st8[(len_to_use/2):len_to_use]),
               mean(st9[(len_to_use/2):len_to_use]))),
         log(c(mean(fo7[(len_to_use/2):len_to_use]),
               mean(fo8[(len_to_use/2):len_to_use]),
               mean(fo9[(len_to_use/2):len_to_use]))), col="black",pch=23,bg=c("green","red","blue"))
}

# plot 2nd half chain mean
plot(x=1:ll, y=mv7_fo[[1]] / 300, col="green", xlab="last iteration", ylab="last half mean (normalized)", type="n")
lines(x=1:ll, y=mv7[[1]] / 650, col="red", xlab="last iteration", ylab="mean (normalized)")
lines(x=1:ll, y=mv7_fr[[1]] / 250, col="blue", xlab="last iteration", ylab="mean (normalized)")
lines(x=1:ll, y=mv7_fo[[1]] / 500, col="green", xlab="last iteration", ylab="mean (normalized)")
legend(x="right", y="bottom", col=c("red","blue","green"), legend=c("statues_count","friends_count", "followers_count"), lty=1)


## user id rejection-sampled data
id.sample <- read.csv(file="id_sample1.csv",sep=",")
dim(id.sample)
summary(id.sample$followers_count)
summary(id.sample$friends_count)
summary(id.sample$statuses_count)

id.sample$screen_name_length <- as.numeric(charcount_all(strseq=as.character(id.sample$screen_name)))

plot_id_sample_scalar_est_hist <- function() {
  par(mfrow=c(2,2))
  hist(log(id.sample$statuses_count,2), main="statuses_count", xlab="log2(statuses_count)")
  hist(log(id.sample$friends_count,2), main="friends_count", xlab="log2(friends_count)")
  hist(log(id.sample$followers_count,2), main="followers_count", xlab="log2(followers_count)")
  hist(id.sample$screen_name_length, main="screen_name length", xlab="screen_name length")
}

plot_means_scalar_est <- function() {
  # filter out the id samples with 0 connections, as they are unreachable in random walks
  id.sample.filtered <- id.sample[id.sample$followers_count+id.sample$friends_count>=2,]
  dim(id.sample.filtered[id.sample.filtered$statuses_count==0,])[1]/dim(id.sample.filtered)[1]
  dim(id.sample.filtered)[1]/dim(id.sample)[1]
  
  fo_gt <- summary(id.sample$followers_count)
  fr_gt <- summary(id.sample$friends_count)
  st_gt <- summary(id.sample$statuses_count)
  
  fo_gtf <- summary(id.sample.filtered$followers_count)
  fr_gtf <- summary(id.sample.filtered$friends_count)
  st_gtf <- summary(id.sample.filtered$statuses_count)
  
  st7 <- summary(chain_property("statuses_count",chain_id=7,range=(chain_length(7)/2):chain_length(7)))
  st8 <- summary(chain_property("statuses_count",chain_id=8,range=(chain_length(8)/2):chain_length(8)))
  st9 <- summary(chain_property("statuses_count",chain_id=9,range=(chain_length(9)/2):chain_length(9)))
  
  fo7 <- summary(chain_property("followers_count",chain_id=7,range=(chain_length(7)/2):chain_length(7)))
  fo8 <- summary(chain_property("followers_count",chain_id=8,range=(chain_length(8)/2):chain_length(8)))
  fo9 <- summary(chain_property("followers_count",chain_id=9,range=(chain_length(9)/2):chain_length(9)))
  
  fr7 <- summary(chain_property("friends_count",chain_id=7,range=(chain_length(7)/2):chain_length(7)))
  fr8 <- summary(chain_property("friends_count",chain_id=8,range=(chain_length(8)/2):chain_length(8)))
  fr9 <- summary(chain_property("friends_count",chain_id=9,range=(chain_length(9)/2):chain_length(9)))

  par(mfrow=c(1,1))
  # why is matrix construction opposite? :p
  stat_matrix <- function(stat) {
    m <- t(matrix(data=c(st_gt[stat],fo_gt[stat],fr_gt[stat],
                         st_gtf[stat],fo_gtf[stat],fr_gtf[stat],
                         st7[stat],  fo7[stat],  fr7[stat],
                         st8[stat],  fo8[stat],  fr8[stat],
                         st9[stat],  fo9[stat],  fr9[stat]),  
                  ncol=5, nrow=3))
    return (m)
  }
  mm <- stat_matrix("Mean")
  mm[,1] <- mm[,1]/mm[2,1]
  mm[,2] <- mm[,2]/mm[2,2]
  mm[,3] <- mm[,3]/mm[2,3]
  
  med <- stat_matrix("Median")
  med[,1] <- med[,1]/med[1,1]
  med[,2] <- med[,2]/med[1,2]
  med[,3] <- med[,3]/med[1,3]
  
  legend.names <- c("ground truth", "no disconnected", "chain 7", "chain 8", "chain 9")
  sample.colors <- gray.colors(5)
  barplot(height=mm, beside=TRUE, 
          names.arg=c("statuses_count", "followers_count", "friends_count"), 
          ylab="Mean (normalized)", ylim=c(0,2),
          col=sample.colors,
          main="Scalar estimand means (2nd 50%)")
  legend("bottomright", legend=legend.names, fill=sample.colors)
  
  barplot(height=stat_matrix("Mean"), beside=TRUE)
  barplot(height=stat_matrix("Median"), beside=TRUE)  
}

# quartile plots against ground truth
snl7 <- as.numeric(charcount_all(strseq=as.character(chain_property("screen_name",chain_id=7,range=1:chain_length(7)))))
snl8 <- as.numeric(charcount_all(strseq=as.character(chain_property("screen_name",chain_id=8,range=1:chain_length(8)))))
snl9 <- as.numeric(charcount_all(strseq=as.character(chain_property("screen_name",chain_id=9,range=1:chain_length(9)))))
qqplot( id.sample$screen_name_length, snl7[(chain_length(7)/2):chain_length(7)])


today <- as.Date("Dec 07 2012", format="%B %d %Y")
age <- function(then, format="%B %d %Y") {
  then <- paste(substring(then,5,10),substring(then,27,30)) # depend on uniform
  return (today - as.Date(then, format))
}


for (j in 1:length(id.sample$created_at)) {
  id.sample$age[j] <- age(id.sample$created_at[j])
  print(j)
}
# if the dates do not finish, sample 1000 uniformly and calculate age
chosen <- sample(1:length(id.sample$created_at), size=5000,replace=FALSE)
prog_count <- 0
for (j in 1:length(chosen)) {
  index <- chosen[j]
  id.sample$age[index] <- age(id.sample$created_at[index])
  print(prog_count)
  prog_count <- prog_count + 1
}

# lang="en"
dim(id.sample[id.sample$lang=="en",])[1]/dim(id.sample)[1]
dim(id.sample[id.sample$lang=="es",])[1]/dim(id.sample)[1]
dim(id.sample[id.sample$lang=="ja",])[1]/dim(id.sample)[1]

# verified
verified_ids <- function() {
  return(id.sample[id.sample$verified=="t",])
}
dim(verified_ids())[1]/dim(id.sample)[1]

more_friends <- function() {
  return(id.sample[id.sample$friends_count > id.sample$followers_count,])
}
more_followers <- function() {
  return(id.sample[id.sample$friends_count < id.sample$followers_count,])
}
equal_ff <- function() {
  return(id.sample[id.sample$friends_count == id.sample$followers_count,])
}
mean(more_followers()$followers_count-more_followers()$friends_count)
mean(more_friends()$friends_count-more_friends()$followers_count)
dim(equal_ff())[1]/dim(id.sample)[1]
hist(log(equal_ff()$friends_count,2))


# scatter plot matrix
pairs(~id.sample$user_id+id.sample$age+log(id.sample$followers_count,2)+log(id.sample$friends_count,2)+log(id.sample$statuses_count,2)+id.sample$verified, data=id.sample[chosen,])


# histograms
par(mfrow=c(1,3))
id_sample_hist <- function(propname, smpl) {
  hist(log(smpl+1,2), main=sprintf("%s", propname), xlab=sprintf("log2(%s)", propname))
  m <- mean(log(smpl+1,2))
  print(m)
  lines(x=c(m,m), y=c(0,max(table(smpl))), col="red")
}

ks_test_chains <- function(c1, c2, prop, iters1=chain_length(c1), iters2=chain_length(c2), jittering=TRUE) {
  kr <- NULL
  if (jittering) {
  kr <- ks.test(x=jitter(chain_property(property_str=prop,chain_id=c1,(iters1/2):iters1)),
          y=jitter(chain_property(property_str=prop,chain_id=c2,(iters2/2):iters2)))
  } else {
  kr <-  ks.test(x=(chain_property(property_str=prop,chain_id=c1,(iters1/2):iters1)), 
            y=(chain_property(property_str=prop,chain_id=c2,(iters2/2):iters2)))
  }
  return (kr$p.value)
}
