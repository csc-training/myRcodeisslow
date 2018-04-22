#library(foreach)
library(doParallel)

clsize <- 2

#hostlist <- rep("localhost",clsize)
#cl<-makePSOCKcluster(hostlist)

cl <- makeCluster(2)
registerDoParallel(cl)

times.serial <- list()
times.parallel <- list()


#############################
# random forests

library(randomForest)


x <- matrix(runif(500), 100) #try changing 500 to 5000

y <- gl(2, 50)


#try changing 1000 to 10000
times.serial$randomforest <-  
  system.time(rf <- foreach(ntree=rep(1000, clsize), .combine=randomForest::combine) %do%
              randomForest(x, y, ntree=ntree))

times.parallel$randomforest <- 
system.time(rf <- foreach(ntree=rep(1000, clsize), .combine=randomForest::combine, .packages='randomForest') %dopar%
              randomForest(x, y, ntree=ntree))


#############################
# generalized linear model

data(iris)

x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 1000 #try changing this to 10000


times.serial$irisglm <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %do% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})

times.parallel$irisglm <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %dopar% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})

#########################
# cardinal sine function

x <- seq(-16, 16, by=0.05)

times.serial$sinc <- 
  system.time(v <- foreach(y=x, .combine="cbind") %do% {
  r <- sqrt(x^2 + y^2) + .Machine$double.eps
  sin(r) / r
})

times.parallel$sinc <- 
  system.time(v <- foreach(y=x, .combine="cbind") %dopar% {
  r <- sqrt(x^2 + y^2) + .Machine$double.eps
  sin(r) / r
})

#######################
# sorting


times.serial$sorting <- 
system.time(a<-foreach(i=1:clsize) %do% system.time(sort(runif(1e7))))

times.parallel$sorting <- 
system.time(a<-foreach(i=1:clsize) %dopar% system.time(sort(runif(1e7))))



########################
stopCluster(cl)

library(dplyr)
times <- rbind(do.call(rbind,times.serial),
      do.call(rbind,times.parallel))
row.names(times) <- NULL
times <- as_tibble(times)
times %>% 
  select(1:3) %>% 
  mutate(task=rep(names(times.serial),2),
         method=rep(c("serial","parallel"),each=4)) %>% 
  arrange(task)
  
