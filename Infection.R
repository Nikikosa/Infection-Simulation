#
# Infection script for Stats 128 project.
# Author: Niketa Kosyuk
# Version: 1.0.1

require(roperators)

population <- NULL

diseasesim <- function(rows = 0, cols = 0, proportion = 0.1) {
  return(population <- matrix(sample(c(0,1), size = rows*cols, replace=TRUE, prob = c(1-proportion,proportion)), 
                              nrow=rows, ncol=cols, byrow=TRUE))
}
plotinfected <- function(population,last = FALSE) {
  if(last==TRUE) {
    return(image(population,col = c("white","white","grey"),axes = FALSE, main = "Infected Grid"))
  } 
  return(image(population,col = c("white","red","grey"),axes = FALSE, main = "Infected Grid"))
}

infect <- function(data,rows, cols) {
  temp <- NULL
  
  data[1:nrow(data),2] %+=% -1; data[1:nrow(data),1] %+=% -1 # -1 -1
  temp <- data
  
  data[1:nrow(data),1] %+=% +1 # -1 0
  temp <- rbind(temp, data)
  
  data[1:nrow(data),1] %+=% +1 # -1 1
  temp <- rbind(temp, data)
  
  data[1:nrow(data),2] %+=% 1; data[1:nrow(data),1] %+=% -2 # 0 -1
  temp <- rbind(temp, data)
  
  data[1:nrow(data),1] %+=% +2 # 0 1
  temp <- rbind(temp, data)
  
  data[1:nrow(data),2] %+=% 1; data[1:nrow(data),1] %+=% -2 # 1 -1
  temp <- rbind(temp, data)
  
  data[1:nrow(data),1] %+=% +1 # 1 0
  temp <- rbind(temp, data)
  
  data[1:nrow(data),1] %+=% +1 # 1 1
  temp <- rbind(temp, data)
  
  temp <-  temp[temp[,2]<=rows,]
  temp <-  temp[temp[,1]<=cols,]
  temp <-  temp[temp[,2]>0,]
  temp <-  temp[temp[,1]>0,]
  return(as.matrix(temp)) # was unique(temp)
}

sim <- function(probInfect=.125,population) {
  b <- which(population==1,arr.ind=TRUE); originalB <- b; options(warn = -1) 
  population[population>=1] <- 2
  b <- infect(b,nrow(population),ncol(population))
  index <- sample(1:nrow(b),size=(nrow(b)*probInfect),replace=TRUE); index <- unique(index)
  b <- b[index[],]
  population[as.matrix(b)] %+=% 1
  population[population >= 2] <- 2 
  return(population)
}

sir = function(nrow=50, ncol=50, p = 0.2, p0 = 0.1, plot = FALSE, time_per_frame = 0.2,population, ...){
  if (plot == FALSE) {
  population <- diseasesim(nrow,ncol,p0)
  infectedmap <- image(population,col = c("white","grey","red"),axes = FALSE, main = "Infected Grid"); infectedmap
  Sys.sleep(time_per_frame)
  sir(nrow,ncol,p,p0,TRUE,time_per_frame,population)
  } else {
  while(TRUE) {
  population <- sim(p,population)
  infectedmap <- plotinfected(population); infectedmap
  if(length(b <- which(population==1,arr.ind=TRUE))==0) {
    infectedmap <- plotinfected(population,T); infectedmap
    break
  }
  Sys.sleep(time_per_frame)
  }
  }
}

sir(100,100,0.2,0.05,FALSE,0.2,population)

