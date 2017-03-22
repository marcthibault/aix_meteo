library(FactoMineR)
library(energy)
library(mvtnorm)
library(ggplot2)
library(data.table)
library(stats)


model_AR<- function(n) {
  # appliquer un AR
  ar_estimated <- ar(x, aic = TRUE, order.max = n, method=c("yule-walker", "burg", "ols", "mle", "yw"))
  
  innovation <- x
  
  for (i in 1:n) {
    a <- x[(i+1):length(x)]
    for (j in 1:i) {
      a[[length(a) + 1]] = 0
    }
    
    innovation <- innovation - ar_estimated$ar[i]*a
  }
  return(innovation)
}


