x <- function(something) {
  
  return(TRUE)
}

loocv <- function(nlm, dat) {
  return(mean(((dat$NR-fitted(nlm))/(1-hatvalues(nlm)))^2))
}

x(1)

dat = read.table("http://stat.cmu.edu/~larry/=stat401/PlantData.txt")
# pairs(dat)
# library(knitr)
cols <- ncol(dat)
data.summary <- data.frame(Min = sapply(X=1:cols, FUN=function(X) min(dat[,X])), 
                           Max = sapply(X=1:cols, FUN=function(X) max(dat[,X])),
                           Mean = sapply(X=1:cols, FUN=function(X) mean(dat[,X])),
                           Median = sapply(X=1:cols, FUN=function(X) median(dat[,X])),
                           Variance = sapply(X=1:cols, FUN=function(X) var(dat[,X]))
                           )
                           
row.names(data.summary) <- c(names(dat))
kable(data.summary, row.names=TRUE, digits=4, caption="Summary statistics for Plant Data set")

par(mar = c(7,4.5,2,2) + 0.1)
par(bg = "azure")
par(mfrow=c(1,1))
plot(dat[,1], which=1)

colMeans()

# Rough report

# Introduction


# EDA

# Need to do MLR basic

# brush ip on diagnostics and use some of them

# Discuss

# Conclude