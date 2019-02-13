#111111111111111111
data<-read.table("/Users/dijin/Desktop/??????/minipro/4/gpa.csv",header = T,sep=",")
head(data)

plot(data[,1]~data[,2],xlab='act',ylab='gpa') #plot the data gpa against act
help(abline)
help(lm)
lm(data[,1]~data[,2])   #fitting linear model
abline(lm(data[,1]~data[,2])) #draw relate line

correlation <- function(x, indices) {  #define correlation to be the interest data
  result <- cor(x[indices,1],x[indices,2])
  return(result)
}
install.packages("boot",dep=TRUE) 
library(boot)
co<-boot(data = data, statistic = correlation, R = 999,sim = "ordinary", stype = "i") #use boot to calculate
boot.ci(boot.out=co,conf=0.95) #cal the confidence interval
# sample(nrow(data),nrow(data),replace = TRUE)
# s1<-sample(data, length(data), replace = TRUE)
# plot(s1[,1]~s1[,2],xlab='act',ylab='gpa')

#222222222222222
data<-read.table("/Users/dijin/Desktop/??????/minipro/4/VOLTAGE.csv",header = T,sep=",")
remote<-data[which(data[,1] == 0),2]  #get remote
local<-data[which(data[,1] == 1),2]  #get local
data<-cbind(remote,local)
boxplot(remote,local,names=c("remote","local"),range=1.5) #generate sidebyside boxplot 

npar.resample <- function(x){  # resample and record mean 
  xstar <- sample(x, length(x), replace = T)
  median.star <- mean(xstar)
  return(median.star)
}
mean.boot.dist <- replicate(1000, npar.resample(remote)) # see distribution of resample
hist(mean.boot.dist)
qqnorm(mean.boot.dist)

mean.boot.dist <- replicate(1000, npar.resample(local)) # see distribution of resample
hist(mean.boot.dist)
qqnorm(mean.boot.dist)

t.test(remote,local, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE) #assume normal and get CI
# 
# 
# correlation <- function(x, indices) {  #define correlation to be the interest data
#   result <- mean(x[indices,1]-x[indices,2])# change the function to check correlation separately
#   return(result)
# }
# library(boot)
# co<-boot(data = data, statistic = correlation, R = 999,sim = "ordinary", stype = "i") #use boot to calculate correlation separately
# boot.ci(boot.out=co,conf=0.95) #cal the confidence interval

#3333333333333333
# data<-read.table("/Users/dijin/Desktop/??????/minipro/4/VAPOR.csv",header = T,sep=",")
# #plot temperature with theoretical 
# plot(data[,1]~data[,2],xlab='theoretical',ylab='temp') #plot the temp against theoretical
# lm(data[,1]~data[,2])   #fitting linear model
# abline(lm(data[,1]~data[,2])) #draw relate line
# #plot temperature with experiment
# plot(data[,1]~data[,3],xlab='experiment',ylab='temp') #plot the data temp against experiment
# lm(data[,1]~data[,3])   #fitting linear model
# abline(lm(data[,1]~data[,3])) #draw relate line


#plot
data<-read.table("/Users/dijin/Desktop/??????/minipro/4/VAPOR.csv",header = T,sep=",")
plot(data[,2]~data[,3],xlab='theoretical',ylab='experimental') #plot the theretical against experimental
lm(data[,2]~data[,3])   #fitting linear model
abline(lm(data[,2]~data[,3])) #draw relate line

theoretical<-data[,2]
experimental<-data[,3] 
boxplot(theoretical,experiment,names=c("theoretical","experimental"),range=1.5) #generate sidebyside boxplot \

#correlation
correlation <- function(x, indices) {  #define correlation to be the interest data
  result <- cor(x[indices,2],x[indices,3])# change the function to check correlation separately
  return(result)
}
library(boot)
co<-boot(data = data, statistic = correlation, R = 999,
         sim = "ordinary", stype = "i") #use boot to calculate correlation separately
boot.ci(boot.out=co,conf=0.95) #cal the confidence interval


# t test
diff <- data[,2] - data[,3] # for one sample t test
t.test(diff, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE)
#verify data come from normal distribution
npar.resample <- function(x){  # resample and record mean 
  xstar <- sample(x, length(x), replace = T)
  median.star <- mean(xstar)
  return(median.star)
}
mean.boot.dist <- replicate(1000, npar.resample(experimental)) # see distribution of resample
hist(mean.boot.dist)
qqnorm(mean.boot.dist)

#boot
my.mean <- function(x, indices) {  #define correlation to be the interest data
  result <- mean(x[indices,1]-x[indices,2])# change the function to check correlation separately
  return(result)
}
two<-cbind(theoretical,experimental)  #make new array
library(boot)
co<-boot(data = two, statistic = my.mean, R = 999,sim = "ordinary", stype = "i") #use boot to separately
boot.ci(boot.out=co,conf=0.95) #cal the confidence interval

# library(boot)
# 
# LSAT <- c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
# GPA <- c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,
#          2.76,2.88,2.96)
# 
# n = length(LSAT)
# Brep = 10000
# 
# xy <- data.frame(cbind(LSAT,GPA))
# xy
# 
# # Bootstrap the Pearson correlation coefficient
# 
# pearson <- function(d,i=c(1:n)){
#   d2 <- d[i,]
#   return(cor(d2$LSAT,d2$GPA))
# }
# bootcorr <- boot(data=xy,statistic=pearson,R=Brep)
# bootcorr  
# boot.ci(bootcorr,conf=.95)
# 
# windows()
# par(mfrow=c(2,1))
# hist(bootcorr$t,main="Bootstrap Pearson Sample Correlation Coefficients")
# plot(ecdf(bootcorr$t),main="ECDF of Bootstrap Correlation Coefficients")
# 
# 
# # Bootstrap the transformed Pearson correlation coefficient
# 
# xihat <- function(dd,i=c(1:n)){
#   dd2 <- dd[i,]
#   return(.5*log((1+cor(dd2$LSAT,dd2$GPA))/(1-cor(dd2$LSAT,dd2$GPA))))
# }
# bootxi <- boot(data=xy,statistic=xihat,R=Brep)
# bootxi  
# boot.ci(bootxi,conf=.95)
# 
# windows()
# par(mfrow=c(2,1))
# hist(bootxi$t,main="Bootstrap Transformed Correlation Coefficients")
# plot(ecdf(bootxi$t),main="ECDF of Bootstrap Transformed Correlation
#      Coefficients")