#Problem Set 3
rm(list = ls()) #clear the workspace
install.packages("plyr")
library(plyr)
library(abind)
library(foreign)

## Section A: Sampling distributions and p-values

## 1) Create an array with dim=c(20,5,1000) and filled it with random data
my.array <- array(data = rnorm(n=100*1000), dim=c(20,5, 1000)) #array with random data
dim(my.array) #has right dimensions
head(my.array) #see how it looks like

## 2) Make a function to create Y values that are the linear combination of the X's plus normally distributed error. 
Beta <- matrix(c(1,2,0,4,0), ncol=1)
Beta
yfun<- function(x, Beta){ 
  y = (x%*%Beta)+rnorm(n=length(x%*%Beta))
}
yresults <- aaply(.data=my.array, .margins=3, .fun=yfun, Beta=Beta) #Runs the function for the third dimension. 
yres <- t(yresults) #Transpose the y values created to get the right dimensions
dim(yres) #Check output is a 20 by 1000 array

## 3) Run 1000 regressions. Output a 1000 by 6 matrix of estimated regression coefficients
#Create a list by running 1000 times
#Function runs the regressions and gets the summary statistics by taking the coefficients
coefficient.m<- lapply(1:1000, function(i) summary(lm(yres[,i]~my.array[,1,i]+my.array[,2,i]+my.array[,3,i]+my.array[,4,i]+my.array[,5,i]))$coefficient[,1])
coefficient.m<- unlist(coefficient.m) #unlist before transpose
coefficient.m<- t(array(coefficient.m, dim=c(6,1000))) #make an array and transpose to get right dimensions
dim(coefficient.m)
head(coefficient.m)

## 4) Create a density plot for each of the 6 coefficients. Each should be estimated 1000 times
par(mfrow=c(3,2), mar=c(2,2,1,0.25))
plot(density(coefficient.m[,1]), xlim=c(min(coefficient.m[,1]), max(coefficient.m[,1])), main="1st coefficient")
plot(density(coefficient.m[,2]), xlim=c(min(coefficient.m[,2]), max(coefficient.m[,2])), main="2nd coefficient")
plot(density(coefficient.m[,3]), xlim=c(min(coefficient.m[,3]), max(coefficient.m[,3])), main="3rd coefficient")
plot(density(coefficient.m[,4]), xlim=c(min(coefficient.m[,4]), max(coefficient.m[,4])), main="4th coefficient")
plot(density(coefficient.m[,5]), xlim=c(min(coefficient.m[,5]), max(coefficient.m[,5])), main="5th coefficient")
plot(density(coefficient.m[,6]), xlim=c(min(coefficient.m[,6]), max(coefficient.m[,6])), main="6th coefficient")

## 5) Collect t-statistics for all 1000 regressions for all 6 coefficients
#Use the same lapply function to get the t-statistics of regressions
t.stats<- lapply(1:1000, function(i) summary(lm(yres[,i]~my.array[,1,i]+my.array[,2,i]+my.array[,3,i]+my.array[,4,i]+my.array[,5,i]))$coefficient[,3])
t.stats<- unlist(t.stats) #same process
t.stats<- t(array(t.stats, dim=c(6, 1000)))
dim(t.stats)
head(t.stats)

## 6) Calculate how many t-statistics are statistically significant for each variable
#We have 20 observations, and 5 covariates. This gives us 14 degrees of freedom. We need to calculate the values of a t-distribution by using the 0.975 quantile (since we want significance p<=0.05) by using the qt() function. 
t.value<- qt(0.975,14)
significance<- abs(t.stats)>abs(t.value)
length(which(significance))
