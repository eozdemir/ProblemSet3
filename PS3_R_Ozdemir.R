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
#Create the coefficient matrix by running 1000 times
coefficient.m<- lapply(1:1000, function(i) summary(lm(yres[,i]~my.array[,1,i]+my.array[,2,i]+my.array[,3,i]+my.array[,4,i]+my.array[,5,i]))$coefficient[,1])
coefficient.m<- unlist(coefficient.m)
coefficient.m<- t(array(coefficient.m, dim=c(6,1000)))
dim(coefficient.m)
