#Problem Set 3
rm(list = ls()) #clear the workspace
install.packages("plyr")
library(plyr)
library(abind)

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
#Create a dataset of covariates and y values by merging multi-dimensional arrays
dataset<- abind(my.array, yres, along=2)
#Use lapply to create a list with 20 by 6 matrices and convert it to dataframe 
my.data<- data.frame(lapply(1:dim(dataset)[3], function(i) dataset[,,i]))
str(my.data)
#Create the coefficient matrix by creating a list where each element contains regression coefficients of 1000 regressions/dataframes in the my.data)
coefficient.m<- matrix(unlist(lapply(1:dim(dataset)[3], function(i) coef(lm(X6~., my.data[i])))), ncol=6, byrow=TRUE)

## 4) 
