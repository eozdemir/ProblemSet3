#Problem Set 3
rm(list = ls()) #clear the workspace
install.packages(c("plyr", "doMC", "multicore", "foreach"))
library(plyr)
library(doMC)
library(multicore)

## Section A: Sampling distributions and p-values

## 1) Create an array with dim=c(20,5,1000) and filled it with random data
my.array <- array(data = rnorm(n=100*1000), dim=c(20,5, 1000)) #array with random data
dim(my.array) #has right dimensions
str(my.array) #understand how it looks like

## 2) Make a function to create Y values that are the linear combination of the X's plus normally distributed error. 
Beta <- matrix(c(1,2,0,4,0), ncol=1)
Beta
yfun<- function(x, Beta){ 
  y = (x%*%Beta)+rnorm(n=length(x%*%Beta))
}
yresults <- aaply(.data=my.array, .margins=3, .fun=yfun, Beta=Beta) #Runs the function for the third dimension. 
yres <- t(yresults) #Transpose the y values created to get the right dimensions
dim(yres) #Check output is a 20 by 1000 array
str(yres)

## 3) Run 1000 regressions. Output a 1000 by 6 matrix of estimated regression coefficients
#Create a list by running regression 1000 times on 5 variables in our array 
#Function runs the regressions and gets the summary statistics by taking the coefficients
coefficient.m<- laply(1:1000, function(i) coef(lm(yres[,i]~my.array[,1,i]+my.array[,2,i]+my.array[,3,i]+my.array[,4,i]+my.array[,5,i]))) #reach to coefficient matrix of lm
str(coefficient.m)                  
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
t.stats<- laply(1:1000, function(i) summary(lm(yres[,i]~my.array[,1,i]+my.array[,2,i]+my.array[,3,i]+my.array[,4,i]+my.array[,5,i]))$coefficient[,3]) #i don't know if there is any other way of getting t statistics easily, i found this ($coefficient[,3]) online
t.stats<- unlist(t.stats) #same process
t.stats<- t(array(t.stats, dim=c(6, 1000)))
dim(t.stats)
head(t.stats)

## 6) Calculate how many t-statistics are statistically significant for each variable
#We have 20 observations, and 5 covariates. This gives us 14 degrees of freedom. We need to calculate the values of a t-distribution by using the 0.975 quantile (since we want significance p<=0.05) by using the qt() function. 
t.value<- qt(0.975,14) #the critical value
significance<- abs(t.stats)>abs(t.value) #if our statistics is larger than the critical value, it is significant
length(which(significance)) #how many of them are significant
t.stat.df<- data.frame(t.stats) #Turned into df to reach each column seperately
significance1<- abs(t.stat.df$X1)>abs(t.value)
s1<- length(which(significance1)) 
s1 #number of significant t-values for intercept
significance2<- abs(t.stat.df$X2)>abs(t.value)
s2<- length(which(significance2)) 
s2 #number of significant t-values for 1st coefficient
significance3<- abs(t.stat.df$X3)>abs(t.value)
s3<- length(which(significance3)) 
s3 ##number of significant t-values for 2nd coefficient
significance4<- abs(t.stat.df$X4)>abs(t.value)
s4<- length(which(significance4)) 
s4 #number of significant t-values for 3rd coefficient
significance5<- abs(t.stat.df$X5)>abs(t.value)
s5<- length(which(significance5)) 
s5 #number of significant t-values for 4th coefficient
significance6<- abs(t.stat.df$X6)>abs(t.value)
s6<- length(which(significance6)) 
s6 #number of significant t-values for 5th coefficient

## 7) Re-run the code in parallel and estimate how much time is saved.
# Used one of laply() functions to see if parallel makes a difference
system.time(code1 <- laply(1:1000, function(i) coef(lm(yres[,i]~my.array[,1,i]+my.array[,2,i]+my.array[,3,i]+my.array[,4,i]+my.array[,5,i]))))
registerDoMC(cores=8) #Creates 8 (his) instances of R and divides amongst, need before parallel
system.time(code2 <- laply(1:1000, function(i) coef(lm(yres[,i]~my.array[,1,i]+my.array[,2,i]+my.array[,3,i]+my.array[,4,i]+my.array[,5,i]))))
##Parallel is not really different in my case, I also tried with 4 cores.

## Section B: Calculating Fit Statistics

## 1) 
##Read the incumbent data in first
##I downloaded it to my computer since the url linking function gave warning 
getwd()
setwd("/Users/elifozdemir/Desktop/WashU 1.2/R Programming/Problem Sets/ProblemSet3")
inc.data<- read.table("incumbents.txt", header=TRUE, sep="\t", row.names=1, stringsAsFactors=FALSE) 
head(inc.data)
n<- nrow(inc.data)
S <- sample (1:n, n/2) #randomly split to 2
training<- inc.data[S,] #first part is the training set
test<- inc.data[-S,] #rest is the test set
head(training)
head(test)
##Build 3 statistical models where vote share is the dependent variable
mod1<- lm(training$voteshare~training$incspend)
mod2<- lm(training$voteshare~training$unemployed)
mod3<- lm(training$voteshare~training$seniority)
##see the output of the regressions
summary(mod1)
summary(mod2)
summary(mod3)
##use these models to make predictions in test data
##I had to drop one of the observations in test data because two datasets' number of observations didn't match
nrow(test)
i<- sample(nrow(test), 1) #random observation to drop :(
test<- test[-i,] #not lucky i
nrow(test) #check
pred1<- (predict(mod1, newdata=test))
pred2<- (predict(mod2, newdata=test))
pred3<- (predict(mod3, newdata=test))
summary(pred1)
summary(pred2)
summary(pred3)

## 2) Write a function for fit statistics matrix
##First I need a vector of naive forecasts
mod.naive<- lm(training$voteshare~1) #A regression with just a constant as you suggested
#I don't know why but it only worked with 1 as the constant
pred.naive<- predict(mod.naive, newdata=test) #predict on test dataset
is.vector(pred.naive) #check if it is vector as the question asks
##Second I need a matrix of predictors where each column is one of our models
pred.mat<- cbind(pred1,pred2,pred3)
is.matrix(pred.mat) #check if it is a matrix as the question asks
##Lastly we need a vector of true observed outcomes (y)
outcomes<- training$voteshare 
is.vector(outcomes) #check if it is vector as the question asks
##Before going into the function, I need to calculate the error terms etc.
abs.err<- abs(pred.mat-outcomes) #e_i
abs.per.err<- abs.err/(abs(outcomes)*100) #a_i
base<- abs(pred.naive-outcomes) #b_i
N<- nrow(test)

#The function
fit.stats<- function(y=outcomes, P=pred.mat, r=pred.naive){
  RMSE<-  laply(1:N, function(i) sqrt(sum(abs.err*abs.err, na.rm=TRUE)/N))
  MAD<-   laply(1:N, function(i) median(abs.err, na.rm=TRUE))
  RMSLE<- laply(1:N, function(i) sqrt(sum((log(pred.mat+1)-log(outcomes+1))^2, na.rm=TRUE)/2))
  MAPE<-  laply(1:N, function(i) sum(abs.per.err, na.rm=TRUE)/N)
  MEAPE<- laply(1:N, function(i) median(abs.per.err, na.rm=TRUE))
  MRAE<-  laply(1:N, function(i) median(abs.err/base, na.rm=TRUE))
  output<- cbind(RMSE,MAD,RMSLE,MAPE,MEAPE,MRAE)
  return(output)
}
head(fit.stats()) #first 6 rows of the matrix I stored the statistics in column-wise for each observation in our dataset

