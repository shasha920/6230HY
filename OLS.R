# Drawbacks of Correlation
# Only linear relationship
# Import data: data1.xls
# We can import many data sets into R at the same time
# When we want to call a variable in a data set, we need to specify the data set
# for example, data1$x gives us the x variable in data1
library(readxl)
data1<-read_excel("data1.xlsx")
plot(data1$x,data1$y) # this is the basic scatter plot function
# this function adds a line connected the average of y given different values of x
lines(c(mean(data1$y[data1$x==1]),mean(data1$y[data1$x==2]),mean(data1$y[data1$x==3])),col="pink")
cor(data1$x,data1$y)

# This example demonstrates one of the drawbacks in using correlation to find relationship between X and Y
# It only captures the linear relationship. If the relationship is nonlinear, it will find the best linear approximation.
# In this case, the best linear approximation is a (almost) horizontal line, so we get (almost) 0 correlation 
# Confounding factor
# Import data: data2.xls
# In this data set, there are three variables, x, y , and z
# In this example, z is the confounding factor
# This means that both x and y are affected the values of z
# Let's first find the correlation between x and y
data2<-read_excel("data2.xlsx")
cor(data2$x,data2$y)

# The correlation between x and y is very high
# But is this the actual relationship between x and y?
# Or is this just a fake relationship between x and y caused by z?
attach(data2)
# Let's see what's the relationship between x and y after we control the effect from z
plot(x[z==-2],y[z==-2])
cor(x[z==-2],y[z==-2])
plot(x[z==0],y[z==0])
cor(x[z==0],y[z==0])

# The correlations are quite low in both cases
# This implies that there is actually no relationships between x and y after we control the effect from z
# z takes 5 different values, -2,-1,0,1,2
# Can you try to plot the scatter plot of x and y and find their correlation given other values of z?
# This example demonstrates that there is actually no relationship between x and y
# But the correlation measure is corrupted by the confounding factor z
# Population Regression vs. Sample Regression
N=50
x=runif(N,0,5)# x is generated from uniform distribution with min=0, max=5
y=5+0.5*x+rnorm(N,0,100)# y=5+0.5*x+e, where e=error term follows standard normal distribution 
# We can plot the scatter plot of the data and add the theoretical line y=5+0.5*x on it
plot(x,y)
abline(a=5,b=0.5,col="pink")

# The vertical differences between each data point and the straight line represents the error term
# But notice that this pink line is the theoretical line
# In reality, we don't know where the pink line is
# We have to estimate it
# Let's add the fitted line on top of the previous graph
abline(lm(y~x),col="green")

# Simulation: Least Square Method:
# simplest mean only model
N=1000
mu=4
y=rnorm(N,mu,1)
II=1000
mui=matrix(0,II,1)
SSR=matrix(0,II,1)
for(i in 1:II){
  mui[i]=2+i/100
  SSR[i]=sum(y-mui[i]^2)
}
plot(mui,SSR)

# Simple Regression Model
N=1000
e=rnorm(N,0,2)
x=rnorm(N,0,1)
beta0=1
beta1=2
y=beta0+beta1*x+e
beta0i=matrix(0,II,1)
SSR1=matrix(0,II,1)
for(i in 1:II){
  beta0i[i]=-3+i/100
  SSR1[i]=sum((y-beta0i[i]-beta1*x)^2)
}
plot(beta0i,SSR1)

beta1i = matrix(0,II,1)
SSR2 = matrix(0,II,1)
for(i in 1:II){
  beta1i[i] = -3 + i/100
  SSR2[i] = sum((y-beta0-beta1i[i]*x)^2)
}
plot(beta1i,SSR2)

# Some Real Data Examples
# gpa1
# When X is binary
gpa1<-read_excel("gpa1.xls")
summary(lm(gpa1$colGPA~gpa1$business))
# b0=2.99655, b1=0.07577
# What do these two numbers represent?
mean(gpa1$colGPA[gpa1$business==0])
mean(gpa1$colGPA[gpa1$business==1])
mean(gpa1$colGPA[gpa1$business==1])-mean(gpa1$colGPA[gpa1$business==0])
# so b0 is the average of nonbusiness major
# b1 is the average of business major
# this should confirm that regression model is capturing averages
plot(gpa1$business,gpa1$colGPA)
abline(lm(gpa1$colGPA~gpa1$business),col="pink")

# When X is continuous
summary(lm(gpa1$colGPA~gpa1$alcohol))
plot(gpa1$alcohol,gpa1$colGPA)
abline(lm(gpa1$colGPA~gpa1$alcohol),col="pink")

# When X is continuous with strong relationship
summary(lm(gpa1$colGPA~gpa1$hsGPA))
plot(gpa1$hsGPA,gpa1$colGPA)
abline(lm(gpa1$colGPA~gpa1$hsGPA),col="pink")

# Fitted values and Residuals
colGPAhat=lm(gpa1$colGPA~gpa1$hsGPA)$fitted.value
uhat=lm(gpa1$colGPA~gpa1$hsGPA)$residuals

# Simulation: Sampling Distribution
set.seed(100)
II=10000
N=30
beta0=1
beta1=2
beta0mat=matrix(0,II,1)
beta1mat=matrix(0,II,1)
for(i in 1:II){
  x=rnorm(N,0,2)
  e=rnorm(N,0,1)
  y=beta0+beta1*x+e
  beta0mat[i]=summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~x))$coefficients[2,1]
}

hist(beta0mat,100)
hist(beta1mat,100)

# Simulation: Algebraic Properties
# Case 1: error term and x are uncorrelated
set.seed(111)
N=2000
e=rnorm(N,0,5)
x=rnorm(N,0,2)
y=1+2*x+e
cor(x,e)
summary(lm(y~x))
sum(lm(y~x)$residuals)
sum(x*lm(y~x)$residuals)

plot(x,y)
abline(a=1,b=2,col="green")
abline(lm(y~x),col="pink")

# Case 2: error term and x are correlated