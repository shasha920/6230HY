install.packages("sandwich")

library("sandwich")
library(readxl)
library(car)

# Simulation: Homoskedasticity vs. Heteroskedasticity, what is wrong?
# Homoskedasticity case
set.seed(100)
II=10000
N=30
beta0=1
beta1=2
beta0mat=matrix(0,II,1)
beta1mat=matrix(0,II,1)
varbeta0mat=matrix(0,II,1)
varbeta1mat=matrix(0,II,1)
for(i in 1:II){
  x=rnorm(N,0,2)
  e=rnorm(N,0,1)
  y=beta0+beta1*x+e
  beta0mat[i]=summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~x))$coefficients[2,1]
  varbeta0mat[i]=summary(lm(y~x))$coefficients[1,2]
  varbeta1mat[i]=summary(lm(y~x))$coefficients[2,2]
}

sqrt(var(beta0mat))# sd of beta0hat
mean(varbeta0mat)# avearge of se(beta0hat)
sqrt(var(beta1mat)) # sd of beta1hat
mean(varbeta1mat)# average of se(beta1hat)

# Heteroskedasticity case
set.seed(100)
II=10000
N=30
beta0=1
beta1=2
beta0mat=matrix(0,II,1)
beta1mat=matrix(0,II,1)
varbeta0mat=matrix(0,II,1)
varbeta1mat=matrix(0,II,1)
for(i in 1:II){
  x=rnorm(N,0,2)
  y=matrix(0,N,1)
  for(j in 1:N){
    y[j]=beta0+beta1*x[j]+rnorm(1,0,(abs(x[j])))
  }
  beta0mat[i]=summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~x))$coefficients[2,1]
  varbeta0mat[i]=summary(lm(y~x))$coefficients[1,2]
  varbeta1mat[i]=summary(lm(y~x))$coefficients[2,2]
}

sqrt(var(beta0mat))
mean(varbeta0mat)
sqrt(var(beta1mat))
mean(varbeta1mat)

# Heteroskedasticity case (robust inference)
set.seed(100)
II=10000
N=300
beta0=1
beta1=2
beta0mat=matrix(0,II,1)
beta1mat=matrix(0,II,1)
varbeta0mat=matrix(0,II,1)
varbeta1mat=matrix(0,II,1)
for(i in 1:II){
  x=rnorm(N,0,2)
  y=matrix(0,N,1)
  for(j in 1:N){
    y[j]=beta0+beta1*x[j]+rnorm(1,0,(abs(x[j])))
  }
  beta0mat[i]=summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~x))$coefficients[2,1]
  varbeta0mat[i]=coeftest(lm(y~x),vcov.=vcovHC(lm(y~x),type="HC1"))[1,2]
  varbeta1mat[i]=coeftest(lm(y~x),vcov.=vcovHC(lm(y~x),type="HC1"))[2,2]
}

sqrt(var(beta0mat))
mean(varbeta0mat)
sqrt(var(beta1mat))
mean(varbeta1mat)

# Difference between conventional standard error and robust standard error
# Example 1
gpa1<-read_excel("gpa1.xls")
attach(gpa1)
m1<-lm(colGPA~hsGPA+ACT+skipped+alcohol+clubs+bgfriend+gradMI,data=gpa1)

summary(m1)

coeftest(m1,vcov.=vcovHC(m1,type="HC1"))

# Example 2
crime1<-read_excel("crime1.xls")
attach(crime1)
a<-lm(narr86~pcnv+avgsen+I(avgsen^2)+ptime86+qemp86+inc86+black+hispan,data=crime1)

summary(a)

coeftest(a,vcov. = vcovHC(a,type="HC1"))

# Robust Joint test
# Our hypothesis test is: whether avgsen has effecr on narr86 or not
linearHypothesis(a,c("avgsen=0","I(avgsen^2)=0"),white.adjust = FALSE)
linearHypothesis(a,c("avgsen=0","I(avgsen^2)=0"),white.adjust = TRUE)

# Consistency and Inefficiency of Robust standard error when the error terms are homoskedastic
s=10
set.seed(s)
N=100
x1=rnorm(N,0,0.5)
beta0=1
beta1=2
e=rnorm(N,0,3)
y=beta0+beta1*x1+e
a=lm(y~x1)
res=a$residuals
sighat=(1/(N-2))*sum(res^2)
se=sighat/(sum((x1-mean(x1))^2))
rse=sum((x1-mean(x1))^2*res^2)/((sum((x1-mean(x1))^2))^2)

set.seed(s)
II=1000
N=100
sem=matrix(0,II,1)
rsem=matrix(0,II,1)
for(i in 1:II){
  x1=rnorm(N,0,0.5)
  beta0=1
  beta1=2
  e=rnorm(N,0,3)
  y=beta0+beta1*x1+e
  a=lm(y~x1)
  res=a$residuals
  sighat=(1/(N-2))*sum(res^2)
  sem[i]=sighat/(sum((x1-mean(x1))^2))
  rsem[i]=sum((x1-mean(x1))^2*res^2)/((sum((x1-mean(x1))^2))^2)
}

sd(sem)
sd(rsem)

# Testing for Heteroskedasticity
# First example
# BP test
m1<-lm(colGPA~hsGPA+ACT+skipped+alcohol+clubs+bgfriend+gradMI,data=gpa1)
bptest(m1)

# White Test
res2=m1$residuals^2
yhat=m1$fitted.values

w1<-lm(res2~yhat+I(yhat^2))

linearHypothesis(w1,c("yhat=0","I(yhat^2)=0"),white.adjust = FALSE)
linearHypothesis(w1,c("yhat=0","I(yhat^2)=0"),white.adjust = TRUE)

# Example 2
a<-lm(narr86~pcnv+avgsen+I(avgsen^2)+ptime86+qemp86+inc86+black+hispan,data = crime1)
bptest(a)

# White Test
res2=a$residuals^2
yhat=a$fitted.values

w1<-lm(res2~yhat+I(yhat^2))

linearHypothesis(w1,c("yhat=0","I(yhat^2)=0"),white.adjust = FALSE)
linearHypothesis(w1,c("yhat=0","I(yhat^2)=0"),white.adjust = TRUE)