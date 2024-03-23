install.packages("quantreg")

library("quantreg")

# RESET
# Proxy Variable example 1 (wage2)
library(readxl)
wage2<-read_excel("wage2.xls")
attach(wage2)
summary(lm(lwage~educ+exper+tenure+married+south+urban+black))
# missing factor is talent
# Omitted variable has positive effect on lwage
# Correlation between omitted variable and educ are positive
# We will get positive bias

summary(lm(lwage~I(educ/100)+exper+tenure+married+south+urban+black+IQ))
# we are using IQ as a proxy for talent

summary(lm(lwage~educ+exper+tenure+married+south+urban+black+IQ+I(IQ*educ)))

# Proxy variable example 2 (crime 2)
crime2<-read_excel("crime2.xls")
attach(crime2)

summary(lm(lcrmrte[d87==1]~unem[d87==1]+llawexpc[d87==1]))
summary(lm(lcrmrte[d87==1]~unem[d87==1]+llawexpc[d87==1]+lcrmrte[d87==0]))

# Measurement Error: Simulation
# True variable is xstar
# Observed variable is x
# error is e_x = xstar-x
# simple regression y = beta_0 + beta_1xstar + e
# Measurement error in X: The case where observed value and measurement error are not correlated
II=10000
N=100
beta0vec=matrix(0,II,1)
beta1vec=matrix(0,II,1)
for(i in 1:II){
  x=rnorm(N,0,1)
  e_x=rnorm(N,0,1)
  e=rnorm(N,0,1)
  xstar=x+e_x
  beta0=1
  beta1=2
  y=beta0+beta1*xstar+e
  beta0vec[i]=summary(lm(y~x))$coefficients[1,1]
  beta1vec[i]=summary(lm(y~x))$coefficients[2,1]
}

mean(beta0vec)
mean(beta1vec)

# Measurement error in X: The case where observed value and measurement error are correlated
II=10000
N=100
beta0vec=matrix(0,II,1)
beta1vec=matrix(0,II,1)
for(i in 1:II){
  xstar=rnorm(N,0,100)# signal
  e_x=rnorm(N,0,1)# noise
  e=rnorm(N,0,1)
  x=xstar+e_x
  beta0=1
  beta1=2
  y=beta0+beta1*xstar+e
  beta0vec[i]=summary(lm(y~x))$coefficients[1,1]
  beta1vec[i]=summary(lm(y~x))$coefficients[2,1]
}

mean(beta0vec)
mean(beta1vec)

# Quantile Regression (ceosal2)
ceosal2<-read_excel("ceosal2.xls")
attach(ceosal2)

summary(lm(salary~sales))

summary(rq(salary~sales,tau=0.5,method = "fn"))

plot(sales,salary)
abline(lm(salary~sales))
abline(rq(salary~sales,tau=0.5,method = "fn"),col="pink")
abline(rq(salary~sales,tau=0.2,method = "fn"),col="yellow")
abline(rq(salary~sales,tau=0.8,method = "fn"),col="blue")

# Quantile Regression (X401ksubs)
X401ksubs<-read_excel("X401ksubs.xls")
attach(X401ksubs)

summary(lm(nettfa~inc+incsq+age+agesq+male+e401k))

summary(rq(nettfa~inc+incsq+age+agesq+male+e401k,tau=0.5,method = "fn"))

summary(rq(nettfa~inc+incsq+age+agesq+male+e401k,tau=0.2,method = "fn"))

# Bootstrap standard error (See it as robust standard error)
summary(rq(nettfa~inc+incsq+age+agesq+male+e401k,tau=0.5,method = "fn"),se="boot")

# Joint test
a=rq(nettfa~inc+incsq+age+agesq+male+e401k,tau=0.5,method = "fn")
b=rq(nettfa~inc+incsq+male+e401k,tau=0.5,method = "fn")

anova.rq(a,b)
anova.rq(a,b,se="ker")

a=rq(nettfa~inc+incsq+age+agesq+male+e401k,tau=0.5,method = "fn")
b=rq(nettfa~inc+incsq+age+agesq+male+e401k,tau=0.2,method = "fn")

anova.rq(a,b)
anova.rq(a,b,se="ker")