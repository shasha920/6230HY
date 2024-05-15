install.packages("AER")
install.packages("sampleSelection")
library(AER)
library(sampleSelection)
library(readxl)

# Simulation Study
# Regression Model
set.seed(1234)
II = 10000
beta1hat = matrix(0,II,1)
for(i in 1:II){
  N = 20
  x1 = rnorm(N,0,3)
  beta0 = 3
  beta1 = 4
  e = rnorm(N,0,30)
  y = beta0 + beta1*x1 + e
  m1 <- lm(y~x1)
  beta1hat[i] <- summary(m1)$coefficients[2,1]
}

mean(beta1hat)

# Linear projection model
set.seed(1234)
II = 10000
corxe = matrix(0,II,1)
beta1hat = matrix(0,II,1)
for(i in 1:II){
  N = 20
  x1 = rnorm(N,0,3)
  beta0 = 3
  beta1 = 4
  e = 3*x1^2 - 2*x1^4
  y = beta0 + beta1*x1 + e
  m2 <- lm(y~x1)
  beta1hat[i] <- summary(m2)$coefficients[2,1]
  corxe[i] = cor(x1,e)
}
mean(corxe)
mean(beta1hat)

# Consistency
set.seed(1234)
N = 50000000
x1 <- rnorm(N,0,3)
beta0 = 3
beta1 = 4
e = 3*x1^2 - 2*x1^4
y = beta0 + beta1*x1 + e
m3 <- lm(y~x1)
summary(m3)$coefficients[2,1]

# Inconsistency
set.seed(1234)
N = 50000000
x1 <- rnorm(N,0,3)
beta0 = 3
beta1 = 4
e = 2*x1+rnorm(N,0,1)
y = beta0 + beta1*x1 + e
m3 <- lm(y~x1)
summary(m3)$coefficients[2,1]

mroz<-read_excel("mroz.xls")
attach(mroz)
summary(lm(lwage~educ,data=mroz[1:428,]))

summary(a <- lm(educ~fatheduc,data=mroz[1:428,]))
educ_fit = a$fitted.values
summary(b <- lm(lwage~educ_fit,data=mroz[1:428,]))

summary(ivreg(lwage~educ|fatheduc,data=mroz[1:428,]))

wage2<-read_excel("wage2.xls")
attach(wage2)

summary(lm(lwage~educ,data=wage2))

summary(a<-lm(educ~sibs,data=wage2))
educ_fit = a$fitted.values
summary(b<-lm(lwage~educ_fit,data=wage2))

summary(ivreg(lwage~educ|sibs+feduc,data=wage2))

# simulation studies for standard errors
# Linear regression
set.seed(100)
II = 10000
N = 300
beta0 = 1
beta1 = 2
beta0mat = matrix(0,II,1)
beta1mat = matrix(0,II,1)
varbeta0mat = matrix(0,II,1)
varbeta1mat = matrix(0,II,1)
for(i in 1:II){
  x = rnorm(N,0,2)
  e = rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta0mat[i]=summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~x))$coefficients[2,1]
  varbeta0mat[i]=summary(lm(y~x))$coefficients[1,2]
  varbeta1mat[i]=summary(lm(y~x))$coefficients[2,2]
}

sqrt(var(beta0mat))
mean(varbeta0mat)
sqrt(var(beta1mat))
mean(varbeta1mat)


# Two stage linear regression (with package)
set.seed(100)
II = 10000
N = 300
beta0 = 1
beta1 = 2
beta0mat = matrix(0,II,1)
beta1mat = matrix(0,II,1)
varbeta0mat = matrix(0,II,1)
varbeta1mat = matrix(0,II,1)
for(i in 1:II){
  z = rnorm(N,0,1)
  e = rnorm(N,0,1)
  x = z + e + rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta0mat[i]=summary(ivreg(y~x|z))$coefficients[1,1]
  beta1mat[i]=summary(ivreg(y~x|z))$coefficients[2,1]
  varbeta0mat[i]=summary(ivreg(y~x|z))$coefficients[1,2]
  varbeta1mat[i]=summary(ivreg(y~x|z))$coefficients[2,2]
}

mean(beta0mat)
mean(beta1mat)

sqrt(var(beta0mat))
mean(varbeta0mat)
sqrt(var(beta1mat))
mean(varbeta1mat)

# Two stage linear regression (manually)
set.seed(100)
II = 10000
N = 300
beta0 = 1
beta1 = 2
beta0mat = matrix(0,II,1)
beta1mat = matrix(0,II,1)
varbeta0mat = matrix(0,II,1)
varbeta1mat = matrix(0,II,1)
for(i in 1:II){
  z = rnorm(N,0,1)
  e = rnorm(N,0,1)
  x = z + e + rnorm(N,0,1)
  y = beta0 + beta1*x + e
  xfit = lm(x~z)$fitted.values
  beta0mat[i]=summary(lm(y~xfit))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~xfit))$coefficients[2,1]
  varbeta0mat[i]=summary(lm(y~xfit))$coefficients[1,2]
  varbeta1mat[i]=summary(lm(y~xfit))$coefficients[2,2]
}

mean(beta0mat)
mean(beta1mat)

sqrt(var(beta0mat))
mean(varbeta0mat)
sqrt(var(beta1mat))
mean(varbeta1mat)

# Example code for multiple instrument or multiple exogenous variable
# Multiple instrument, no exogenous
summary(ivreg(lwage~educ|feduc+sibs,data=wage2))

# Singel instument, some exogenous
summary(ivreg(lwage~educ+exper+married|sibs+exper+married,data=wage2))

# Multiple instument, some exogenous
summary(ivreg(lwage~educ+exper+married|sibs+feduc+exper+married,data=wage2))

# Error in variable
attach(wage2)
# no proxy
summary(lm(lwage~educ+exper+tenure+married+south+urban+black))

# proxy
summary(lm(lwage~educ+exper+tenure+married+south+urban+black+IQ))

# Proxy with Instrument
summary(ivreg(lwage~educ+exper+tenure+married+south+urban+black+IQ|KWW+educ+exper+tenure+married+south+urban+black))

# Control function
summary(lm(lwage~educ+exper+expersq,data=mroz[1:428,]))

b = lm(educ~fatheduc+motheduc+exper+expersq,data=mroz[1:428,])

educ_fit = b$fitted.values
res = b$residuals

# 2SLS
summary(lm(lwage~educ_fit+exper+expersq,data=mroz[1:428,]))

# Control function
summary(lm(lwage~educ+exper+expersq+res,data=mroz[1:428,]))

# Testing for endogeneity can be done by t test in the above result.
# If there are multiple endogenous variables, use F test.

# Testing for overidentification
attach(wage2)
summary(a<- ivreg(lwage~educ+exper+I(exper^2)|feduc+meduc+exper+I(exper^2)))

res_1 = a$residuals

res = lwage-summary(a)$coefficients[1,1]-summary(a)$coefficients[2,1]*educ-summary(a)$coefficients[3,1]*exper-summary(a)$coefficients[4,1]*exper^2

summary(lm(res~feduc+meduc+exper+I(exper^2)))

# Test statistics is nR^2
722*3.29e-05

# Critical value is Chi-square with degree of freedom q, the number of instrument variable minus the number of endoegenous variable
722*3.29e-05>qchisq(0.95,1)

# sample selection
data(Mroz87)

# Linear regression
#lr <- lm(log(wage) ~ educ + city)
#summary(lr)

# Sample selection
# Manually
Probitm <-glm(lfp ~ huswage + kids5 + mtr + fatheduc + educ + city, data=Mroz87,
              family = binomial(link="probit"))
summary(Probitm)

# Calculating inverse mills ratio
imr = dnorm(summary(Probitm)$coefficients[1,1]+summary(Probitm)$coefficients[2,1]*Mroz87$huswage+summary(Probitm)$coefficients[3,1]*Mroz87$kids5+summary(Probitm)$coefficients[4,1]*Mroz87$mtr+summary(Probitm)$coefficients[5,1]*Mroz87$fatheduc+summary(Probitm)$coefficients[6,1]*Mroz87$educ+summary(Probitm)$coefficients[7,1]*Mroz87$city)/pnorm(summary(Probitm)$coefficients[1,1]+summary(Probitm)$coefficients[2,1]*Mroz87$huswage+summary(Probitm)$coefficients[3,1]*Mroz87$kids5+summary(Probitm)$coefficients[4,1]*Mroz87$mtr+summary(Probitm)$coefficients[5,1]*Mroz87$fatheduc+summary(Probitm)$coefficients[6,1]*Mroz87$educ+summary(Probitm)$coefficients[7,1]*Mroz87$city)

# Second step regression
summary(lm(log(wage) ~ educ + city + imr, data=Mroz87, subset=(wage>0)))

# using package
a <- heckit(lfp ~ huswage + kids5 + mtr + fatheduc + educ + city,
            log(wage) ~ educ + city, data=Mroz87)
summary(a)

# Simultaneous equations
# example 1

# Be careful when you import data, lwage contains .
# R read . as a character instead of a number
# So if we leave . in the data, and use lwage as a explanatory variable, R will treat this variable as character variable, not numerical variable
# In other words, R will treat the variable like a dummy variable (different values represent different characteristic)
# We can tell R to replace . as NA as we import data. This will solve the problem
# for hours equation
summary(a<- ivreg(hours~lwage+educ+age+kidslt6+nwifeinc|educ+age+kidslt6+nwifeinc+exper+I(exper^2), data=mroz[1:428,]))
# for lwage equation
summary(b<- ivreg(lwage~hours+educ+exper+I(exper^2)|educ+age+kidslt6+nwifeinc+exper+I(exper^2), data=mroz[1:428,]))

# example 2
# for inf equation
openness<-read_excel("openness.xls")
attach(openness)
summary(a<- ivreg(inf~open+lpcinc|lpcinc+lland, data=openness))

# Weak instrument: Simulation

# Strong instrument
set.seed(100)
II = 10000
N = 200
beta0 = 1
beta1 = 2
beta1mat_s = matrix(0,II,1)
for(i in 1:II){
  z = rnorm(N,0,1)
  e = rnorm(N,0,1)
  x = 1*z + e + rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta1mat_s[i]=summary(ivreg(y~x|z))$coefficients[2,1]
}


# Relatively strong instrument
set.seed(100)
II = 10000
N = 200
beta0 = 1
beta1 = 2
beta1mat_rs = matrix(0,II,1)
for(i in 1:II){
  z = rnorm(N,0,1)
  e = rnorm(N,0,1)
  x = 0.5*z + e + rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta1mat_rs[i]=summary(ivreg(y~x|z))$coefficients[2,1]
}

# Relatively weak instrument
set.seed(100)
II = 10000
N = 200
beta0 = 1
beta1 = 2
beta0mat = matrix(0,II,1)
beta1mat_rw = matrix(0,II,1)
for(i in 1:II){
  z = rnorm(N,0,1)
  e = rnorm(N,0,1)
  x = 0.25*z + e + rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta1mat_rw[i]=summary(ivreg(y~x|z))$coefficients[2,1]
}

# Weak instrument
set.seed(100)
II = 10000
N = 200
beta0 = 1
beta1 = 2
beta0mat = matrix(0,II,1)
beta1mat_w = matrix(0,II,1)
for(i in 1:II){
  z = rnorm(N,0,1)
  e = rnorm(N,0,1)
  x = 0.1*z + e + rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta1mat_w[i]=summary(ivreg(y~x|z))$coefficients[2,1]
}

plot(density(beta1mat_s),col="green",xlim=c(-5,10))
lines(density(beta1mat_rs),col="blue")
lines(density(beta1mat_rw),col="yellow")
lines(density(beta1mat_w),col="red")