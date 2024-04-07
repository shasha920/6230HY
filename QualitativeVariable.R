library(readxl)
# Simulation
# Regression with one dummy variable
e=rnorm(500,0,1)
x=rnorm(500,0,1)
d=rbinom(500,1,0.5)
beta0=1
delta0=5
beta1=1
y=beta0+delta0*d+beta1*x+e
plot(x,y)
abline(lm(y[d==0]~x[d==0]),col="pink")
abline(lm(y[d==1]~x[d==1]),col='green')
summary(lm(y~d+x))

# Regression with 2 categories
wage1<-read_xls('wage1.xls')
attach(wage1)
summary(lm(wage~female+educ+exper+tenure))
# beta0hat = -1.56794
# delta0hat = -1.81085

male=1-female
summary(lm(wage~female+male+educ+exper+tenure))
summary(lm(wage~male+educ+exper+tenure))

gpa1<-read_xls('gpa1.xls')
attach(gpa1)
summary(lm(colGPA~business+engineer))
# 0.05149
# -0.14083

rest=1-business-engineer
summary(lm(colGPA~business+rest))
#0.1923
#0.1408

## Regression with interaction of dummy variables
attach(wage1)
summary(lm(wage~female+married+I(female*married)))
#this lm results:
#                    Estimate 
#(Intercept)           5.1680     
#female               -0.5564    
#married               2.8150     
#I(female * married)  -2.8607   
# our base group here is single male(no married male)
# average wage for this group is 5.168
# average wage for single female is 5.168(single male)-0.5564(female)=4.6116
# average wage for married male is 5.168(single male)+2.8150(married)=7.983
# average wage for married female is 5.1680(single male)-0.5564(female)+2.8150(married)-2.8607((female * married)=4.5659
marrmale = (1-female)*married
marrfemale = female*married
singfemale = female*(1-married)

summary(lm(wage~marrmale+marrfemale+singfemale))
#this lm results:
#            Estimate 
#(Intercept)   5.1680     
#marrmale      2.8150   
#marrfemale   -0.6021     
#singfemale   -0.5564     
# our base group here is single male
# average wage for this group is 5.168
# average wage for single female is 5.168(single male)-0.5564(singfemale)=4.6116
# average wage for married male is 5.168(single male)+2.8150(marrmale)=7.983
# avefage wage for married female is 5.1680(single male)-0.6021(marrfemale)=4.5659

# Question 1
beauty<-read_xls('beauty.xls')
attach(beauty)
#(a)
sum(abvavg*female)/sum(female)

sum(abvavg*(1-female))/sum((1-female))
# (b)
summary(lm(abvavg~female))
# (c) and (d)
summary(lm(wage~belavg+abvavg,data = subset(beauty,female==1)))# only female observation
summary(lm(lwage~belavg+abvavg,data = subset(beauty,female==0)))
# (e)
summary(a<- lm(lwage~belavg+abvavg+educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service,data=subset(beauty,female==1)))

summary(a<- lm(lwage~belavg+abvavg+educ+exper+expersq+union+goodhlth+black+married+south+bigcity+smllcity+service,data=subset(beauty,female==0)))

# Question 2
# For loanapp data, it contains lots of missing value. 
# Usually people use . in excel to represent missing value
# However, R read . as one type of input.
# In order to tell R that . is missing value, we need to specify it as NA
# This can be done when we import data
loanapp <- read_xls('loanapp.xls')
attach(loanapp)
#(b)
summary(lm(approve~white))
# (c)
summary(lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp))
#find column's name
colnames(loanapp)
# (d)
summary(lm(approve~white+hrat+obrat+I(white*obrat)+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp))

# Question 3
# (a)
jtrain2 <- read_xls('jtrain2.xls')
attach(jtrain2)
sum(train)/length(train)
jtrain3 <- read_xls('jtrain3.xls')
attach(jtrain3)
sum(train)/length(train)
# (b)
attach(jtrain2)
summary(lm(re78~train))
# (c)
summary(lm(re78~train+re74+re75+educ+age+black+hisp))
# (d)
attach(jtrain3)
summary(lm(re78~train))

summary(lm(re78~train+re74+re75+educ+age+black+hisp))
# Linear Probability vs. Logistic regression
mroz <- read_xls('mroz.xls')
attach(mroz)
lpm<-lm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6)
summary(lpm)

# percentage of correct prediction
# when yhat > 0.5 => we predict y = 1
# when yhat <= 0.5 => we predict y = 0
yhat<-lpm$fitted.values
ytilde=matrix(0,length(yhat),1)
for(i in 1:length(yhat)){
  ytilde[i]<-if(yhat[i]>0.5)1 else 0
}
pcp=matrix(0,length(yhat),1)
for(i in 1:length(yhat)){
  pcp[i]<-if(ytilde[i]==inlf[i])1 else 0
}

sum(pcp)/length(yhat)

# Logit Model
logitm <-glm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,family = "binomial")
summary(logitm)

yhat<-logitm$fitted.values
ytilde=matrix(0,length(yhat),1)
for(i in 1:length(yhat)){
  ytilde[i]<-if(yhat[i]>0.5)1 else 0
}
pcp=matrix(0,length(yhat),1)
for(i in 1:length(yhat)){
  pcp[i]<-if(ytilde[i]==inlf[i])1 else 0
}

sum(pcp)/length(yhat)