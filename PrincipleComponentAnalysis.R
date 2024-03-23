install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("pls")

library("corrr")
library("ggcorrplot")
library("FactoMineR")
library("factoextra")
library("pls")
library(readxl)


#Principle Component Analysis(PCA)
#Simulation
set.seed(101)
N=1000
x=rchisq(N,2)
y=rnorm(N,0,2)+x
z=rnorm(N,0,1)+x
data=data.frame(x,y,z)
corr_matrix<-cor(data)

data.pca<-princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[,1:3]
fviz_pca_var(data.pca,col.var = "pink")

#Real data set(mtcars)
data=mtcars

a<-pcr(hp~mpg+disp+drat+wt+qsec,data=mtcars,scale=TRUE,validation="CV")

summary(a)

a$coefficients

a$scores

summary(lm(data$hp~a$scores))

forecasting=a$fitted.values

validationplot(a)
validationplot(a,val.type = "MSEP")
validationplot(a,val.type = "R2")

#Real data set(hprice2)
hprice2<-read_excel("hprice2.xls")
attach(hprice2)

a<-pcr(lprice~crime+nox+rooms+dist+radial+proptax+stratio+lowstat,scale=TRUE,validation="CV")

summary(a)

a$coefficients

a$scores

summary(lm(lprice~a$scores))

forecasting=a$fitted.values

validationplot(a)
validationplot(a,val.type = "MSEP")
validationplot(a,val.type = "R2")