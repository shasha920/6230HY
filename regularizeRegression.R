install.packages("glmnet")
install.packages("caret")
install.packages("tidyverse")
install.packages("faraway")
install.packages("MASS")

library("glmnet")
library("caret")
library("tidyverse")
library("faraway")
library("MASS")

#LASSO model
set.seed(1233)
data=fat
X<-model.matrix(brozek~age+weight+height+adipos+neck+chest+abdom+hip+thigh+
                  knee+ankle+biceps+forearm+wrist,data=fat)[,-1]
Y<-fat[,"brozek"]

cv.lambda.lasso<-cv.glmnet(x=X,y=Y,alpha=1,standardize=T)

cv.lambda.lasso
plot(cv.lambda.lasso)

#LASSO path
plot(cv.lambda.lasso$glmnet.fit,
     "lambda",label=TRUE)

l.lasso.min<-cv.lambda.lasso$lambda.min
lasso.model<-glmnet(x=X,y=Y,alpha=1,lambda = l.lasso.min,standardize = T)
lasso.model$beta

ols.model<-lm(brozek~age+weight+height+adipos+neck
              +chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data=fat)

summary(ols.model)

#Ridge Model
set.seed(1233)
data=fat
X<-model.matrix(brozek~age+weight+height+adipos+neck+chest+abdom+hip+thigh+
                  knee+ankle+biceps+forearm+wrist,data=fat)[,-1]
Y<-fat[,"brozek"]

cv.lambda.ridge<-cv.glmnet(x=X,y=Y,alpha=0,standardize=T)

cv.lambda.ridge
plot(cv.lambda.ridge)

#Ridge path
plot(cv.lambda.ridge$glmnet.fit,
     "lambda",label=TRUE)

l.ridge.min<-cv.lambda.ridge$lambda.min
ridge.model<-glmnet(x=X, y=Y,alpha=0,lambda=l.ridge.min,standardize = T)
ridge.model$beta

ols.model<-lm(brozek~age+weight+height+adipos+neck
              +chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data=fat)

summary(ols.model)

#Elastic Net
# let's use training data and test data here to compare three different models
# Start with creating training and test data
data=Boston
training.samples<-createDataPartition(Boston$medv,p=0.8,list=FALSE)
train.data<-data[training.samples,]
test.data<-data[-training.samples,]

x<-model.matrix(medv~., train.data)[,-1]
# Outcome variable
y<-train.data$medv

# Start with LASSO and ridge regression
# LASSO regression
cv.lasso<-cv.glmnet(x,y,alpha=1,standardize=T)
cv.lasso$lambda.min

lasso.model<-glmnet(x,y,alpha = 1,lambda = cv.lasso$lambda.min,standardize = T)

x.test<-model.matrix(medv~., test.data)[,-1]
predictions<-as.vector(predict(lasso.model,x.test))
# Model performance metrics
data.frame(
  RMSE=RMSE(predictions,test.data$medv)
)

# Ridge regression
cv.ridge<-cv.glmnet(x,y,alpha=0,standardize=T)
cv.ridge$lambda.min

ridge.model<-glmnet(x,y,alpha=0,lambda = cv.ridge$lambda.min,standardize = T)

predictions<-as.vector(predict(ridge.model,x.test))
# Model performance metrics
data.frame(
  RMSE=RMSE(predictions,test.data$medv)
)

# Elastic Net
elastic.model<-train(
  medv~.,data=train.data,method="glmnet",
  trControl=trainControl("cv",number = 10),
  tuneLength=10
)

elastic.model

elastic.model$bestTune

coef(elastic.model$finalModel,elastic.model$bestTune$lambda)

predctions<-as.vector(predict(elastic.model,x.test))
# Model performance metrics
data.frame(
  RMSE=RMSE(predctions,test.data$medv)
)