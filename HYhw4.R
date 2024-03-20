install.packages("AER")
install.packages("pls")
install.packages("glmnet")
library(AER)
library(readxl)
library(MASS)
library(pls)
library(glmnet)
library(caret)
data("Boston")

#Question1
RDCHEM<-read_excel("rdchem.xls")
attach(RDCHEM)
RDCHEM$sales<-RDCHEM$sales/1000
summary(lm(rdintens~sales+I(sales^2)+profmarg,data = RDCHEM))
noMax<-RDCHEM[sales!=max(sales),]
summary(lm(rdintens~sales+I(sales^2)+profmarg,data=noMax))

summary(rlm(rdintens~sales+I(sales^2)+profmarg,data = RDCHEM))
summary(rlm(rdintens~sales+I(sales^2)+profmarg,data=noMax))

#Question2
attach(Boston)
x<-Boston[,-14]
y<-Boston$medv

maxComponents<-min(ncol(x),nrow(x))

rmse_values<-numeric(maxComponents)

for (i in 1:maxComponents) {
  pcr_model <- pcr(y ~ ., data = x, scale = TRUE, validation = "CV", ncomp = i)
  rmse_values[i] <- RMSEP(pcr_model)$val[1]
}

optimal_Components<-which.min(rmse_values)

final_pcr_model<-pcr(y~.,data=x,scale=TRUE,ncomp=optimal_Components)
summary(final_pcr_model)

loadings<-final_pcr_model$loadings

loadings_nox<-loadings["nox",1]
loadings_age<-loadings["age",1]

#Question3
hprice2<-read_excel("hprice2.xls")
attach(hprice2)
x<-c("crime", "nox", "rooms", "dist", "radial", "proptax", "stratio", "lowstat")
y<-"lprice"

df<-hprice2[,c(x,y)]
set.seed(100)

training.samples <- createDataPartition(df$lprice, p = 0.5, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

x <- model.matrix(lprice~., train.data)[,-1]
y <- train.data$lprice
# Outcome variable
# Start with LASSO and ridge regression

# LASSO regression

cv.lasso <- cv.glmnet(x, y, alpha = 1,standardize = T)
cv.lasso$lambda.min

lasso.model <- glmnet(x, y, alpha = 1, lambda = cv.lasso$lambda.min,standardize = T)

x.test <- model.matrix(lprice ~., test.data)[,-1]
predictions <- as.vector(predict(lasso.model,x.test))

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$lprice)
)

# Ridge regression
cv.ridge <- cv.glmnet(x, y, alpha = 0,standardize = T)
cv.ridge$lambda.min

ridge.model <- glmnet(x, y, alpha = 0, lambda = cv.ridge$lambda.min,standardize = T)

predictions <- as.vector(predict(ridge.model,x.test))
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$lprice)
)

# Elastic Net

elastic.model <- train(
  lprice ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

elastic.model

elastic.model$bestTune

coef(elastic.model$finalModel, elastic.model$bestTune$lambda)

predictions <-  as.vector(predict(elastic.model,x.test))
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$lprice)
)