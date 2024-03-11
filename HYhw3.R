install.packages("AER")
library(AER)
library(readxl)

#Question2
mlb1<-read_excel("mlb1.xls")
attach(mlb1)
summary(lm(mlb1$salary~mlb1$years+mlb1$gamesyr+mlb1$bavg+mlb1$hrunsyr+mlb1$rbisyr+mlb1$hrunsyr+mlb1$fldperc+mlb1$allstar+mlb1$frstbase+mlb1$scndbase+mlb1$thrdbase+mlb1$shrtstop+mlb1$catcher))

#Question3
SLEEP75<-read_excel("SLEEP75.xls")
attach(SLEEP75)
summary(lm(SLEEP75$sleep~SLEEP75$male+SLEEP75$totwrk+SLEEP75$educ+SLEEP75$age+I(SLEEP75$age^2)+SLEEP75$yngkid))

SLEEP75$female = 1-SLEEP75$male
summary(lm(SLEEP75$sleep~SLEEP75$female+SLEEP75$totwrk+SLEEP75$educ+SLEEP75$age+I(SLEEP75$age^2)+SLEEP75$yngkid))

summary(lm(SLEEP75$sleep~SLEEP75$male+SLEEP75$totwrk+I(SLEEP75$male*SLEEP75$totwrk)+SLEEP75$educ+I(SLEEP75$male*SLEEP75$educ)+SLEEP75$age+I(SLEEP75$male*SLEEP75$age)+I(SLEEP75$age^2)+I(SLEEP75$male*SLEEP75$age^2)+SLEEP75$yngkid+I(SLEEP75$male*SLEEP75$yngkid)))

#Question4
model <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid + male, data = SLEEP75)

residuals <- resid(model)

residuals_male <- residuals[SLEEP75$male == 1]
residuals_female <- residuals[SLEEP75$male == 0]

var_male <- var(residuals_male)
var_female <- var(residuals_female)

if (var_male > var_female) {
  cat("Variance of u is higher for men.")
} else if (var_male < var_female) {
  cat("Variance of u is higher for women.")
} else {
  cat("Variance of u is the same for men and women.")
}

bp_test <- bptest(model)

p_value_bp <- bp_test$p.value

if (p_value_bp < 0.05) {
  cat("The variance of u is statistically different for men and women (p < 0.05).")
} else {
  cat("There is no significant difference in the variance of u between men and women (p >= 0.05).")
}

#Question5
hprice1<-read_excel("hprice1.xls")
attach(hprice1)

model <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)

bp_test <- bptest(model)

if (p_value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis of homoscedasticity. There is evidence of heteroscedasticity in the error term.")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis of homoscedasticity. There is no evidence of heteroscedasticity in the error term.")
}

hprice1$lprice <- log(hprice1$price)
hprice1$llotsize <- log(hprice1$lotsize)
hprice1$lsqrft <- log(hprice1$sqrft)

model <- lm(lprice ~ llotsize + lsqrft + bdrms, data = hprice1)

bp_test <- bptest(model)

model <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)

residuals <- resid(model)

fitted_values <- fitted(model)

data_aux <- data.frame(eb = residuals^2, price = fitted_values, price_squared = fitted_values^2)

model_aux <- lm(eb ~ price + price_squared, data = data_aux)

summary(model_aux)