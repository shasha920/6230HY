install.packages("AER")
library(AER)
library(readxl)
library(sampleSelection)
#Question 3
wage2<-read_excel("wage2.xls")
attach(wage2)
summary(model <- ivreg(log(wage) ~ educ + exper + tenure | sibs + exper + tenure, data = wage2))

summary(a<-lm(educ ~ sibs + exper + tenure, data = wage2))
fitted_educ <- a$fitted.values
summary(b<-lm(log(wage) ~ fitted_educ + exper + tenure, data = wage2))

summary(a<-lm(educ ~ sibs, data = wage2))
fitted_educ <- a$fitted.values
summary(b<-lm(log(wage) ~ fitted_educ + exper + tenure, data = wage2))

#Question 4
FERTIL2<-read_excel("fertil2.xls")
attach(FERTIL2)
summary(lm(children ~ educ + age + I(age^2), data = FERTIL2))

summary(a<-lm(educ ~ frsthalf + age + I(age^2), data = FERTIL2))

summary(ivreg(children ~ educ + age + I(age^2) | frsthalf + age + I(age^2), data = FERTIL2))

#Question 5
CPS91<-read_excel("cps91.xls")
attach(CPS91)
table(CPS91$inlf)
women_in_labor_force <- sum(CPS91$inlf == 1) 
total_women <- nrow(CPS91)
fraction_in_labor_force <- women_in_labor_force / total_women
fraction_in_labor_force

working_women <- subset(CPS91, inlf == 1) 
summary(lm(lwage ~ educ + exper + I(exper^2) + black + hispanic, data = working_women))

summary(glm(inlf ~ educ + exper + I(exper^2) + black + hispanic + nwifeinc + kidlt6,
                    data = CPS91, family = binomial(link = "probit")))

probit_model <- glm(inlf ~ educ + exper + I(exper^2) + black + hispanic + nwifeinc + kidlt6,
                    data = CPS91, family = binomial(link = "probit"))
predicted_probs <- predict(probit_model, type = "response")
IMR <- dnorm(predicted_probs) / (1 - predicted_probs)
wage_model_with_IMR <- lm(lwage ~ educ + exper + I(exper^2) + black + hispanic + nwifeinc + kidlt6 + IMR,
                          data = CPS91,subset=(lwage>0))
summary(wage_model_with_IMR)