#Question2
bwght1<-read_excel("bwght1.xls")
attach(bwght1)

summary(lm(bwght~faminc+I(faminc^2)+male+white+I(male*white)))

turning_point <- -0.240757 / (2 * -0.002157)

coef_male <- 3.622585
coef_white <- 5.027034
coef_male_white <- -0.496444

difference <- coef_male + coef_white + coef_male_white

print(difference)

coef_male_white <- -0.496444

std_error_male_white <- 2.631817 

df <- 1382

t_value <- coef_male_white / std_error_male_white

p_value <- 2 * pt(-abs(t_value), df)

print(paste("t-value:", t_value))
print(paste("p-value:", p_value))

#Question 3
RENTAL<-read_excel("rental.xls")
attach(RENTAL)

RENTAL$rentperc <- (RENTAL$rnthsg / RENTAL$tothsg) * 100
summary(lm(log(rent) ~ log(pop) + log(avginc) + log(enroll) + rentperc, data = RENTAL))

model<-lm(log(rent) ~ log(pop) + log(avginc) + log(enroll) + rentperc, data = RENTAL)
conf_int_beta3 <- confint(model)["log(enroll)",]

#Question 4
NBASAL<-read_excel("nbasal.xls")
attach(NBASAL)

model<- lm(NBASAL$wage ~ NBASAL$exper + NBASAL$points + NBASAL$rebounds + NBASAL$assists, data = NBASAL)

summary(model)

beta3_usual <- coef(model)["NBASAL$rebounds"]
se_beta3_usual <- summary(model)$coefficients["NBASAL$rebounds", "Std. Error"]

t_stat_usual <- beta3_usual / se_beta3_usual

p_value_usual <- 2 * pt(-abs(t_stat_usual), df = length(model$residuals) - length(model$coefficients))

p_value_usual

model_robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))

p_value_robust <- model_robust[4, "Pr(>|t|)"]

p_value_robust

bp_test <- bptest(model)
p_value_bp <- bp_test[["p.value"]]
print(p_value_bp)

res2=model$residuals^2
yhat=model$fitted.values

w1<-lm(res2~yhat+I(yhat^2))

linearHypothesis(w1,c("yhat=0","I(yhat^2)=0"),white.adjust = FALSE)
linearHypothesis(w1,c("yhat=0","I(yhat^2)=0"),white.adjust = TRUE)

#Question 5
CPS91<-read_excel("cps91.xls")
attach(CPS91)

model<-lm(faminc~huseduc+educ+husage+age+black+hispanic)
summary(model)

quantile_model05 <- rq(faminc ~ huseduc + educ + husage + age + black + hispanic, data = CPS91, tau = 0.5, ,method = "fn")
summary(quantile_model05)

quantile_model01 <- rq(faminc ~ huseduc + educ + husage + age + black + hispanic, data = CPS91, tau = 0.1, ,method = "fn")
summary(quantile_model01)

anova.rq(quantile_model05, quantile_model01)
anova.rq(quantile_model05, quantile_model01,se="ker")