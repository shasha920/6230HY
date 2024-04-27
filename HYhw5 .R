#Question3
injury<-read_excel("injury.xls")
attach(injury)
summary(lm(log(durat)~afchnge+highearn+afchnge*highearn))
summary(lm(log(durat)~highearn+afchnge*highearn))
summary(lm(log(durat)~afchnge+afchnge*highearn))

#Question4
rental<-read_excel("rental.xls")
attach(rental)
summary(lm(log(rent)~y90+log(pop)+log(avginc)+pctstu))
summary(lm(log(rent) ~ y90 + log(pop) + log(avginc) + pctstu + pctstu * y90))
c=plm(log(rent) ~ y90 + log(pop) + log(avginc) + pctstu + pctstu * y90,model=c('within'),data=rental,index=c("city","year"))
summary(c)