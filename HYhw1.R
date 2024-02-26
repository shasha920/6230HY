library(readxl)
#question4
CEOSAL2<-read_excel("CEOSAL2.xls")
summary(lm(log(CEOSAL2$salary)~CEOSAL2$ceoten))

#question5
BWGHT1<-read_excel("BWGHT1.xls")
summary(lm(BWGHT1$bwght~BWGHT1$cigs+BWGHT1$faminc))
summary(lm(BWGHT1$bwght~BWGHT1$cigs))

#question6
HPRICE1<-read_excel("HPRICE1.xls")
summary(lm(HPRICE1$price~HPRICE1$sqrft+HPRICE1$bdrms))