install.packages("AER")
library(AER)
library(readxl)

#question1
vote1<-read_excel("vote1.xls")
attach(vote1)

summary(a<-lm(voteA~lexpendA+lexpendB+prtystrA))

linearHypothesis(a,c("lexpendA=-lexpendB"))

#question2
wage2<-read_excel("wage2.xls")
attach(wage2)

summary(a<-lm(wage~educ+exper+tenure))

linearHypothesis(a,c("exper=tenure"))

linearHypothesis(a,c("educ=0", "exper=0", "tenure=0"))

#question3
sleep75<-read_excel("sleep75.xls")
attach(sleep75)

summary(a<-lm(sleep~totwrk+educ+age))

linearHypothesis(a,c("educ=0","age=0"))

linearHypothesis(a,c("age=0"))

#question4
gpa2<-read_excel("gpa2.xls")
attach(gpa2)

summary(a<-lm(sat~hsize+hsizesq))

19.814/(2*2.131)

summary(a<-lm(log(sat)~hsize+hsizesq))

0.0196029/(2*0.0020872)

#question5
attach(vote1)

pe = expendA*expendB

summary(a<-lm(voteA~prtystrA+expendA+expendB+pe))

linearHypothesis(a,c("pe=0"))

linearHypothesis(a,c("expendA=0","expendB=0","pe=0"))

-3.172e-02-6.630e-06*3.828e-02
3.828e-02-6.630e-06*-3.172e-02

mean(expendA)