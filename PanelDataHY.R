install.packages("plm")
library("plm")

# pooled cross section
kielmc<-read_excel("kielmc.xls")
attach(kielmc)
summary(lm(rprice~nearinc,data=subset(kielmc,y81==1)))
summary(lm(rprice~nearinc,data=subset(kielmc,y81==0)))
summary(lm(rprice~nearinc+y81+y81nrinc,data=kielmc))
summary(lm(rprice~nearinc+y81+y81nrinc+age+agesq+intst+land+area+rooms+baths,data=kielmc))

# Examples for Difference-in-Difference-in-Differences
# Suppose Big House and Small House Have different trend
big=(area>2000)
big=as.numeric(big)
summary(lm(rprice~nearinc+y81+y81nrinc+age+agesq+big+I(big*y81)+I(big*nearinc)+I(big*y81nrinc),data=kielmc))

# panel data part
# pooled cross section
airfare<-read_excel("airfare.xls")
attach(airfare)
a=plm(lfare~y98+y99+y00+passen+bmktshr+ldist+ldistsq,model=c("pooling"),data=airfare,index=c("id","year"))
summary(a)

#Fixed Effects Regression
# First Difference
#“Changes” specification, without an intercept
b=plm(lfare~y98+y99+y00+passen+bmktshr+ldist+ldistsq,model=c("fd"),data=airfare,index=c("id","year"))
summary(b)

#“Entity-demeaned” regression model
## Fixed Effect Estimation (demeaned estimation)
c=plm(lfare~y98+y99+y00+passen+bmktshr+ldist+ldistsq,model=c('within'),data=airfare,index=c("id","year"))
summary(c)

#Random effect
d=plm(lfare~y98+y99+y00+passen+bmktshr+ldist+ldistsq,model=c("random"),data=airfare,index=c("id","year"))
summary(d)

#Hausman test
#null hypothesis, random effect model is better than fixed effect,
#as the fixed effect model is inefficient (but still consistent) in this case
phtest(c,d)