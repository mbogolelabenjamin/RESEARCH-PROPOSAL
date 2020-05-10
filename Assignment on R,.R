rm(list=ls())
setwd("C:/Users/FELIC/Desktop/R_Assignment soln")
#Qn1
bwdata = read.csv('bwt.csv', na=-99, header=TRUE)
View(bwdata)
nrow(bwdata)
ncol(bwdata)
names(bwdata)
bwdata$LOW=factor(bwdata$LOW)
bwdata$RACE = factor(bwdata$RACE)
bwdata$SMOKE=factor(bwdata$SMOKE)
bwdata$PTL=factor(bwdata$PTL)
bwdata$HT=factor(bwdata$HT)
bwdata$UI=factor(bwdata$UI)
bwdata$FTV=factor(bwdata$FTV)
summary(bwdata)
#Qn2
table(bwdata$LOW)
round(100*prop.table(table(bwdata$LOW), margin=NULL)) 
barplot(table(bwdata$LOW),main = "Barchart showing patients having babies with norm and low weight")
#barplot(round(100*prop.table(table(bwdata$LOW), margin=NULL)),main = "Barchart showing patients having babies with norm and low weight")

#Qn3

#Qn4
boxplot(bwdata$AGE, main="BOXPLOT SHOWING THE AGE OF MOTHERS")
median(bwdata$AGE)
IQR(bwdata$AGE)
quantile(bwdata$AGE)
#QN5
hist(bwdata$BWT, main = "HISTOGRAM SHOWING THE BIRTH WEIGHT",xlab = 'grams',ylab = 'number of births',col = 'yellow')
qqnorm(bwdata$BWT)
qqline(bwdata$BWT)
ggdensity(bwdata$BWT,main="Test for normality using BWT data")
shapiro.test(bwdata$BWT)

#Qn6
t.test(bwdata$BWT, alternative = "less", mu=2500)

#Qn7
K<-boxplot(bwdata$BWT~bwdata$SMOKE, main="boxplot of birth weight by smoking status of mothers", col = c("orange","red"),border = "brown")
K
#Qn8
hist(bwdata$BWT~table(bwdata$SMOKE),freq = NULL, main = "Birth weight by smoking status")
#Qn9

#Qn10
tapply(bwdata$BWT, list(bwdata$SMOKE),mean)
tapply(bwdata$BWT, list(bwdata$SMOKE),var)
tapply(bwdata$BWT, list(bwdata$SMOKE),sd)
tapply(bwdata$BWT, list(bwdata$SMOKE),sd)/sqrt(nrow(bwdata))
tapply(bwdata$BWT, list(bwdata$SMOKE),mean)-(1.96*tapply(bwdata$BWT, list(bwdata$SMOKE),sd)/sqrt(nrow(bwdata)))
tapply(bwdata$BWT, list(bwdata$SMOKE),mean)+(1.96*tapply(bwdata$BWT, list(bwdata$SMOKE),sd)/sqrt(nrow(bwdata)))
table(bwdata$SMOKE)
#Qn11
quantile(table(bwdata$SMOKE),c(.10,.25,.50,.60,.75,.90))

#Qn12
t.test(bwdata$BWT~bwdata$SMOKE,alternative="less")
#for non parametric test
wilcox.test(bwdata$BWT~bwdata$SMOKE,alternative="less")
#Qn13
tapply(bwdata$BWT, list(bwdata$RACE),mean)
bartlett.test(bwdata$BWT~bwdata$RACE)
#QN14
#chq<-chisq.test(table(bwdata$LOW,bwdata$RACE))
#chq

#Qn15
prop.test(table(bwdata$LOW,bwdata$SMOKE),correct = TRUE)
#QN16
xsq<-chisq.test(table(bwdata$LOW,bwdata$RACE))
xsq
xsq$observed   # observed counts (same as M)
xsq$expected   # expected counts under the null
xsq$residuals  # Pearson residuals
xsq$stdres 
#Qn17
pairs(~AGE+LWT+BWT,data = bwdata, main="Scatter plot matrix for continuous variables")
mydata<-cbind(bwdata$AGE,bwdata$LWT,bwdata$BWT)
mydata
cor(mydata)
#Qn18.
round(cor(cbind(bwdata$BWT,bwdata$AGE,bwdata$LWT,bwdata$SMOKE,bwdata$PTL,bwdata$HT,bwdata$UI,bwdata$FTV)),2)
modelT<-lm(bwdata$BWT~bwdata$AGE+bwdata$LWT+bwdata$SMOKE+bwdata$PTL+bwdata$HT+bwdata$UI+bwdata$FTV)
summary(modelT)
vif(modelT)
coefficients(modelT)
#confint(modelT, level=0.05)
fitted(modelT)
residuals(modelT)
#modelT$residuals
shapiro.test(residuals(modelT))
hist(residuals(modelT),probability = T,main = 'HISTOGRAM FOR TESTING NORMALITY')
lines(density(residuals(modelT)),col=2)
qqnorm(residuals(modelT),col='red')
qqline(residuals(modelT),col='blue')
ggdensity(residuals(modelT),main='Testing for Normality',col='black')
plot(fitted(modelT),residuals(modelT))
#Qn19
round(cor(cbind(bwdata$BWT,bwdata$LWT,bwdata$HT,bwdata$UI)))
modelReduced<-lm(bwdata$BWT~bwdata$LWT+bwdata$HT+bwdata$UI)
modelReduced
summary(modelReduced)
vif(modelReduced)
coefficients(modelReduced)
residuals(modelReduced)
fitted(modelReduced)
shapiro.test(residuals(modelReduced))
hist(residuals(modelReduced),probability = T,main = "HISTOGRAM FOR NORMALITY CHECKING")
lines(density(residuals(modelReduced)))
qqnorm(residuals(modelReduced))
qqline(residuals(modelReduced))
ggdensity(residuals(modelReduced))
plot(fitted(modelReduced),residuals(modelReduced))

#Qn20
summary(glm(formula=bwdata$LOW ~ bwdata$AGE + bwdata$LWT + bwdata$SMOKE, family=binomial(link=logit)))
#Qn21
summary(glm(formula=bwdata$LOW ~ bwdata$LWT + bwdata$SMOKE, family=binomial(link=logit)))

#Qn22
logisout<-glm(formula=bwdata$LOW ~ bwdata$AGE + bwdata$LWT + bwdata$SMOKE, family=binomial(link=logit))
odds_ratious<-exp(logisout$coefficients)
odds_ratious
