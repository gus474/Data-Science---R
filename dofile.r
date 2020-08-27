#Appendix I 

#6.28 Part A
	pop <- c(CDI$V5)
	area <- c(CDI$V4)
	income <- c(CDI$V16)
	physicians<- c(CDI$V8)
	over64 <- c(CDI$V7)
popdensity <- c(pop/area)
	stem(pop)
	stem(area)
	stem(income)
	stem(over64)
	stem(popdensity)

#Part B
data <- data.frame(physicians, pop, area, income, over64, popdensity)
model1 = data[,1:4]
pairs(model1)
model2=data[,c(1,4,5,6)]
pairs(model2)
cor(model1)
cor(model2)

#Part C
fit1= lm(physicians~ pop+area+income)
fit2= lm(physicians~ popdensity+over64+income)
summary(fit1)
summary(fit2)
#Part D
summary(fit1)
summary(fit2)
#Part E
 residuals1= fit1$residuals
Yhat1= fitted.values(fit1)
 Yhat2=fitted.values(fit2)
 residuals2=fit2$residuals
 plot(x=Yhat1, y=residuals1)
 plot(x=Yhat2, y=residuals2)
 plot(x=pop,y=residuals1)
 plot(x=area, y=residuals1)
 plot(x=income, y=residuals1)
 plot(x=over64, y=residuals2)
 plot(x=income, y=residuals2)
plot(x=popdensity, y=residuals2)
plot(x=pop*area, y=residuals1)
plot(x=pop*income, y=residuals1)
plot(x= income*area, y=residuals1)
plot(x= popdensity*income, y=residuals2)
plot(x=popdensity*over64, y=residuals2)
plot(x=over64*income, y=residuals2)
qqplot1=qqnorm(residuals1)
qqline(residuals1, col= "red")
qqplot=qqnorm(residuals2)
qqline(residuals2, col= "blue")
#Part F
fit1wi = lm(physicians ~ pop + area + income + pop*area + pop*income + income*area, data)
summary(fit1wi)

fit2wi = lm(physicians ~ over64 + popdensity + income + over64*popdensity + over64*income + popdensity*income,data)
summary(fit2wi)
#7.37
#Part A & B
m <- lm(physicians ~ pop + income, data)
anova(m)
m3 <- lm(physicians ~ pop + income + area, data)
anova(m3)
m4 <- lm(physicians~ pop + income + over64, data)
anova(m4)
m5 <- lm(physicians ~ pop + income + hospitalbeds, data)
anova(m5)
# (140967081 - 136903711)/140967081

#Part C
anova(m, m5)
#Part D
m34 <- lm(physicians ~ pop + income + area + over64, data)
anova(m34)
m35 <- lm(physicians ~ pop + income + area + hospitalbeds, data)
anova(m35)
m45 <- lm(physicians ~ pop + income + over64 + hospitalbeds, data)
anova(m45)
anova(m,m45)
