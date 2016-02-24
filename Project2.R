##### Model Selection#####

#### Problem 3 #### same year prediction ####

##### read data #####
my.data=read.csv("data3.csv",header=T)
names(my.data)=c("year","XC1","XC2","XC3","XC4","XF1","XF2","XF3","XF4","XO1","XO2","XO3","XO4","XU1","XU2","XU3","XU4")

##### full model #####
fmodel=lm(XO3~XC1+XC2+XC3+XC4+XF1+XF2+XF3+XF4+XO1+XO2+XO4+XU1+XU2+XU3+XU4,data=my.data)

##### AIC #####
library(MASS)
AICp = stepAIC(fmodel, direction="both")
AICp$anova

##### autoselection with both direction #####
step(Base, scope = list( upper=fmodel, lower=~1 ), direction = "both", trace=FALSE)

dropterm( fmodel, test = "F" )

##### Cp #####
library(leaps)
p=leaps(x=my.data[,c(2:11,13:17)], y=my.data[,12],names=names(my.data)[c(2:11,13:17)],
	method="Cp")

plot(p$size,p$Cp,xlab="p", ylab="Cp")
plot(p$size[21:141],p$Cp[21:141],xlab="p", ylab="Cp")

##### adjusted R2 ######
r=leaps(x=my.data[,c(2:11,13:17)], y=my.data[,12],names=names(my.data)[c(2:11,13:17)],
	method="adjr2")
plot(r$size,r$adjr2)
plot(r$size[21:141],r$adjr2[21:141])

#####selected model#####

### 1. XO3~XC1+XC4+XF3+XO2+XU1+XU2+XU3 ### P=8 ### Order=61###
s.model1=lm(XO3~XC1+XC4+XF3+XO2+XU1+XU2+XU3,data=my.data)
p$Cp[61]
r$adjr2[61]

### 2. XO3~XC1+XC4+XF1+XF3+XO2+XU1+XU2+XU3 ### P=9 ### Order=71###
s.model2=lm(XO3~XC1+XC4+XF1+XF3+XO2+XU1+XU2+XU3,data=my.data)
p$Cp[71]
r$adjr2[71]

### 3. XO3~XC1+XC4+XF1+XF4+XO1+XO2+XO4+XU1+XU2+XU3 ### P=11 ### Order=91###
s.model3=lm(XO3~XC1+XC4+XF1+XF4+XO1+XO2+XO4+XU1+XU2+XU3,data=my.data)
p$Cp[91]
r$adjr2[91]

### 4. XO3~XC1+XC2+XC4+XF1+XF2+XF4+XO1+XO2+XO4+XU1+XU2+XU3+XU4 ### P=14 ### Order=123### Cloest Cp to p from above
s.model4=lm(XO3~XC1+XC2+XC4+XF1+XF2+XF4+XO1+XO2+XO4+XU1+XU2+XU3+XU4,data=my.data)
p$Cp[123]
r$adjr2[123]

### 5. XO3~XC1+XC2+XC4+XF1+XF2+XF3+XF4+XO1+XO2+XO4+XU1+XU2+XU3+XU4 ### P=15 ### Order=131### Max adjusted R squared
s.model3=lm(XO3~XC1+XC2+XC4+XF1+XF2+XF3+XF4+XO1+XO2+XO4+XU1+XU2+XU3+XU4,data=my.data)
p$Cp[131]
r$adjr2[131]


##### Model Selection#####

#### Problem 4 #### same year prediction ####

##### read data #####
my.data=read.csv("data4.csv",header=T)
names(my.data)=c("year","XC1","XC2","XC3","XC4","XF1","XF2","XF3","XF4","XO1","XO2","XO3","XO4","XU1","XU2","XU3","XU4","Y")

##### full model #####
fmodel=lm(Y~XC1+XC2+XC4+XF1+XF2+XF3+XF4+XO1+XO2+XO3+XO4+XU1+XU2+XU3+XO3,data=my.data)
summary(fmodel)

##### AIC #####
library(MASS)
AICp = stepAIC(fmodel, direction="both")
AICp$anova

##### autoselection with both direction #####
step(Base, scope = list( upper=fmodel, lower=~1 ), direction = "both", trace=FALSE)

dropterm( fmodel, test = "F" )

##### Cp #####
library(leaps)
p=leaps(x=my.data[,c(2:3,5:17)], y=my.data[,18],names=names(my.data)[c(2:3,5:17)],
	method="Cp")

plot(p$size,p$Cp,xlab="p",ylab="Cp")

min(abs(p$Cp-p$size)[abs(p$Cp-p$size)>0])
p$which[61,]

##### adjusted R2 ######
r=leaps(x=my.data[,c(2:3,5:17)], y=my.data[,18],names=names(my.data)[c(2:3,5:17)],
	method="adjr2")
plot(r$size,r$adjr2xlab="p",ylab="adjusted R-square")
max(r$adjr2[1:140])
p$which[61,]

#####selected model#####
### Y~XC1+XC2+XF1+XF2+XU1+XU2+XU3 ### P=8 ### Order=61### Max adjusted R squared, Cp Close to p
s.model=lm(Y~XC1+XC2+XF1+XF2+XU1+XU2+XU3,data=my.data)
p$Cp[61]
r$adjr2[61]

####model delection#####
##### read data #####
my.data=read.csv("data4.csv",header=T)
names(my.data)=c("year","XC1","XC2","XC3","XC4","XF1","XF2","XF3","XF4","XO1","XO2","XO3","XO4","XU1","XU2","XU3","XU4","Y")
library(MASS)

##### full model #####
fmodel=lm(Y~XC1+XC2+XC4+XF1+XF2+XF3+XF4+XO1+XO2+XO3+XO4+XU1+XU2+XU3+XU4,data=my.data)
#summary(fmodel)
extractAIC (fmodel)

##### $ reduced model 0: through origin ##### 
rmodel0=lm(Y~0+XC1+XC2+XC3+XC4+XF1+XF2+XF3+XF4+XO1+XO2+XO3+XO4+XU1+XU2+XU3+XU4,data=my.data)
#summary(rmodel0)
extractAIC(rmodel0)

##### $ reduced model 1: within same state #####
rmodel1=lm(Y~XO1+XO2+XO3+XO4,data=my.data)
#summary(rmodel1)
extractAIC(rmodel1)

##### reduced model 2: within same type #####
rmodel2=lm(Y~XC3+XF3+XU3+XO3,data=my.data)
#summary(rmodel2)
extractAIC(rmodel2)

##### reduced model 3: US data #####
rmodel3=lm(Y~XU1+XU2+XU3+XU4,data=my.data)
#summary(rmodel3)
extractAIC(rmodel3)

##### reduced model 4: US data same type #####
rmodel4=lm(Y~XU3,data=my.data)
#summary(rmodel4)
extractAIC(rmodel4)

##### model 5:time series #####
rmodel5=lm(Y~year,data=my.data)
#summary(rmodel5)
extractAIC(rmodel5)

### p value min -> max ###
#XC1, XC2, XU2, XU4, XU1, XF4, XO3, XC4, XF1, XO2, XO3, XO4, XF2, XF3, XU3


##### reduced model 6: significant variables defined from the full model result (3) #####
rmodel6=lm(Y~XC1+XC2+XU2,data=my.data)
#summary(rmodel6)
extractAIC(rmodel6)

##### reduced model 7: significant variables defined from the full model result (7) #####
rmodel7=lm(Y~XC1+XC2+XU2+XU4+XU1+XF4+XO3,data=my.data)
#summary(rmodel7)
extractAIC(rmodel7)

##### reduced model 8: significant variables defined from the full model result (10) #####
rmodel8=lm(Y~XC1+XC2+XU2+XU4+XU1+XF4+XO3+XC4+XF1+XO2,data=my.data)
#summary(rmodel8)
extractAIC(rmodel8)

##### reduced model 9: picked from the result of Cp and adjusted R2 #####
rmodel9=lm(Y~XC1+XC2+XF1+XF2+XU1+XU2+XU3,data=my.data)
#summary(rmodel9)
extractAIC(rmodel9)

##### reduced model 10: only affected by the previous year #####
rmodel10=lm(Y~XO3, data=my.data)
#summary(rmodel10)
extractAIC(rmodel10)

##### reduced model 11: modified based on collinearity (from full model) #####
rmodel11=lm(Y~XC1+XF1+XF2+XF3+XO1+XO2+XO3+XU1+XU3,data=my.data)
#summary(rmodel11)
extractAIC (rmodel11)

##### reduced model 12: modified based on collinearity (from full model) (from reduced model 9) #####
rmodel12=lm(Y~XC1+XF1+XF2+XU1+XU3,data=my.data)
#summary(rmodel12)
extractAIC(rmodel12)


##### evaluation using 2010 data to test 2011#####

data.pred=read.csv('test4.csv',header=F)
names(data.pred)=c("year","XC1","XC2","XC3","XC4","XF1","XF2","XF3","XF4","XO1","XO2","XO3","XO4","XU1","XU2","XU3","XU4","Y")

##### full model #####
predict(fmodel, newdata=data.pred,interval='prediction')

##### reduced model : through origin #####
predict(rmodel0, newdata=data.pred,interval='prediction')

##### reduced model 1: within same state #####
predict(rmodel1, newdata=data.pred,interval='prediction')

##### reduced model 2: within same type #####
predict(rmodel2, newdata=data.pred,interval='prediction')

##### reduced model 3: US data #####
predict(rmodel3, newdata=data.pred,interval='prediction')

##### reduced model 4: US data same type #####
predict(rmodel4, newdata=data.pred,interval='prediction')

##### model 5:time series #####
predict(rmodel5, newdata=data.pred,interval='prediction')

##### reduced model 6: significant variables defined from the full model result #####
predict(rmodel6, newdata=data.pred,interval='prediction')

##### reduced model 7: significant variables defined from the full model result #####
predict(rmodel7, newdata=data.pred,interval='prediction')

##### reduced model 8: significant variables defined from the full model result #####
predict(rmodel8, newdata=data.pred,interval='prediction')

##### reduced model 9: picked from the result of Cp #####
predict(rmodel9, newdata=data.pred,interval='prediction')

##### reduced model 10
predict(rmodel10, newdata=data.pred,interval='prediction')

##### reduced model 11
predict(rmodel11, newdata=data.pred,interval='prediction')

##### reduced model 12
predict(rmodel12, newdata=data.pred,interval='prediction')


my.data=read.csv("dataAR.csv",header=T)
names(my.data)=c("XO3")
my.data=data.frame(my.data)
attach(my.data)
ts(data=my.data,start=1994, end=2010, frequency=1, deltat=1)
ts.data=ts(data=my.data,start=1994, end=2010, frequency=1, deltat=1)
ts.plot(XO3)

###fit a regressional model to remove the linear trend###
Y=ts.data
X=time(ts.data)
trend=lm(Y~X,data=ts.data)

resid.data=ts.data-predict(trend, newdata=X)

#Construct a sample autocorrelation and partial autocorrelation plots
par(mfrow=c(2,1))
acf(resid.data)
pacf(resid.data)

#############################################
## Fit AR models
#Fit a AR(p) model
ar.data <- ar(resid.data)
ar.data

#examine the residuals
par(mfrow=c(1,1))
ts.plot(ar.data$resid)

par(mfrow=c(2,1))
acf(ar.data$resid,na.action=na.omit)
pacf(ar.data$resid,na.action=na.omit)

#############################################
## Prediction

#Subset time series
data.obs=predict(trend, newdata=X)+window(resid.data)

#predict the XO3 from 2011 to 2015
N.pred=5
time.pred=read.table("pre.txt",header=F)
names(time.pred)=c("X")

resid.data.pred=(predict(ar.data, n.ahead=N.pred))$pred
data.pred= resid.data.pred+ predict(trend, newdata=time.pred)

#plot the entire observed series and the predictions
par(mfrow=c(1,1))
ts.plot(data.obs, data.pred,col=c("black", "red"), lty=c(1,2))
abline(v=2010.5, lty=2)
