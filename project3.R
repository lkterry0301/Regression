load("Lab3data.Rdata")
summary(reg.data)

#names(reg.data)=c("SITE","SEX","RACE","ITN","BMI","MVPA","PC","CL","AP")
attach(reg.data)
nrow(reg.data)

#call amelia
library(Amelia)
library(foreign)

pairs(reg.data[,4:9])

list.na=is.na(reg.data)

## data only has SEX and RACE ###
list.missing6 <- rowSums(list.na) >= 6
reg.data.missing6 <- reg.data[list.missing6,]
nrow(reg.data.missing6)

## exclude data that are completely missing 

list.missing5<-rowSums(list.na)<=5
reg.data.full<-reg.data[list.missing5,]
nrow(reg.data.full)

# save data file
write.dta(reg.data.full, "f:/R/6450/full.dta")

###### senario 1 ############
## Exclude any data that has variable missing
reg.data.valid <- na.omit(reg.data)
reg.data.valid
pairs(reg.data.valid[,6:9])
nrow(reg.data.valid)

# save data file
write.dta(reg.data.valid, "f:/R/6450/valid.dta")

###### senario 2 ############
## Exclude data that missing all of MVPA, PC, CL and AP
list.na=is.na(reg.data[,6:9])
list.nomissing= rowSums(list.na)<=3
reg.data.select=reg.data[list.nomissing,]
reg.data.select
nrow(reg.data.select)
pairs(reg.data.select[,6:9])

# save data file
write.dta(reg.data.select, "f:/R/6450/select.dta")

############ Regression analysis ############
library(leaps)
library(car)

####### regress using imputated data ###########
## load imputated data ##
data.imp=read.csv("select-imp1.csv",header=T)
names(data.imp)

names(data.imp)=c("SITE","SEX","RACE","ITN","BMI","MVPA","PC","CL","AP")

## fit multiple linear regression
fit.imp=lm(data.imp$MVPA~data.imp$PC+data.imp$CL+data.imp$AP,data=data.imp)
summary(fit.imp)
par(mfrow=c(2,2))
plot(fit.imp)

####### diagnostics #######
# Assessing Outliers
outlierTest(fit.imp) 			# Bonferonni p-value for most extreme obs
qqPlot(fit.imp, main="QQ Plot")   #qq plot for studentized resid 
	# leverage plots

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit.imp, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit.imp) 
hist(sresid, freq=FALSE, 
   main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit.imp)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit.imp)

# Evaluate Collinearity
vif(fit.imp) # variance inflation factors 
sqrt(vif(fit.imp)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit.imp)

####### excludeing a few possible outliers 497, 502 ########
data.imp.adj = data.imp[c(1:496,498:501,503:1036),]
fit.imp.adj=lm(data.imp.adj$MVPA~data.imp.adj$PC+data.imp.adj$CL+data.imp.adj$AP,data=data.imp.adj)
summary(fit.imp.adj)
plot(fit.imp.adj)

## Model selection
leaps( x=data.imp[,7:9], y=data.imp[,6], 
	names=names(data.imp)[7:9], method="Cp")

Base <- lm( data.imp$MVPA ~ data.imp$PC, data=data.imp )
step(Base, scope = list( upper=fit.imp, lower=~1 ), direction = "both", trace=FALSE)

####### regress using valid data ###########
## load valid data
load("Lab3data.Rdata")
data.valid <- na.omit(reg.data)

names(data.valid)=c("SITE","SEX","RACE","ITN","BMI","MVPA","PC","CL","AP")

## fit multiple linear regression
fit.valid=lm(data.valid$MVPA~data.valid$PC+data.valid$CL+data.valid$AP,data=data.valid)
summary(fit.valid)

fit.other=lm(data.imp$PC~data.imp$MVPA,data=data.imp)
summary(fit.other)

####### regress using subset data ###########
library(leaps)
library(car)

## Read glaobal data ##
data.imp=read.csv("select-imp1.csv",header=T)
names(data.imp)=c("SITE","SEX","RACE","ITN","BMI","MVPA","PC","CL","AP")

## only use data with MVPA<150 ##
data.imp.low <- subset(data.imp, MVPA < 150)
nrow(data.imp.low)

fit.imp.low=lm(data.imp.low$MVPA~data.imp.low$PC+data.imp.low$CL+data.imp.low$AP,data=data.imp.low)
summary(fit.imp.low)

plot(fit.imp.low)

## Model selection

Base.low <- lm( data.imp.low$MVPA ~ data.imp.low$PC, data=data.imp.low )
step(Base.low, scope = list( upper=fit.imp.low, lower=~1 ), direction = "both", trace=FALSE)


## only use data with MVPA>=150 ##
data.imp.high <- subset(data.imp, MVPA >= 150)
nrow(data.imp.high)

fit.imp.high=lm(data.imp.high$MVPA~data.imp.high$PC+data.imp.high$CL+data.imp.high$AP,data=data.imp.high)
summary(fit.imp.high)
plot(fit.imp.high)

## Model selection

Base.high <- lm( data.imp.high$MVPA ~ data.imp.high$PC, data=data.imp.high )
step(Base.high, scope = list( upper=fit.imp.high, lower=~1 ), direction = "both", trace=FALSE)


### only use male data ###

data.imp.boy = subset(data.imp, SEX == 1)
nrow(data.imp.boy)
summary(data.imp.boy$MVPA)

fit.imp.boy=lm(data.imp.boy$MVPA~data.imp.boy$PC+data.imp.boy$CL+data.imp.boy$AP,
data=data.imp.boy)
summary(fit.imp.boy)

par(mfrow=c(2,2))
plot(fit.imp.boy)

## Model selection
library (MASS)
step.boy <- stepAIC(fit.imp.boy, direction="both")
step.boy$anova # display results


### only use female data ###

data.imp.girl = subset(data.imp, SEX == 2)
nrow(data.imp.girl)

fit.imp.girl=lm(data.imp.girl$MVPA~data.imp.girl$PC+data.imp.girl$CL+data.imp.girl$AP,
data=data.imp.girl)
summary(fit.imp.girl)

plot(fit.imp.girl)

## Model selection

step.girl <- stepAIC(fit.imp.girl, direction="both")
step.girl$anova # display results


### only use asian ###
data.imp.asian = subset(data.imp, RACE== 2)
nrow(data.imp.asian)

fit.imp.asian=lm(data.imp.asian$MVPA~data.imp.asian$PC+data.imp.asian$CL+data.imp.asian$AP,
data=data.imp.asian)
summary(fit.imp.asian)

plot(fit.imp.asian)

## Model selection

step.asian <- stepAIC(fit.imp.asian, direction="both")
step.asian$anova # display results

### only use black ###
data.imp.black = subset(data.imp, RACE== 3)
nrow(data.imp.black)

fit.imp.black=lm(data.imp.black$MVPA~data.imp.black$PC+data.imp.black$CL+data.imp.black$AP,
data=data.imp.black)
summary(fit.imp.black)
summary(data.imp.black$MVPA)
summary(data.imp.black$CL)

plot(fit.imp.black)

## Model selection

step.black <- stepAIC(fit.imp.black, direction="both")
step.black$anova # display results

### only use white ###
data.imp.white = subset(data.imp, RACE== 4)
nrow(data.imp.white)

fit.imp.white=lm(data.imp.white$MVPA~data.imp.white$PC+data.imp.white$CL+data.imp.white$AP,
data=data.imp.white)
summary(fit.imp.white)
summary(data.imp.white$MVPA)
summary(data.imp.white$CL)
plot(fit.imp.white)

## Model selection

step.white <- stepAIC(fit.imp.white, direction="both")
step.white$anova # display results

### only use other ###
data.imp.other = subset(data.imp, RACE== 5)
nrow(data.imp.other)

fit.imp.other=lm(data.imp.other$MVPA~data.imp.other$PC+data.imp.other$CL+data.imp.other$AP,
data=data.imp.other)
summary(fit.imp.other)

plot(fit.imp.other)

## Model selection
step.other <- stepAIC(fit.imp.other, direction="both")
step.other$anova # display results

### regression using ITN ###

fit.itn = lm(data.imp$MVPA ~ log(data.imp$ITN), data=data.imp)
data.itn.low=subset(data.imp, ITN<1)
fit.itn.low=lm(data.itn.low$MVPA~log(data.itn.low$ITN), data=data.itn.low)
summary(fit.itn.low)

data.itn.high=subset(data.imp, ITN>=1)
fit.itn.high=lm(data.itn.high$MVPA~log(data.itn.high$ITN), data=data.itn.low)
summary(fit.itn.high)
fit.itn.high=lm(data.itn.high$PC~log(data.itn.high$ITN), data=data.itn.low)
summary(fit.itn.high)
fit.itn.high=lm(data.itn.high$CL~log(data.itn.high$ITN), data=data.itn.low)
summary(fit.itn.high)
fit.itn.high=lm(data.itn.high$AP~log(data.itn.high$ITN), data=data.itn.low)
summary(fit.itn.high)
### regression using BMI ###

fit.bmi = lm( data.imp$BMI~ data.imp$MVPA, data=data.imp)
summary(fit.bmi)

data.bmi.high=subset(data.imp, BMI>=85)

nrow(data.bmi.high)

fit.bmi.high=lm( data.bmi.high$BMI~ data.bmi.high$MVPA, data=data.bmi.high)
summary(fit.bmi.high)
