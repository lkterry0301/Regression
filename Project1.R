## Load librarys ##
library(MASS)
library(graphics) 

## original dataset ##
data.exercise = read.csv("Force-Delta-log.csv",header = T, sep = ",");
data.exercise = data.frame(data.exercise) 
attach(data.exercise)

## modified dataset-eliminate outlies and wrong data ##
data.tracker = read.csv("proj1.csv",header = T, sep = ",");
data.tracker = data.frame(data.tracker) 
attach(data.tracker)

## ------- Simple Plotting --------------------##

pairs(data.exercise)
pairs(data.tracker)

## ------Regress Calories on 4 seperate variables------------##
Y=data.tracker$tCalories
X.steps=data.tracker$tSteps
X.distance=data.tracker$tDistance
X.floors=data.tracker$tFloors
X.actminute=data.tracker$tActiveMinutes

## Regress on steps##
b.1.steps=sum((X.steps-mean(X.steps))*(Y-mean(Y)))/sum((X.steps-mean(X.steps))^2)
b.0.steps=mean(Y)-b.1.steps*mean(X.steps)
Y.hat.steps=b.0.steps + b.1.steps*X.steps
e.steps=Y-Y.hat.steps

## Regress on distance ##
b.1.distance=sum((X.distance-mean(X.distance))*(Y-mean(Y)))/sum((X.distance-mean(X.distance))^2)
b.0.distance=mean(Y)-b.1.distance*mean(X.distance)
Y.hat.distance=b.0.distance + b.1.distance*X.distance
e.distance=Y-Y.hat.distance

## Regress on floors ##
b.1.floors=sum((X.floors-mean(X.floors))*(Y-mean(Y)))/sum((X.floors-mean(X.floors))^2)
b.0.floors=mean(Y)-b.1.floors*mean(X.floors)
Y.hat.floors=b.0.floors + b.1.floors*X.floors
e.floors=Y-Y.hat.floors

## Regress on active minutes ##
b.1.actminute=sum((X.actminute-mean(X.actminute))*(Y-mean(Y)))/sum((X.actminute-mean(X.actminute))^2)
b.0.actminute=mean(Y)-b.1.actminute*mean(X.actminute)
Y.hat.actminute=b.0.actminute + b.1.actminute*X.actminute
e.actminute=Y-Y.hat.actminute

## --------Check the best-fit regression model----------##

SSTO=sum((Y-mean(Y))^2)
n=20

SSE.steps=sum(e.steps^2)
MSE.steps=SSE.steps/(n-2)

SSE.distance=sum(e.distance^2)
SSE.floors=sum(e.floors^2)
SSE.actminute=sum(e.actminute^2)

Rsquare.steps = (SSTO-SSE.steps)/SSTO
Rsquare.distance = (SSTO-SSE.distance)/SSTO
Rsquare.floors = (SSTO-SSE.floors)/SSTO
Rsquare.actminute = (SSTO-SSE.actminute)/SSTO

Rsquare.steps
Rsquare.distance
Rsquare.floors
Rsquare.actminute

##--------Test Hypothesis if Beta.1.Steps =0-----#
s.b1.steps=sqrt(MSE.steps/sum((X.steps-mean(X.steps))^2))
t.star.steps=b.1.steps/s.b1.steps
t.star.steps
qt(0.975,n-2)

##----------Use Regression Model on steps -----##
X.steps.new = 12765
Y.hat.new =b.0.steps + b.1.steps*X.steps.new
alpha = 0.05
t=qt(1-(alpha/2),n-2)

plot(data.tracker$tSteps,data.tracker$tCalories,cex=1.5,cex.lab=1.5,xlab="Steps",ylab="Calories Burned")
abline(b.0.steps, b.1.steps,col=2,lwd=3)

##---------Prediction Interval for 23rd day--------##
Nume=(X.steps.new-mean(X.steps))^2
Deno=sum((X.steps-mean(X.steps))^2)
add=1+(1/n)+(Nume/Deno)
s.square.pred = MSE.steps*add
s.pred = sqrt(s.square.pred)
e.steps.star=e.steps/sqrt(MSE.steps)

lower = Y.hat.new-t*s.pred
upper = Y.hat.new+t*s.pred
Y.hat.new
lower
upper

##------plot the 95 percent Prediction Interval with Predicted value----##
##-can change confidence by setting alpha above another value-##
plot(data.tracker$tSteps,data.tracker$tCalories,cex=1.5,cex.lab=1.5,xlab="Steps",ylab="Calories Burned")
abline(b.0.steps, b.1.steps,col=2,lwd=3)
lines(X.steps.new,Y.hat.new,type="p",col=4,lwd=2)
segments(X.steps.new,lower,X.steps.new,upper,col=4,lwd=2)

##-----------Check Model's Assumptions----------##
##-------Check independent epsilons----##
plot(seq(1:20),e.steps,cex=1.5,cex.lab=1.5,xlab="Observation Order",ylab="Residuals",type="l",lwd=1.5)
lines(e.steps,type="p")
abline(0,0,col=2,lwd=2)

##-----Check Constant Variance of epsilons---##
plot(X.steps, e.steps,cex=1.5,cex.lab=1.5,xlab="Steps",ylab="Residuals",lwd=1.5)
e.steps.absolute=sqrt(e.steps^2)
plot(X.steps, e.steps.absolute,cex=1.5,cex.lab=1.5,xlab="Steps",ylab="Absolute Residuals",lwd=1.5)

##-----Check for Normality-------##
rank=rank(e.steps)
expected.value = sqrt(MSE.steps)*qnorm((rank-0.375)/(n+0.25))
plot(expected.value,e.steps,cex=1.5,type="p",cex.lab=1.5,xlab="Expected Value under Normality",ylab="Residual")
abline(0,1,col=2,lwd=2)

rnume=sum((e.steps-mean(e.steps))*(expect-mean(expect)))
rdeno=sqrt(sum((e.steps-mean(e.steps))^2)*sum((expect-mean(expect))^2))
r=rnume/rdeno

##-----Plots to see a healthier lifestyle--##
plot(seq(1:20),tSteps,type="p",cex=1.5,xlab="Observation Order",ylab="Steps",cex.lab=1.5)
plot(seq(1:20),tDistance,type="p",cex=1.5,xlab="Observation Order",ylab="Distance",cex.lab=1.5)
plot(seq(1:20),tFloors,type="p",cex=1.5,xlab="Observation Order",ylab="Floors",cex.lab=1.5)
plot(seq(1:20),tActiveMinutes,type="p",cex=1.5,xlab="Observation Order",ylab="ActiveMinutes",cex.lab=1.5)
plot(seq(1:20),tCalories,type="p",cex=1.5,xlab="Observation Order",ylab="Calories",cex.lab=1.5)
