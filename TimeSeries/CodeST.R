### R code from vignette source 'C:/R-4.0.2/library/utils/Sweave/slides-series-temporelles-M1.Rnw'

###################################################
### code chunk number 1: slides-series-temporelles-M1.Rnw:63-67
###################################################
rm(list=ls(all=TRUE))
setwd("C:/Users/mlebreto/Documents/gretha/2020-2021/M1IREF/slides")
library(forecast);library(caschrono)
library(lmtest);library(urca)


###################################################
### code chunk number 2: slides-series-temporelles-M1.Rnw:151-155
###################################################
uspop=read.table("uspop.dat")[,1]
uspop=ts(uspop,start=1790,freq=0.1)
deaths =read.table("deaths.dat")[,1]  
deaths<-ts(deaths, start=c(1973,1), freq=12) 


###################################################
### code chunk number 3: slides-series-temporelles-M1.Rnw:161-163
###################################################
data(champa.ts)
str(champa.ts)


###################################################
### code chunk number 4: slides-series-temporelles-M1.Rnw:166-168
###################################################
data(indbourse)
str(indbourse)


###################################################
### code chunk number 5: plot1
###################################################
plot.ts(uspop,xlab='année', ylab='population', main="Population des Etats-Unis",col=1,type="o")


###################################################
### code chunk number 6: slides-series-temporelles-M1.Rnw:188-190
###################################################
library(ggplot2);date=seq(1790,1990,by=10)
data=as.data.frame(cbind(date,uspop),nrow=length(date))


###################################################
### code chunk number 7: plot2
###################################################
ggplot(data) + geom_line(aes(x = date, y = uspop))


###################################################
### code chunk number 8: plot3
###################################################
plot.ts(deaths,xlab='année',col=3,type="o")


###################################################
### code chunk number 9: plot4
###################################################
plot(champa.ts)


###################################################
### code chunk number 10: slides-series-temporelles-M1.Rnw:247-249
###################################################
cac=as.numeric(indbourse[,5])
rt=diff(log(cac));rt=na.omit(rt)


###################################################
### code chunk number 11: plot5
###################################################
plot.ts(rt)


###################################################
### code chunk number 12: slides-series-temporelles-M1.Rnw:300-301
###################################################
dec<-decompose(champa.ts,"multiplicative", filter = NULL)


###################################################
### code chunk number 13: plot6
###################################################
plot(dec)


###################################################
### code chunk number 14: slides-series-temporelles-M1.Rnw:311-312
###################################################
decd<-decompose(deaths,"additive", filter = NULL)


###################################################
### code chunk number 15: plot7
###################################################
plot(decd)


###################################################
### code chunk number 16: plot8
###################################################
monthplot(champa.ts)


###################################################
### code chunk number 17: plot9
###################################################
monthplot(deaths)


###################################################
### code chunk number 18: slides-series-temporelles-M1.Rnw:435-438
###################################################
data(npext)
PIB<-ts(na.omit(npext[,"realgnp"]),start=1909,freq=1)
TPIB<-length(PIB)


###################################################
### code chunk number 19: slides-series-temporelles-M1.Rnw:518-532
###################################################
set.seed(41304769);N=150
#fonction pour simuler une marcha aléatoire
#mu est la dérive
#x0 est la valeur de départ
#N est le nombre d'observations
RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}

DS<-RW(N=N,0,0.1,0.2)


###################################################
### code chunk number 20: slides-series-temporelles-M1.Rnw:539-545
###################################################
TS<-1+0.15*seq(1,N)+rnorm(N)
AR1=arima.sim(list(order=c(1,0,0),ar=0.65),n=N)+10
XXX=matrix(cbind(DS,TS, AR1),nrow=N,ncol=3)
colnames(XXX) <- c("DS","TS","AR1")
XXX=XXX[50:100,]
XXX=ts(XXX,start=c(1960,1),freq=12)


###################################################
### code chunk number 21: plot10
###################################################
plot.ts(XXX,plot.type = "single",col=1:3)
#legend("top",c("DS", "TS", "AR1"),col=1:3,lty=rep(1,3))


###################################################
### code chunk number 22: slides-series-temporelles-M1.Rnw:573-575
###################################################
set.seed(123456)
BB<-rnorm(150)


###################################################
### code chunk number 23: plot11
###################################################
plot.ts(BB)


###################################################
### code chunk number 24: plot12
###################################################
Acf(BB)


###################################################
### code chunk number 25: plot13
###################################################
Acf(PIB)


###################################################
### code chunk number 26: slides-series-temporelles-M1.Rnw:694-696
###################################################
data(nporg);cho=na.omit(nporg[,"ur"])
cho=ts(cho,freq=1,start=1890)


###################################################
### code chunk number 27: plot14
###################################################
Pacf(PIB)


###################################################
### code chunk number 28: slides-series-temporelles-M1.Rnw:818-821
###################################################
set.seed(1518289)
ar1.1<- arima.sim(list(order=c(1,0,0),ar=0.75),n=200)
ar1.2<- arima.sim(list(order=c(1,0,0),ar=-0.75),n=200)


###################################################
### code chunk number 29: plot15
###################################################
op<-par(mfrow=c(1,2))
Acf(ar1.1,main=expression(phi[1]==0.75))
Acf(ar1.2,main=expression(phi[1]==-0.75))
par(op)


###################################################
### code chunk number 30: slides-series-temporelles-M1.Rnw:881-884
###################################################
ARpar<-c(0,0.42)
print(ARroots<-polyroot(c(1,-ARpar)))
print(Mod(ARroots))


###################################################
### code chunk number 31: slides-series-temporelles-M1.Rnw:971-975
###################################################
library(forecast)
reg<-Arima(ar1.1,order=c(1,0,0))
library(lmtest)
coeftest(reg)


###################################################
### code chunk number 32: slides-series-temporelles-M1.Rnw:982-984
###################################################
reg<-Arima(ar1.1,order=c(1,0,0),include.mean=FALSE)
coeftest(reg)


###################################################
### code chunk number 33: slides-series-temporelles-M1.Rnw:1008-1011
###################################################
set.seed(123456)
ma1.1<- arima.sim(list(order=c(0,0,1),ma=0.75),n=200)
ma1.2<- arima.sim(list(order=c(0,0,1),ma=-0.75),n=200)


###################################################
### code chunk number 34: plot16
###################################################
op<-par(mfrow=c(1,2))
Acf(ma1.1,main=expression(theta[1]==0.75))
Acf(ma1.2,main=expression(theta[1]==-0.75))
par(op)


###################################################
### code chunk number 35: slides-series-temporelles-M1.Rnw:1046-1048
###################################################
reg1<-Arima(ma1.1,order=c(0,0,1))
coeftest(reg1)


###################################################
### code chunk number 36: slides-series-temporelles-M1.Rnw:1054-1056
###################################################
reg1<-Arima(ma1.1,order=c(0,0,1),include.mean=FALSE)
coeftest(reg1)


###################################################
### code chunk number 37: slides-series-temporelles-M1.Rnw:1102-1104
###################################################
library(TSA)
eacf(BB)


###################################################
### code chunk number 38: slides-series-temporelles-M1.Rnw:1112-1114
###################################################
data(flow)
eacf(flow)


###################################################
### code chunk number 39: slides-series-temporelles-M1.Rnw:1123-1125
###################################################
data(larain)
eacf(larain)


###################################################
### code chunk number 40: slides-series-temporelles-M1.Rnw:1153-1155
###################################################
Tts<-length(XXX[,"TS"]); trend=1:Tts
reg<-lm(XXX[,"TS"]~trend);summary(reg)


###################################################
### code chunk number 41: slides-series-temporelles-M1.Rnw:1161-1162
###################################################
TSstar<-XXX[,"TS"]-reg$coef[1]-trend*reg$coef[2]


###################################################
### code chunk number 42: plot17
###################################################
plot(TSstar,type='l',col=4)


###################################################
### code chunk number 43: slides-series-temporelles-M1.Rnw:1184-1190
###################################################
DSsans<- RW(N=1000,10,0,0.2)
DSavec<- RW(N=1000,10,0.1,0.2)
XX=matrix(cbind(DSsans,DSavec),nrow=1000,ncol=2)
colnames(XX) <- c("DSsans","DSavec")
XX=XX[250:1000,]
XX=ts(XX,start=c(1950,1),freq=12)


###################################################
### code chunk number 44: plot18
###################################################
plot.ts(XX,plot.type = "single",col=1:2)


###################################################
### code chunk number 45: slides-series-temporelles-M1.Rnw:1211-1212
###################################################
DSstar=diff(XXX[,"DS"])


###################################################
### code chunk number 46: plot19
###################################################
plot(DSstar,type='l',col=5)

