# Appel des librairies ----
setwd("/Users/josephbarbier/Desktop/M1S2/série temp/projet")
rm(list=ls())
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(urca)
library(stats)
library(fpp2)
library(forecast)
library(lmtest)
library(TSA)
library(tseries)
library(knitr)
library(CADFtest)
library(foreach)
library(doSNOW)
library(parallel)

# Ouverture des données ----
savings = t(read_excel("taux-epargne.xls"))
colnames(savings) = paste(as.character(savings[1,]),as.character(savings[2,])) 
savings = as.data.frame(savings[-c(1,2),])
savings = subset(savings, select = c("Netherlands NLD", "Country Name Country Code"))
colnames(savings) = c("Saving rate", "Date")
savings = subset(savings, Date > 1968)
rownames(savings) = 1:nrow(savings)
savings$Date = as.integer(savings$Date)
savings$`Saving rate` = as.numeric(savings$`Saving rate`)
ts = ts(savings$`Saving rate`, freq=1, start=1969)

# Graphique introduction ----
savings %>%
  ggplot( aes(x=`Saving rate`)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  theme_classic() +
  ggtitle("")

savings %>% 
  ggplot(aes(x=Date, y=`Saving rate`)) +
  geom_line(color="#69b3a2") +
  ylim(20,33) +
  annotate(geom="text", x=1974, y=30.5, label="Première crise pétrolière") +
  annotate(geom="point", x=1973, y=30, size=10, shape=21, fill="transparent") +
  annotate(geom="text", x=1976.5, y=23.5, label="Deuxième crise pétrolière") +
  annotate(geom="point", x=1978, y=22.8, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=median(savings$`Saving rate`), color="orange", size=.5) + 
  geom_segment(x = 2016, y = 23, xend = 2019, yend = 25.5, arrow = arrow(length = unit(0.3, "cm"))) +
  annotate(geom="text", x=2015, y=22.5, label="Taux d'épargne médian au cours de la période") +
  annotate(geom="point", x=2007, y=27, size=10, shape=21, fill="transparent") +
  annotate(geom="text", x=2007, y=27.5, label="Crise des subprimes") +
  theme_bw()

# Test de DF et ADF (bonne spécification) ----
summary(ur.df(ts, type = "trend", lags = 0))
plot(ur.df(ts, type = "trend", lags = 0))
pmax = as.integer(12*(length(ts)/100)^(0.25)) #formule de Schwert
summary(CADFtest(ts, criterion="MAIC", type="trend", max.lag.y=pmax))
summary(ur.df(ts, type = "trend", lags = pmax-1, selectlags = "BIC"))
plot(ur.df(ts, type = "trend", lags = pmax-1))

# Test de ZA (bonne spécification) ----
summary(ur.za(ts, model="intercept", lag=pmax-1))

# Test de LS ----

source("~/Desktop/M1S2/série temp/LeeStrazicichUnitRoot-master/LeeStrazicichUnitRootTest.R", encoding = 'UTF-8')
source("~/Desktop/M1S2/série temp/LeeStrazicichUnitRoot-master/LeeStrazicichUnitRootTestParallelization.R", encoding = 'UTF-8')
cl = makeCluster(max(1, detectCores() - 1))
registerDoSNOW(cl)
myLS_test = ur.ls.bootstrap(y=ts, model="crash", breaks=1, lags=pmax-5,
                            method="GTOS", critval="bootstrap", print.results="print")
myLS_test = ur.ls.bootstrap(ts, model="crash", breaks=2, lags=pmax-5,
                            method="GTOS", critval="bootstrap", print.results="print")

# Différenciation de la série et graphique de la série différenciée ----

ts_diff = diff(ts)
savingsDiff = savings[2:nrow(savings),]
savingsDiff$SavingDiff = ts_diff
savingsDiff %>% 
  ggplot(aes(x=Date, y=SavingDiff)) +
  geom_line(color="darkblue") +
  theme_bw() +
  ggtitle("Taux d'épargne de la Hollande différencié à l'ordre 1")

# Détection d'autocorrélation ----

ggAcf(ts_diff) + theme_bw() + ggtitle("") #ACF
ggPacf(ts_diff) + theme_bw() + ggtitle("") #PACF
Box.test(ts_diff, lag = 9, type = "Ljung-Box") #Ljung-Box
eacf(ts_diff) #EACF

# ARMA ----

model = Arima(ts_diff,
              order=c(9,0,9),
              include.mean=TRUE) #première spécification
coeftest(model)

model = Arima(ts_diff,
              order=c(9,0,9),
              include.mean=FALSE,
              fixed=c(NA, NA, 0, NA, NA, 0, NA, NA, 0, 0, NA, 0, NA, NA, 0, NA, NA, NA)) # bonne spécification
coeftest(model)

# Tests sur les résidus ----

residuals = model$residuals
t.test(residuals)
Box.test(residuals, lag = 15)
jarque.bera.test(residuals)

# Prévision ----

#on definit la prévision
h = 5
prev = forecast(model, h=h, level=0.95)
prev = data.frame(prev)
prev$Date = rownames(prev)
names(prev)[1] = "ts_diff"

#df complet
ts_diff = as.numeric(ts_diff)
df_all = data.frame(ts_diff)
df_all$Lo.95 = NA
df_all$Hi.95 = NA
df_all$Date = seq(1970,2021)
df_all = rbind(df_all, prev)
df_all$Date = as.integer(df_all$Date)

#definition de la zone d'IC
last = tail(df_all, h)

#graphique
df_all %>%
  ggplot(aes(x=Date, y=ts_diff)) +
  geom_line() +
  geom_ribbon(data = last, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "lightblue", alpha = 0.5) +
  xlab("Date") +
  ylab("Value") +
  theme_bw()

for (i in seq(1,h)){
  cat("Prédiction pour", last$Date[i], "(de la série différenciée) :",
      round(last$ts_diff[i],2), "(95% IC : [", round(last$Lo.95[i],2), ",", round(last$Hi.95[i],2), "])\n\n")
}

# Annexe ----

#DF test
summary(ur.df(ts, type = "trend", lags = pmax, selectlags = "BIC"))
plot(ur.df(ts, type = "trend", lags = pmax, selectlags = "BIC"))

#ZA test
summary(ur.za(ts, model="both", lag=pmax-1))

#LJ test
lag = 1
repeat {
  LJ.test = Box.test(ts, lag = lag, type = "Ljung-Box")
  if (LJ.test$p.value < 0.05) {
    break
  } else {
    lag = lag + 1
  }
}
print(LJ.test)
cat("Premier lag significatif :", lag)

#ARMA
model = Arima(ts_diff, order=c(9,0,9), include.mean=TRUE)
coeftest(model)
model = Arima(ts_diff, order=c(9,0,9), include.mean = FALSE)
coeftest(model)

