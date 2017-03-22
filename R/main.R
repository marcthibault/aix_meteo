library(FactoMineR)
library(energy)
library(mvtnorm)
library(ggplot2)
library(data.table)
library(stats)
library(fGarch)

source("importer.R")
source("models.R")
source("evaluate.R")

temperature <- import.global()


# Simuler s�rie temp, test
# serietemp <- ts(data = rt(250, df = 10), start = 1, frequency = 1)
# plot(serietemp, ylim = c(-5,5), main ="R�alisations de variables al�atoires iid de loi de Student a 10 degres de libert�")

# Simulation de l'ARIMA
# SIMU_ARIMA <- arima.sim(model = list(0,0,0), n = 50,innov = rnorm(n), n.start = NA)
# plot(SIMU_ARIMA)

# Ploter la serie temporelle
# ggplot(temperature[], aes(x = date, y = MAX_AIR_TEMP)) + geom_line()


# On va travailler sur la s�rie teporelle des temp�ratures
x <- temperature[, MAX_AIR_TEMP]
date <- temperature[, X]


# Residus de Buys Ballo
plot.series(x, model.BB(date, x))
evaluate.acf(model.BB(date, x)[[3]])


# Residus de STL
plot.series(x, model.stl(date, x))
evaluate.acf(model.stl(date, x)[[3]])

# Residus de AR
plot.series(x, model.AR(date, x))
evaluate.acf(model.AR(date, x)[[3]])

# Residus de ARIMA
plot.series(x, model.ARIMA(date, x))
evaluate.acf(model.ARIMA(date, x)[[3]])

# appliquer une ARIMA
order_pdq <- c(10,3,3)
arima_estimated <- arima(x, order = order_pdq,
      seasonal = list(order = c(0, 0, 0), period = NA),
      xreg = NULL, include.mean = TRUE,
      transform.pars = TRUE,
      fixed = NULL, init = NULL,
      method = c("CSS-ML", "ML", "CSS"))

acf(arima_estimated$residuals, lag.max = NULL,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE)
pacf(arima_estimated$residuals, lag.max = NULL,plot = TRUE)
