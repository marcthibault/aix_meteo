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
plot.series(x, model.BB(date, x, verbose = TRUE))
evaluate.acf(model.BB(date, x)[[3]])

# Residus de STL
plot.series(x, model.stl(date, x))
evaluate.acf(model.stl(date, x)[[3]])

# Residus de AR
plot.series(x, model.AR(date, x))
evaluate.acf(model.AR(date, x)[[3]])

# Residus de ARIMA
plot.series(x, model.ARIMA(date, x, order_pdq = c(5,2,12), verbose = TRUE))
evaluate.acf(model.ARIMA(date, x, order_pdq = c(5,2,12), verbose = TRUE)[[3]])

# Resultat de GARCH, sur residus STL
plot.series(x, model.GARCH(date, model.stl(date, x)[[3]], verbose = TRUE))
evaluate.acf(model.GARCH(date, x)[[3]])







#
