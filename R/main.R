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

# On va travailler sur la serie teporelle des temperatures
x <- temperature[, MAX_AIR_TEMP]
date <- temperature[, X]



# Residus de Buys Ballot
plot.series(x, model.BB(date, x))
evaluate.acf(model.BB(date, x)[[3]])

# Residus de STL
plot.series(x, model.stl(date, x))
evaluate.acf(model.stl(date, x)[[3]])

# Residus de AR
plot.series(x, model.AR(date, x))
evaluate.acf(model.AR(date, x)[[3]])

# Residus de ARIMA
plot.series(x, model.ARIMA(date, x,c(1,0,0)))
evaluate.acf(model.ARIMA(date, x,c(10,3,3))[[3]])

# Residus de GARCH
resid <- model.GARCH(date, x)[[3]]
plot.series(x, model.GARCH(date, x))
evaluate.acf(resid)
verif.Box_pierce.AR(resid,5)



verif.Box_pierce(model.AR(date, x)[[3]],lag=5)

