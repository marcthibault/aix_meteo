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


<<<<<<< HEAD
# Residus de Buys Ballo
plot.series(x, model.BB(date, x, verbose = TRUE))
=======

# Residus de Buys Ballot
plot.series(x, model.BB(date, x))
>>>>>>> f14b2f0d7b1e38ad926ce16cf1cae16b0be88de9
evaluate.acf(model.BB(date, x)[[3]])

# Residus de STL
plot.series(x, model.stl(date, x))
evaluate.acf(model.stl(date, x)[[3]])

# Residus de AR
plot.series(x, model.AR(date, x))
evaluate.acf(model.AR(date, x)[[3]])
verif.Box_pierce(model.AR(date, x)[[3]],lag=5)

# Residus de ARIMA
plot.series(x, model.ARIMA(date, x, order_pdq = c(5,2,12), verbose = TRUE))
evaluate.acf(model.ARIMA(date, x, order_pdq = c(5,2,12), verbose = TRUE)[[3]])

# Resultat de GARCH, sur residus STL
plot.series(x, model.GARCH(date, model.stl(date, x)[[3]], verbose = TRUE))
evaluate.acf(model.GARCH(date, x)[[3]])

# Residus de GARCH - jeannerod
resid <- model.GARCH(date, x)[[3]]
plot.series(x, model.GARCH(date, x))
evaluate.acf(resid)
verif.Box_pierce.AR(resid,5)
