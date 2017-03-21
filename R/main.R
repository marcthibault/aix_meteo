library(FactoMineR)
library(energy)
library(mvtnorm)
library(ggplot2)
library(data.table)

source("importer.R")
temperature <- import()



ggplot(temperature,aes(x=mois,y=AverageTemperature)) + geom_point()
View(temperature)
