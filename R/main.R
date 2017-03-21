library(FactoMineR)
library(energy)
library(mvtnorm)
library(ggplot2)
library(data.table)
library(stats)

source("importer.R")
temperature <- import()

# Créer série temp, test
serietemp <- ts(data = rt(250, df = 10), start = 1, frequency = 1)
plot(serietemp, ylim = c(-5,5), main ="Réalisations de variables aléatoires iid de loi de Student a 10 degres de liberté")


ggplot(temperature[], aes(x = date, y = AverageTemperature)) + geom_line()


# appliquer une MA
a <- filter(temperature[, AverageTemperature], filter=c(1,1), method = c("convolution"), sides = 2, circular = FALSE)



