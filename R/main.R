library(FactoMineR)
library(energy)
library(mvtnorm)
library(ggplot2)
library(data.table)
library(stats)
library(fGarch)

source("importer.R")
source("models.R")

temperature <- import.global()
temperature <- temperature[is.na(temperature[, MAX_AIR_TEMP]) == FALSE]


# Simuler série temp, test
serietemp <- ts(data = rt(250, df = 10), start = 1, frequency = 1)
plot(serietemp, ylim = c(-5,5), main ="Réalisations de variables aléatoires iid de loi de Student a 10 degres de liberté")

# Simulation de l'ARIMA
SIMU_ARIMA <- arima.sim(model = list(0,0,0), n = 50,innov = rnorm(n), n.start = NA)
plot(SIMU_ARIMA)

# Ploter la serie temporelle
ggplot(temperature[], aes(x = date, y = MAX_AIR_TEMP)) + geom_line()


# On va travailler sur la série teporelle des températures
x <- temperature[,MAX_AIR_TEMP]
date <- temperature[,X]


# appliquer une MA
a <- filter(x, filter=c(1,1), method = c("convolution"), sides = 2, circular = FALSE)



# On applique le modèle de l'AR, et on va ploter l'ACF et la PACF de l'innovation
innovation <- model_AR(20)
acf(innovation, lag.max = NULL,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE)
pacf(innovation, lag.max = NULL,plot = TRUE)



# Méthode de Buys-Ballot
datesquare <- date*date
b <- lm(x ~ sin(date*2*pi/12.)+sin(2*date*2*pi/12.)+cos(2*date*2*pi/12.)+ cos(date*2*pi/12.) + date + datesquare)
plot(x)
lines(b$fitted.values)

# Afficher la trend
plot(b$coefficients["(Intercept)"]+b$coefficients["date"]*date+b$coefficients["datesquare"]*datesquare)

# ACF et PACF pour les résidus de Buys Ballot
acf(b$residuals, lag.max = NULL,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE)
pacf(b$residuals, lag.max = NULL, plot = TRUE)
plot(date*b$coefficients["date"]+datesquare*b$coefficients["datesquare"])


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




stl(x, s.window = "periodic", s.degree = 0, t.window = NULL, t.degree = 1, l.window = nextodd(period), l.degree = t.degree, s.jump = ceiling(s.window/10), t.jump = ceiling(t.window/10),l.jump = ceiling(l.window/10), robust = FALSE)




decomposition <- decompose(x, type = c("multiplicative"),filter = NULL)









