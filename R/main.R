library(FactoMineR)
library(energy)
library(mvtnorm)
library(ggplot2)
library(data.table)
library(stats)

source("importer.R")
temperature <- import()
temperature <- temperature[is.na(temperature[,AverageTemperature]) == FALSE]


# Créer série temp, test
serietemp <- ts(data = rt(250, df = 10), start = 1, frequency = 1)
plot(serietemp, ylim = c(-5,5), main ="Réalisations de variables aléatoires iid de loi de Student a 10 degres de liberté")

# Ploter la serie temporelle
ggplot(temperature[], aes(x = date, y = AverageTemperature)) + geom_line()


x <- temperature[,AverageTemperature]
date <- temperature[,date]


# appliquer une MA
a <- filter(x, filter=c(1,1), method = c("convolution"), sides = 2, circular = FALSE)

# appliquer un AR
ar_estimated <- ar(x, aic = TRUE, order.max = 5, method=c("yule-walker", "burg", "ols", "mle", "yw"))
acf(ar_estimated$ar, lag.max = NULL,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE)

# appliquer une ARIMA
predict(x, se.fit = FALSE, scale = NULL, df = Inf,interval = c("none", "confidence", "prediction"),level = 0.95, type = c("response", "terms"),terms = NULL, na.action = na.pass,pred.var = res.var/weights, weights = 1)

model_AR<- function() {
  X <- x
  for (i in 1:5) {
    a <- x[(i+1):length(x)]
    for (j in 1:i) {
        a[[length(a) + 1]] = 0
    }
    
    X <- X - ar_estimated$ar[i]*a
  }
}
X
acf(X, lag.max = NULL,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE)






datesquare <- date*date
b <- lm(x ~ sin(date*2*pi/12.)+sin(2*date*2*pi/12.)+cos(2*date*2*pi/12.)+ cos(date*2*pi/12.) + date + datesquare)
plot(x[2000:2080])
lines(b$fitted.values[2000:2080])

# Afficher la trend
plot(b$coefficients["(Intercept)"]+b$coefficients["date"]*date+b$coefficients["datesquare"]*datesquare)

# ACF et PACF
acf(b$residuals, lag.max = NULL,type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE)
pacf(b$residuals, lag.max = NULL, plot = TRUE)



SIMU_ARIMA <- arima.sim(model = list(0,0,0), n = 50, rand.gen = rnorm,innov = rand.gen(n), n.start = NA)
plot(SIMU_ARIMA)


stl(x, s.window = "periodic", s.degree = 0, t.window = NULL, t.degree = 1, l.window = nextodd(period), l.degree = t.degree, s.jump = ceiling(s.window/10), t.jump = ceiling(t.window/10),l.jump = ceiling(l.window/10), robust = FALSE)



x <- ts(x, start = c(2000,1), end=c(2050,51), frequency = 51)

decomposition <- decompose(x, type = c("multiplicative"),filter = NULL)









