library(FactoMineR)
library(energy)
library(mvtnorm)
library(ggplot2)
library(data.table)
library(stats)
library(fGarch)


model.AR<- function(date, x) {
  ar_estimated <- ar(x, aic = TRUE, order.max = n, method=c("yule-walker", "burg", "ols", "mle", "yw"))

  residuals <- ar_estimated$resid
  residuals <- residuals[is.na(residuals) == FALSE]
  res <- list()
  res[[3]] <- residuals
  res
}

model.BB <- function(date, x)
{
  datesquare <- date * date
  reg <- lm(x ~ sin(date * 2 * pi / 12) + sin(2 * date * 2 * pi / 12.) + cos(2 * date * 2 * pi / 12.)+ cos(date * 2 * pi / 12.) + date + datesquare)

  res <- list()
  res[[1]] <- reg$coefficients["sin(date * 2 * pi/12)"] * sin(date * 2 * pi / 12) +
                   reg$coefficients["sin(2 * date * 2 * pi/12)"] * sin(2 * date * 2 * pi/12) +
                   reg$coefficients["cos(2 * date * 2 * pi/12)"] * cos(2 * date * 2 * pi/12) +
                   reg$coefficients["cos(date * 2 * pi/12)"] * cos(date * 2 * pi / 12)
  res[[2]] <- reg$coefficients["(Intercept)"]+reg$coefficients["date"]*date+reg$coefficients["datesquare"]*datesquare
  res[[3]] <- c(reg$residuals)
  res
}


model.stl <- function(date, x)
{
  a <- stl(ts(x, frequency = 12), s.window = "periodic")
  res <- list()
  res[[1]] <- a$time.series[, 1]
  res[[2]] <- a$time.series[, 2]
  res[[3]] <- a$time.series[, 3]
  res
}


model.ARIMA <- function(date, x, order_pdq = c(10,3,3))
{

  arima_estimated <- arima(x, order = order_pdq,
    seasonal = list(order = c(0, 0, 0), period = NA),
    xreg = NULL, include.mean = TRUE,
    transform.pars = TRUE,
    fixed = NULL, init = NULL,
    method = c("CSS-ML", "ML", "CSS"))

  residuals <- arima_estimated$resid
  residuals <- residuals[is.na(residuals) == FALSE]
  res <- list()
  res[[3]] <- residuals
  res

}

model.GARCH <- function(date, x)
{
  garch_estimated <- garchFit(~ garch(3, 3), data = x)

  residuals <- garch_estimated@residuals
  residuals <- residuals[is.na(residuals) == FALSE]
  res <- list()
  res[[3]] <- residuals
  res

}


# a <- model.stl(date, x)
