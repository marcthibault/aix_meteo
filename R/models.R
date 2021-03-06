library(FactoMineR)
library(energy)
library(mvtnorm)
library(ggplot2)
library(data.table)
library(stats)
library(fGarch)

model.MA <- function(date, x) {
  ma_estimated <- decompose(x,c("additive"),filter=NULL)
  residuals <- ar_estimated$resid
  residuals <- residuals[is.na(residuals) == FALSE]
  res <- list()
  res[[3]] <- residuals
  res
}

model.AR<- function(date, x, n = 12) {
  ar_estimated <- ar(x, aic = TRUE, order.max = n, method=c("yule-walker", "burg", "ols", "mle", "yw"))

  residuals <- ar_estimated$resid
  residuals <- residuals[is.na(residuals) == FALSE]
  res <- list()
  res[[3]] <- residuals
  res
}

model.BB <- function(date, x, verbose = FALSE)
{
  datesquare <- date * date
  reg <- lm(x ~ sin(date * 2 * pi / 12) + sin(2 * date * 2 * pi / 12.) + cos(2 * date * 2 * pi / 12.)+ cos(date * 2 * pi / 12.) + date + datesquare)

  if(verbose)
  {
    print(reg$coefficients)
  }
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


model.ARIMA <- function(date, x, order_pdq = c(10,3,3), verbose = FALSE)
{
  arima_estimated <- arima(x, order = order_pdq,
    seasonal = list(order = c(1, 0, 0), period = NA),
    xreg = NULL, include.mean = TRUE,
    transform.pars = TRUE,
    fixed = NULL, init = NULL,
    method = c("CSS-ML", "ML", "CSS"))

  if (verbose)
  {
    print(arima_estimated)
  }
  residuals <- arima_estimated$resid
  residuals <- residuals[is.na(residuals) == FALSE]
  res <- list()
  res[[3]] <- residuals
  res
}

model.GARCH <- function(date, x, verbose = FALSE)
{
  garch_estimated <- garchFit(~ garch(5, 5), data = x)

  if(verbose)
  {
    print(garch_estimated)
  }
  residuals <- garch_estimated@residuals
  residuals <- residuals[is.na(residuals) == FALSE]
  res <- list()
  res[[3]] <- residuals
  return(res)
}


verif.Box_pierce <- function(residuals,lag = 10)
{
  return(Box.test(residuals, lag, type = c("Box-Pierce"), fitdf = 0))
}
