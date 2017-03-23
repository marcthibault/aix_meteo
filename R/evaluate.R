library(stats)
library(ggplot2)
library(grid)

transform.ID <- function(residuals, sigma = 1.)
{
  residuals <- residuals / sigma
  residuals
}

transform.MA <- function(residuals, sigma = 1., theta = 0.5)
{
  residuals <- residuals / sigma
  ma <- filter(residuals, filter = c(1, theta), method = c("convolution"), sides = 1, circular = TRUE)
  ma
}

transform.AR <- function(residuals, sigma = 1., phi = 0.5)
{
  residuals <- residuals / sigma
  ar <- filter(residuals, filter = c(1, phi), method = c("recursive"), sides = 1, circular = TRUE)
  ar
}

evaluate.acf <- function(residuals = rt(250, df = 10), sigma = 1., theta = 0.5, phi = 0.5, ...)
{
  residuals <- transform.ID(residuals, ...)

  par(mfrow = c(2,1))
  par(mar = c(3, 3, 3, 3))
  acf(residuals)
  pacf(residuals)
}

plot.series <- function(x, results)
{
  par(mfrow = c(4,1))
  par(mar=c(2, 2, 2, 2))
  plot(x, type = "l")
  par(mar=c(2, 2, 2, 2))
  plot(results[[1]], type = "l")
  par(mar=c(2, 2, 2, 2))
  plot(results[[2]], type = "l")
  par(mar=c(2, 2, 2, 2))
  plot(results[[3]], type = "l")

}


# plot.series(x, model.BB(date, x))
# evaluate.acf(model.BB(date, x)[[3]])
