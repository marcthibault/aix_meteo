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

evaluate.acf <- function(residuals, sigma = 1., theta = 0.5, phi = 0.5, ...)
{
  id <- transform.ID(residuals, ...)
  ma <- transform.MA(residuals, ...)
  ar <- transform.AR(residuals, ...)

  dev.new()
  acf(id)
  dev.new()
  acf(ma)
  dev.new()
  acf(ar)
}

x <- rt(250, df = 10)
evaluate.acf(x)







# 
