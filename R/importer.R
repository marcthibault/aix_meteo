library(data.table)

import <- function()
{
  data = data.table(read.csv("data/Temperatures_Aix.csv"))

  data[, City := NULL]
  data[, Latitude := NULL]
  data[, Longitude := NULL]
  data[, Country := NULL]

  data
  data
}

data = import()
