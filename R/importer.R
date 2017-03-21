library(data.table)

import <- function()
{
  data = data.table(read.csv("../data/Temperatures_Aix.csv"))

  data[, City := NULL]
  data[, Latitude := NULL]
  data[, Longitude := NULL]
  data[, Country := NULL]

  data[, date := 12 * (annee - 1743) + mois]
  data
}

data = import()



