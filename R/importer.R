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


import.file <- function(file = "../data/midas_tempdrnl_201601-201612.txt", station = 57247)
{
  data <- data.table(read.csv(file))
  names(data) <- c("OB_END_TIME", "ID_TYPE", "ID", "OB_HOUR_COUNT", "VERSION_NUM", "MET_DOMAIN_NAME", "SRC_ID", "REC_ST_IND", "MAX_AIR_TEMP", "MIN_AIR_TEMP", "MIN_GRSS_TEMP", "MIN_CONC_TEMP", "MAX_AIR_TEMP_Q", "MIN_AIR_TEMP_Q", "MIN_GRSS_TEMP_Q", "MIN_CONC_TEMP_Q", "METO_STMP_TIME", "MIDAS_STMP_ETIME", "MAX_AIR_TEMP_J", "MIN_AIR_TEMP_J", "MIN_GRSS_TEMP_J", "MIN_CONC_TEMP_J")

  data[, c("ID_TYPE", "ID", "OB_HOUR_COUNT", "VERSION_NUM", "MET_DOMAIN_NAME", "REC_ST_IND", "MIN_AIR_TEMP", "MIN_GRSS_TEMP", "MIN_CONC_TEMP", "MAX_AIR_TEMP_Q", "MIN_AIR_TEMP_Q", "MIN_GRSS_TEMP_Q", "MIN_CONC_TEMP_Q", "METO_STMP_TIME", "MIDAS_STMP_ETIME", "MAX_AIR_TEMP_J", "MIN_AIR_TEMP_J", "MIN_GRSS_TEMP_J", "MIN_CONC_TEMP_J") := NULL]
  # data <- data[SRC_ID == 61937, ]
  data <- data[SRC_ID == station, ]

  data[, day := format(as.Date(OB_END_TIME), "%d")]
  data[, hour := format(as.POSIXct(OB_END_TIME), '%H')]
  data <- data[day == "01", ]
  data <- data[as.double(hour) > 12, ]

  setkey(data, OB_END_TIME)
  data <- unique(data)

  data[, SRC_ID := NULL]
  data[, day := NULL]
  data[, hour := NULL]
  data
}


transform.global <- function(range = c(2010:2016), station = 57247)
{
  data <- data.table()

  years <- as.character(range)
  names.files <- paste0("../data/RAW/midas_tempdrnl_", years, "01-", years, "12.txt")

  for (name in names.files)
  {
    print(name)
    temp <- import.file(name, station = station)
    print(temp)
    data <- rbind(data, temp)
  }
  write.csv(data, file = '../data/final_temperatures.csv')
  archive <<- data
}

transform.global(range = c(1853:2016), station = 9)




import.global <- function()
{
  data <- data.table(read.csv("../data/final_temperatures.csv"))
  data
}
