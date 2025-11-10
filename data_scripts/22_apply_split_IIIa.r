

library(dplyr)
library(sqldf)

options("scipen" = 1000)
path_data <- "./data/"


#Read in and recode split proportions from Henrik
#Read-in

split <-
  read.table(
    paste0(
      path_data,
      "11_updated_split_2000_2024_SD20SD21.csv"
    ),
    sep = ";",
    header = T
  )
str(split)

unique(split$area)

unique(split$wr)


#Read in CANUM

canum <-
  read.table(paste0(
    path_data,
    "21_updated_canum_with_imputations_2000_2024.csv"),
    sep = ";", header = T
  )

#Apply split proportions

splitted <- merge(canum, split, all.x = T)

splitted$wbss <-
  ifelse(splitted$area %in% c("27.3.b.23", "27.3.c.22", "27.3.d.24"),
         1,
         splitted$wbss)
splitted$nsas <-
  ifelse(splitted$area %in% c("27.3.b.23", "27.3.c.22", "27.3.d.24"),
         0,
         splitted$nsas)

splitted$nsas_canum_1000 <- splitted$canum_1000 * splitted$nsas

splitted$wbss_canum_1000 <- splitted$canum_1000 * splitted$wbss

splitted <- arrange(splitted, year, quarter, area, ctry, fleet, wr)

splitted$nsas_canum_1000[is.na(splitted$nsas_canum_1000)] <- 0

splitted$wbss_canum_1000[is.na(splitted$wbss_canum_1000)] <- 0


#Output

#WBSS and NSAS
write.table(
  splitted,
  paste(path_data, "22_updated_canum_wbss_nsas_2000-2024.csv", sep = ""),
  sep = ",",
  row.names = F
)
saveRDS(splitted,
        paste(path_data, "22_updated_canum_wbss_nsas_2000-2024.rsd", sep = ""))



