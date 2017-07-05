# Historia Gestion

library(readr)
library(dplyr)
library(data.table)
library(stringr)

dir <- getwd()

setwd(paste(dir,"/historia_gestion/SAC10/", sep=""))
SAC10 <- read_csv("historia_gestion_2016_01_01.csv", col_names = FALSE)[-20,3:14]
View(SAC10)

setwd(paste(dir,"/historia_gestion/SAC09/", sep=""))
SAC09 <- read_csv("historia_gestion_2016_01_01.csv", col_names = FALSE)[,3:15]
View(SAC09)

setwd(paste(dir,"/historia_gestion/SAC07/", sep=""))
SAC07 <- read_csv("historia_gestion_2016_01_01.csv", col_names = FALSE)[,3:15]
View(SAC07)

setwd(paste(dir,"/historia_gestion/APP06/", sep=""))
APP06 <- read_csv("historia_gestion_2016_01_01.csv", col_names = FALSE)[,3:11]
View(APP06)


save(list = ls(all.names = TRUE), file = "historia.RData", envir = .GlobalEnv)


# DOCUMENTO DE IDENTIFICACION
table(nchar(SAC10$X3))
barplot(table(nchar(SAC10$X3)))

SAC10$digito <- nchar(SAC10$X3)
SAC10$dia <- weekdays(as.Date(SAC10$X4))
SAC10$mes <- months(as.Date(SAC10$X4))
SAC10$anio <- substr(SAC10$X4, 1,4)
SAC10$hora <- substr(SAC10$X4, 12,13)

# Anio 2016-2017
SAC10 <- SAC10[SAC10$anio %in% c("2016", "2017"),]
SAC10[SAC10$digito %in% c("7", "8", "10", "11", "13"),]


table(SAC10$digito)
table(SAC10$mes)


View(tail(SAC10))
