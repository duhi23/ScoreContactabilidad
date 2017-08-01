########################################################################
#####     Información Gestion Historica - Punto de Observación     #####
########################################################################

library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(haven)

data <- read_sav("Info_Pto_Obs.sav")
data$DIA <- weekdays(as.Date(data$fecha))
data %>% filter(MES=='02') %>% dim(.)

write_excel_csv(data[data$MES=='02',], path = "Pto_obs_febrero.csv", col_names = TRUE)
