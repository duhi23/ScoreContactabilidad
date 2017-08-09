########################################################################
#####     Información Gestion Historica - Punto de Observación     #####
########################################################################

library(readr)
library(readxl)
library(dplyr)
library(data.table)
library(stringr)
library(haven)
library(ggplot2)
library(ggvis)

data <- read_sav("GestionesHistoricasCompl.sav")
colnames(data)
# Obtención - Dia de la Gestión
data <- data %>% mutate(DIA=weekdays(as.Date(fecha)))
# Distribución Punto de Observación
data %>% select(-id,-(c_id_historia_gestion:valor_promesa), -servidor, -usuario, -digito_cedula) %>% 
      filter(PTO_OBS==1, REG_IVR==1, REG_VALIDO==1) %>% select(MES) %>% table()
# Cédulas únicas
write_excel_csv(data %>% filter(PTO_OBS==1, REG_IVR==1, REG_VALIDO==1, MES=='09'), 
                path = "Pto_Obs_Sep16.csv", col_names = TRUE)
write_excel_csv(data %>% filter(PTO_OBS==1, REG_IVR==1, REG_VALIDO==1, MES=='12'), 
                path = "Pto_Obs_Dic16.csv", col_names = TRUE)
write_excel_csv(data %>% filter(PTO_OBS==1, REG_IVR==1, REG_VALIDO==1, MES=='03'), 
                path = "Pto_Obs_Mar17.csv", col_names = TRUE)
write_excel_csv(data %>% filter(PTO_OBS==1, REG_IVR==1, REG_VALIDO==1, MES=='06'), 
                path = "Pto_Obs_Jun17.csv", col_names = TRUE)

# Graficos Horario de llamada
data %>% filter(PTO_OBS==1, REG_VALIDO==1, REG_IVR==1, MES=='09', HORAS!='00') %>% select(HORAS) %>% 
      table(.) %>% barplot(main="PTO OBS - Septiembre 2016")
data %>% filter(PTO_OBS==1, REG_VALIDO==1, REG_IVR==1, MES=='12') %>% select(HORAS) %>% 
      table(.) %>% barplot(main="PTO OBS - Diciembre 2016")

# Registros para la generación de variables históricas
# Pto_Obs: Septiembre 2016 - Marzo 2016 - Agosto 2016
data %>% filter(HISTORIA_6M_SEP==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n())
data %>% group_by(cedula, ANIO) %>% summarise(conteo=n())

