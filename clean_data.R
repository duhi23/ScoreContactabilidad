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
library(tidyr)
library(tidyverse)

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

# Pto_Obs: Septiembre 2016 - Marzo 2016 - Agosto 2016 - 6 Meses
res1 <- data %>% filter(HISTORIA_6M_SEP==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(HISTORIA_6M_SEP==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(HISTORIA_6M_SEP==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(HISTORIA_6M_SEP==1) %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_6M_Sep16.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Septiembre 2016 - Junio 2016 - Agosto 2016 - 3 Meses
res1 <- data %>% filter(HISTORIA_3M_SEP==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(HISTORIA_3M_SEP==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(HISTORIA_3M_SEP==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(HISTORIA_3M_SEP==1) %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_3M_Sep16.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Septiembre 2016 - Agosto 2016 - 1 Mes
res1 <- data %>% filter(MES=='08', ANIO=='2016') %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(MES=='08', ANIO=='2016') %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(MES=='08', ANIO=='2016') %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(MES=='08', ANIO=='2016') %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_1M_Sep16.csv", col_names = TRUE)
rm(list=c("cons3"))


# Pto_Obs: Diciembre 2016 - Junio 2016 - Noviembre 2016 - 6 Meses
res1 <- data %>% filter(HISTORIA_6M_DIC==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(HISTORIA_6M_DIC==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(HISTORIA_6M_DIC==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(HISTORIA_6M_DIC==1) %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_6M_Dic16.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Diciembre 2016 - Septiembre 2016 - Noviembre 2016 - 3 Meses
res1 <- data %>% filter(HISTORIA_3M_DIC==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(HISTORIA_3M_DIC==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(HISTORIA_3M_DIC==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(HISTORIA_3M_DIC==1) %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_3M_Dic16.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Diciembre 2016 - Noviembre 2016 - 1 Mes
res1 <- data %>% filter(MES=='11', ANIO=='2016') %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(MES=='11', ANIO=='2016') %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(MES=='11', ANIO=='2016') %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(MES=='11', ANIO=='2016') %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_1M_Dic16.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Marzo 2017 - Septiembre 2016 - Febrero 2017 - 6 Meses
res1 <- data %>% filter(HISTORIA_6M_MAR==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(HISTORIA_6M_MAR==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(HISTORIA_6M_MAR==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(HISTORIA_6M_MAR==1) %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_6M_Mar17.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Marzo 2017 - Diciembre 2016 - Febrero 2017 - 3 Meses
res1 <- data %>% filter(HISTORIA_3M_MAR==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(HISTORIA_3M_MAR==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(HISTORIA_3M_MAR==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(HISTORIA_3M_MAR==1) %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_3M_Mar17.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Marzo 2017 - Febrero 2017 - 1 Mes
res1 <- data %>% filter(MES=='02', ANIO=='2017') %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(MES=='02', ANIO=='2017') %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(MES=='02', ANIO=='2017') %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(MES=='02', ANIO=='2017') %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_1M_Mar17.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Junio 2017 - Diciembre 2016 - Mayo 2017 - 6 Meses
res1 <- data %>% filter(HISTORIA_6M_JUN==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(HISTORIA_6M_JUN==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(HISTORIA_6M_JUN==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(HISTORIA_6M_JUN==1) %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_6M_Jun17.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Junio 2017 - Marzo 2017 - Mayo 2017 - 3 Meses
res1 <- data %>% filter(HISTORIA_3M_JUN==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(HISTORIA_3M_JUN==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(HISTORIA_3M_JUN==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(HISTORIA_3M_JUN==1) %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_3M_Jun17.csv", col_names = TRUE)
rm(list=c("cons3"))

# Pto_Obs: Junio 2017 - Mayo 2017 - 1 Mes
res1 <- data %>% filter(MES=='05', ANIO=='2017') %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
res2 <- data %>% filter(MES=='05', ANIO=='2017') %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro)
cons1 <- left_join(res1, res2, by = "cedula")
rm(list=c("res1", "res2"))

res3 <- data %>% filter(MES=='05', ANIO=='2017') %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
cons2 <- left_join(cons1, res3, by = "cedula")
rm(list=c("res3", "cons1"))

res4 <- data %>% filter(MES=='05', ANIO=='2017') %>% mutate(rango_hora=cut(parse_double(HORAS),
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% 
      summarise(conteo=n()) %>% spread(key = rango_hora, value = conteo)
cons3 <- left_join(cons2, res4, by = "cedula")
rm(list=c("res4", "cons2"))
write_excel_csv(cons3, path = "Var_Hist_1M_Jun17.csv", col_names = TRUE)
rm(list=c("cons3"))