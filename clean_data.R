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


### Registros para la generación de variables históricas

# Pto_Obs: Septiembre 2016 - Marzo 2016 - Agosto 2016 - 6 Meses
reg_sep16_6m <- data %>% filter(ANIO=="2016", MES=="09", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(HISTORIA_6M_SEP==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_6m", "num_llamada_6m", "num_sms_6m", "num_emails_6m", "num_visita_6m")
cons1 <- left_join(reg_sep16_6m, res1, by = "cedula")
rm(list=c("reg_sep16_6m", "res1"))

res2 <- data %>% filter(HISTORIA_6M_SEP==1, NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_6m", "nllamada_directa_6m", "nllamada_indirecta_6m", "nllamada_nocontacto_6m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(HISTORIA_6M_SEP==1, NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_6m", "nvisita_directa_6m", "nvisita_indirecta_6m", "nvisita_nocontacto_6m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(HISTORIA_6M_SEP==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_6m", "max_valpro_visita_6m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(HISTORIA_6M_SEP==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_6m", "min_llamada_6m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(HISTORIA_6M_SEP==1, NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_6m", "nllamada_martes_6m", "nllamada_miercoles_6m", 
                    "nllamada_jueves_6m", "nllamada_viernes_6m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(HISTORIA_6M_SEP==1, NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_6m", "nvisita_martes_6m", "nvisita_miercoles_6m", 
                    "nvisita_jueves_6m", "nvisita_viernes_6m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(HISTORIA_6M_SEP==1, NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_6m", "nllamada_1012_6m", "nllamada_1315_6m", "nllamada_1618_6m",
                    "nllamada_1921_6m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(HISTORIA_6M_SEP==1, NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_6m", "nvisita_1012_6m", "nvisita_1315_6m", "nvisita_1618_6m",
                    "nvisita_1921_6m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_6M_Sep16.csv", col_names = TRUE)
rm(list=c("cons9"))


# Pto_Obs: Septiembre 2016 - Junio 2016 - Agosto 2016 - 3 Meses
reg_sep16_3m <- data %>% filter(ANIO=="2016", MES=="09", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(HISTORIA_3M_SEP==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_3m", "num_llamada_3m", "num_sms_3m", "num_emails_3m", "num_visita_3m")
cons1 <- left_join(reg_sep16_3m, res1, by = "cedula")
rm(list=c("reg_sep16_3m", "res1"))

res2 <- data %>% filter(HISTORIA_3M_SEP==1, NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_3m", "nllamada_directa_3m", "nllamada_indirecta_3m", "nllamada_nocontacto_3m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(HISTORIA_3M_SEP==1, NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_3m", "nvisita_directa_3m", "nvisita_indirecta_3m", "nvisita_nocontacto_3m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(HISTORIA_3M_SEP==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_3m", "max_valpro_visita_3m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(HISTORIA_3M_SEP==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_3m", "min_llamada_3m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(HISTORIA_3M_SEP==1, NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_3m", "nllamada_martes_3m", "nllamada_miercoles_3m", 
                    "nllamada_jueves_3m", "nllamada_viernes_3m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(HISTORIA_3M_SEP==1, NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_3m", "nvisita_martes_3m", "nvisita_miercoles_3m", 
                    "nvisita_jueves_3m", "nvisita_viernes_3m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(HISTORIA_3M_SEP==1, NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_3m", "nllamada_1012_3m", "nllamada_1315_3m", "nllamada_1618_3m",
                    "nllamada_1921_3m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(HISTORIA_3M_SEP==1, NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_3m", "nvisita_1012_3m", "nvisita_1315_3m", "nvisita_1618_3m",
                    "nvisita_1921_3m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_3M_Sep16.csv", col_names = TRUE)
rm(list=c("cons9"))

# Pto_Obs: Septiembre 2016 - Agosto 2016 - 1 Mes
reg_sep16_1m <- data %>% filter(ANIO=="2016", MES=="09", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(ANIO=="2016", MES=="08") %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_1m", "num_llamada_1m", "num_sms_1m", "num_emails_1m", "num_visita_1m")
cons1 <- left_join(reg_sep16_1m, res1, by = "cedula")
rm(list=c("reg_sep16_1m", "res1"))

res2 <- data %>% filter(ANIO=="2016", MES=="08", NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_1m", "nllamada_directa_1m", "nllamada_indirecta_1m", "nllamada_nocontacto_1m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(ANIO=="2016", MES=="08", NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_1m", "nvisita_directa_1m", "nvisita_indirecta_1m", "nvisita_nocontacto_1m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(ANIO=="2016", MES=="08") %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_1m", "max_valpro_visita_1m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(ANIO=="2016", MES=="08", NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_1m", "min_llamada_1m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(ANIO=="2016", MES=="08", NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_1m", "nllamada_martes_1m", "nllamada_miercoles_1m", 
                    "nllamada_jueves_1m", "nllamada_viernes_1m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(ANIO=="2016", MES=="08", NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_1m", "nvisita_martes_1m", "nvisita_miercoles_1m", 
                    "nvisita_jueves_1m", "nvisita_viernes_1m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(ANIO=="2016", MES=="08", NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_1m", "nllamada_1012_1m", "nllamada_1315_1m", "nllamada_1618_1m",
                    "nllamada_1921_1m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(ANIO=="2016", MES=="08", NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_1m", "nvisita_1012_1m", "nvisita_1315_1m", "nvisita_1618_1m",
                    "nvisita_1921_1m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_1M_Sep16.csv", col_names = TRUE)
rm(list=c("cons9"))

# Pto_Obs: Diciembre 2016 - Junio 2016 - Noviembre 2016 - 6 Meses
reg_dic16_6m <- data %>% filter(ANIO=="2016", MES=="12", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(HISTORIA_6M_DIC==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_6m", "num_llamada_6m", "num_sms_6m", "num_emails_6m", "num_visita_6m")
cons1 <- left_join(reg_dic16_6m, res1, by = "cedula")
rm(list=c("reg_dic16_6m", "res1"))

res2 <- data %>% filter(HISTORIA_6M_DIC==1, NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_6m", "nllamada_directa_6m", "nllamada_indirecta_6m", "nllamada_nocontacto_6m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(HISTORIA_6M_DIC==1, NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_6m", "nvisita_directa_6m", "nvisita_indirecta_6m", "nvisita_nocontacto_6m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(HISTORIA_6M_DIC==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_6m", "max_valpro_visita_6m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(HISTORIA_6M_DIC==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_6m", "min_llamada_6m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(HISTORIA_6M_DIC==1, NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_6m", "nllamada_martes_6m", "nllamada_miercoles_6m", 
                    "nllamada_jueves_6m", "nllamada_viernes_6m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(HISTORIA_6M_DIC==1, NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_6m", "nvisita_martes_6m", "nvisita_miercoles_6m", 
                    "nvisita_jueves_6m", "nvisita_viernes_6m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(HISTORIA_6M_DIC==1, NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_6m", "nllamada_1012_6m", "nllamada_1315_6m", "nllamada_1618_6m",
                    "nllamada_1921_6m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(HISTORIA_6M_DIC==1, NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_6m", "nvisita_1012_6m", "nvisita_1315_6m", "nvisita_1618_6m",
                    "nvisita_1921_6m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_6M_Dic16.csv", col_names = TRUE)
rm(list=c("cons9"))

# Pto_Obs: Diciembre 2016 - Septiembre 2016 - Noviembre 2016 - 3 Meses
reg_dic16_3m <- data %>% filter(ANIO=="2016", MES=="12", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(HISTORIA_3M_DIC==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_3m", "num_llamada_3m", "num_sms_3m", "num_emails_3m", "num_visita_3m")
cons1 <- left_join(reg_dic16_3m, res1, by = "cedula")
rm(list=c("reg_dic16_3m", "res1"))

res2 <- data %>% filter(HISTORIA_3M_DIC==1, NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_3m", "nllamada_directa_3m", "nllamada_indirecta_3m", "nllamada_nocontacto_3m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(HISTORIA_3M_DIC==1, NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_3m", "nvisita_directa_3m", "nvisita_indirecta_3m", "nvisita_nocontacto_3m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(HISTORIA_3M_DIC==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_3m", "max_valpro_visita_3m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(HISTORIA_3M_DIC==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_3m", "min_llamada_3m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(HISTORIA_3M_DIC==1, NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_3m", "nllamada_martes_3m", "nllamada_miercoles_3m", 
                    "nllamada_jueves_3m", "nllamada_viernes_3m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(HISTORIA_3M_DIC==1, NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_3m", "nvisita_martes_3m", "nvisita_miercoles_3m", 
                    "nvisita_jueves_3m", "nvisita_viernes_3m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(HISTORIA_3M_DIC==1, NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_3m", "nllamada_1012_3m", "nllamada_1315_3m", "nllamada_1618_3m",
                    "nllamada_1921_3m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(HISTORIA_3M_DIC==1, NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_3m", "nvisita_1012_3m", "nvisita_1315_3m", "nvisita_1618_3m",
                    "nvisita_1921_3m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_3M_Dic16.csv", col_names = TRUE)
rm(list=c("cons9"))

# Pto_Obs: Diciembre 2016 - Noviembre 2016 - 1 Mes
reg_dic16_1m <- data %>% filter(ANIO=="2016", MES=="12", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(ANIO=="2016", MES=="11") %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_1m", "num_llamada_1m", "num_sms_1m", "num_emails_1m", "num_visita_1m")
cons1 <- left_join(reg_dic16_1m, res1, by = "cedula")
rm(list=c("reg_dic16_1m", "res1"))

res2 <- data %>% filter(ANIO=="2016", MES=="11", NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_1m", "nllamada_directa_1m", "nllamada_indirecta_1m", "nllamada_nocontacto_1m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(ANIO=="2016", MES=="11", NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_1m", "nvisita_directa_1m", "nvisita_indirecta_1m", "nvisita_nocontacto_1m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(ANIO=="2016", MES=="11") %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_1m", "max_valpro_visita_1m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(ANIO=="2016", MES=="11", NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_1m", "min_llamada_1m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(ANIO=="2016", MES=="11", NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_1m", "nllamada_martes_1m", "nllamada_miercoles_1m", 
                    "nllamada_jueves_1m", "nllamada_viernes_1m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(ANIO=="2016", MES=="11", NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_1m", "nvisita_martes_1m", "nvisita_miercoles_1m", 
                    "nvisita_jueves_1m", "nvisita_viernes_1m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(ANIO=="2016", MES=="11", NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_1m", "nllamada_1012_1m", "nllamada_1315_1m", "nllamada_1618_1m",
                    "nllamada_1921_1m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(ANIO=="2016", MES=="11", NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_1m", "nvisita_1012_1m", "nvisita_1315_1m", "nvisita_1618_1m",
                    "nvisita_1921_1m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_1M_Dic16.csv", col_names = TRUE)
rm(list=c("cons9"))


# Pto_Obs: Marzo 2017 - Septiembre 2016 - Febrero 2017 - 6 Meses
reg_mar17_6m <- data %>% filter(ANIO=="2017", MES=="03", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(HISTORIA_6M_MAR==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_6m", "num_llamada_6m", "num_sms_6m", "num_emails_6m", "num_visita_6m")
cons1 <- left_join(reg_mar17_6m, res1, by = "cedula")
rm(list=c("reg_mar17_6m", "res1"))

res2 <- data %>% filter(HISTORIA_6M_MAR==1, NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_6m", "nllamada_directa_6m", "nllamada_indirecta_6m", "nllamada_nocontacto_6m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(HISTORIA_6M_MAR==1, NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_6m", "nvisita_directa_6m", "nvisita_indirecta_6m", "nvisita_nocontacto_6m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(HISTORIA_6M_MAR==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_6m", "max_valpro_visita_6m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(HISTORIA_6M_MAR==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_6m", "min_llamada_6m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(HISTORIA_6M_MAR==1, NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_6m", "nllamada_martes_6m", "nllamada_miercoles_6m", 
                    "nllamada_jueves_6m", "nllamada_viernes_6m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(HISTORIA_6M_MAR==1, NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_6m", "nvisita_martes_6m", "nvisita_miercoles_6m", 
                    "nvisita_jueves_6m", "nvisita_viernes_6m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(HISTORIA_6M_MAR==1, NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_6m", "nllamada_1012_6m", "nllamada_1315_6m", "nllamada_1618_6m",
                    "nllamada_1921_6m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(HISTORIA_6M_MAR==1, NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_6m", "nvisita_1012_6m", "nvisita_1315_6m", "nvisita_1618_6m",
                    "nvisita_1921_6m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_6M_Mar17.csv", col_names = TRUE)
rm(list=c("cons9"))

# Pto_Obs: Marzo 2017 - Diciembre 2016 - Febrero 2017 - 3 Meses
reg_mar17_3m <- data %>% filter(ANIO=="2017", MES=="03", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(HISTORIA_3M_MAR==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_3m", "num_llamada_3m", "num_sms_3m", "num_emails_3m", "num_visita_3m")
cons1 <- left_join(reg_mar17_3m, res1, by = "cedula")
rm(list=c("reg_mar17_3m", "res1"))

res2 <- data %>% filter(HISTORIA_3M_MAR==1, NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_3m", "nllamada_directa_3m", "nllamada_indirecta_3m", "nllamada_nocontacto_3m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(HISTORIA_3M_MAR==1, NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_3m", "nvisita_directa_3m", "nvisita_indirecta_3m", "nvisita_nocontacto_3m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(HISTORIA_3M_MAR==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_3m", "max_valpro_visita_3m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(HISTORIA_3M_MAR==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_3m", "min_llamada_3m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(HISTORIA_3M_MAR==1, NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_3m", "nllamada_martes_3m", "nllamada_miercoles_3m", 
                    "nllamada_jueves_3m", "nllamada_viernes_3m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(HISTORIA_3M_MAR==1, NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_3m", "nvisita_martes_3m", "nvisita_miercoles_3m", 
                    "nvisita_jueves_3m", "nvisita_viernes_3m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(HISTORIA_3M_MAR==1, NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_3m", "nllamada_1012_3m", "nllamada_1315_3m", "nllamada_1618_3m",
                    "nllamada_1921_3m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(HISTORIA_3M_MAR==1, NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_3m", "nvisita_1012_3m", "nvisita_1315_3m", "nvisita_1618_3m",
                    "nvisita_1921_3m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_3M_Mar17.csv", col_names = TRUE)
rm(list=c("cons9"))

# Pto_Obs: Marzo 2017 - Febrero 2017 - 1 Mes
reg_mar17_1m <- data %>% filter(ANIO=="2017", MES=="03", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(ANIO=="2017", MES=="02") %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_1m", "num_llamada_1m", "num_sms_1m", "num_emails_1m", "num_visita_1m")
cons1 <- left_join(reg_mar17_1m, res1, by = "cedula")
rm(list=c("reg_mar17_1m", "res1"))

res2 <- data %>% filter(ANIO=="2017", MES=="02", NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_1m", "nllamada_directa_1m", "nllamada_indirecta_1m", "nllamada_nocontacto_1m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(ANIO=="2017", MES=="02", NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_1m", "nvisita_directa_1m", "nvisita_indirecta_1m", "nvisita_nocontacto_1m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(ANIO=="2017", MES=="02") %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_1m", "max_valpro_visita_1m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(ANIO=="2017", MES=="02", NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_1m", "min_llamada_1m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(ANIO=="2017", MES=="02", NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_1m", "nllamada_martes_1m", "nllamada_miercoles_1m", 
                    "nllamada_jueves_1m", "nllamada_viernes_1m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(ANIO=="2017", MES=="02", NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_1m", "nvisita_martes_1m", "nvisita_miercoles_1m", 
                    "nvisita_jueves_1m", "nvisita_viernes_1m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(ANIO=="2017", MES=="02", NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_1m", "nllamada_1012_1m", "nllamada_1315_1m", "nllamada_1618_1m",
                    "nllamada_1921_1m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(ANIO=="2017", MES=="02", NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_1m", "nvisita_1012_1m", "nvisita_1315_1m", "nvisita_1618_1m",
                    "nvisita_1921_1m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_1M_Mar17.csv", col_names = TRUE)
rm(list=c("cons9"))


# Pto_Obs: Junio 2017 - Diciembre 2016 - Mayo 2017 - 6 Meses
reg_jun17_6m <- data %>% filter(ANIO=="2017", MES=="06", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(HISTORIA_6M_JUN==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_6m", "num_llamada_6m", "num_sms_6m", "num_emails_6m", "num_visita_6m")
cons1 <- left_join(reg_jun17_6m, res1, by = "cedula")
rm(list=c("reg_jun17_6m", "res1"))

res2 <- data %>% filter(HISTORIA_6M_JUN==1, NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_6m", "nllamada_directa_6m", "nllamada_indirecta_6m", "nllamada_nocontacto_6m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(HISTORIA_6M_JUN==1, NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_6m", "nvisita_directa_6m", "nvisita_indirecta_6m", "nvisita_nocontacto_6m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(HISTORIA_6M_JUN==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_6m", "max_valpro_visita_6m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(HISTORIA_6M_JUN==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_6m", "min_llamada_6m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(HISTORIA_6M_JUN==1, NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_6m", "nllamada_martes_6m", "nllamada_miercoles_6m", 
                    "nllamada_jueves_6m", "nllamada_viernes_6m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(HISTORIA_6M_JUN==1, NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_6m", "nvisita_martes_6m", "nvisita_miercoles_6m", 
                    "nvisita_jueves_6m", "nvisita_viernes_6m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(HISTORIA_6M_JUN==1, NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_6m", "nllamada_1012_6m", "nllamada_1315_6m", "nllamada_1618_6m",
                    "nllamada_1921_6m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(HISTORIA_6M_JUN==1, NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_6m", "nvisita_1012_6m", "nvisita_1315_6m", "nvisita_1618_6m",
                    "nvisita_1921_6m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_6M_Jun17.csv", col_names = TRUE)
rm(list=c("cons9"))

# Pto_Obs: Junio 2017 - Marzo 2017 - Mayo 2017 - 3 Meses
reg_jun17_3m <- data %>% filter(ANIO=="2017", MES=="06", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(HISTORIA_3M_JUN==1) %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_3m", "num_llamada_3m", "num_sms_3m", "num_emails_3m", "num_visita_3m")
cons1 <- left_join(reg_jun17_3m, res1, by = "cedula")
rm(list=c("reg_jun17_3m", "res1"))

res2 <- data %>% filter(HISTORIA_3M_JUN==1, NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_3m", "nllamada_directa_3m", "nllamada_indirecta_3m", "nllamada_nocontacto_3m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(HISTORIA_3M_JUN==1, NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_3m", "nvisita_directa_3m", "nvisita_indirecta_3m", "nvisita_nocontacto_3m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(HISTORIA_3M_JUN==1) %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_3m", "max_valpro_visita_3m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(HISTORIA_3M_JUN==1, NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_3m", "min_llamada_3m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(HISTORIA_3M_JUN==1, NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_3m", "nllamada_martes_3m", "nllamada_miercoles_3m", 
                    "nllamada_jueves_3m", "nllamada_viernes_3m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(HISTORIA_3M_JUN==1, NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_3m", "nvisita_martes_3m", "nvisita_miercoles_3m", 
                    "nvisita_jueves_3m", "nvisita_viernes_3m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(HISTORIA_3M_JUN==1, NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_3m", "nllamada_1012_3m", "nllamada_1315_3m", "nllamada_1618_3m",
                    "nllamada_1921_3m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(HISTORIA_3M_JUN==1, NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_3m", "nvisita_1012_3m", "nvisita_1315_3m", "nvisita_1618_3m",
                    "nvisita_1921_3m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_3M_Jun17.csv", col_names = TRUE)
rm(list=c("cons9"))

# Pto_Obs: Junio 2017 - Mayo 2017 - 1 Mes
reg_jun17_1m <- data %>% filter(ANIO=="2017", MES=="06", REG_VALIDO==1, REG_IVR==1) %>% group_by(cedula) %>% 
      summarise(conteo=n()) %>% select(cedula)
res1 <- data %>% filter(ANIO=="2017", MES=="05") %>% group_by(cedula, NEW_ACCION) %>% summarise(conteo=n()) %>% 
      spread(key = NEW_ACCION, value = conteo)
colnames(res1) <- c("cedula", "num_noident_1m", "num_llamada_1m", "num_sms_1m", "num_emails_1m", "num_visita_1m")
cons1 <- left_join(reg_jun17_1m, res1, by = "cedula")
rm(list=c("reg_jun17_1m", "res1"))

res2 <- data %>% filter(ANIO=="2017", MES=="05", NEW_ACCION==1) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res2) <- c("cedula", "nllamada_noident_1m", "nllamada_directa_1m", "nllamada_indirecta_1m", "nllamada_nocontacto_1m")
cons2 <- left_join(cons1, res2, by = "cedula")
rm(list=c("cons1", "res2"))

res3 <- data %>% filter(ANIO=="2017", MES=="05", NEW_ACCION==4) %>% group_by(cedula, VAR_DEP) %>% summarise(conteo=n()) %>% 
      spread(key = VAR_DEP, value = conteo)
colnames(res3) <- c("cedula", "nvisita_noident_1m", "nvisita_directa_1m", "nvisita_indirecta_1m", "nvisita_nocontacto_1m")
cons3 <- left_join(cons2, res3, by = "cedula")
rm(list=c("cons2", "res3"))

res4 <- data %>% filter(ANIO=="2017", MES=="05") %>% mutate(valor_pro=parse_double(valor_promesa)) %>% 
      group_by(cedula, NEW_ACCION) %>% summarise(max_val_pro=max(valor_pro)) %>% 
      spread(key = NEW_ACCION, value = max_val_pro) %>% select(1,3,6)
colnames(res4) <- c("cedula", "max_valpro_llamada_1m", "max_valpro_visita_1m")
cons4 <- left_join(cons3, res4, by = "cedula")
rm(list=c("cons3", "res4"))

res5 <- data %>% filter(ANIO=="2017", MES=="05", NEW_ACCION==1) %>% group_by(cedula) %>% 
      summarise(max_llamada=max(duracion), min_llamada=min(duracion))
colnames(res5) <- c("cedula", "max_llamada_1m", "min_llamada_1m")
cons5 <- left_join(cons4, res5, by = "cedula")
rm(list=c("cons4", "res5"))

res6 <- data %>% filter(ANIO=="2017", MES=="05", NEW_ACCION==1) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res6) <- c("cedula", "nllamada_lunes_1m", "nllamada_martes_1m", "nllamada_miercoles_1m", 
                    "nllamada_jueves_1m", "nllamada_viernes_1m")
cons6 <- left_join(cons5, res6, by = "cedula")
rm(list=c("cons5", "res6"))

res7 <- data %>% filter(ANIO=="2017", MES=="05", NEW_ACCION==4) %>% group_by(cedula, DIA) %>% 
      summarise(conteo=n()) %>% spread(key = DIA, value = conteo) %>% select(1,3,7,8,6,2)
colnames(res7) <- c("cedula", "nvisita_lunes_1m", "nvisita_martes_1m", "nvisita_miercoles_1m", 
                    "nvisita_jueves_1m", "nvisita_viernes_1m")
cons7 <- left_join(cons6, res7, by = "cedula")
rm(list=c("cons6", "res7"))

res8 <- data %>% filter(ANIO=="2017", MES=="05", NEW_ACCION==1) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res8) <- c("cedula", "nllamada_79_1m", "nllamada_1012_1m", "nllamada_1315_1m", "nllamada_1618_1m",
                    "nllamada_1921_1m")
cons8 <- left_join(cons7, res8, by = "cedula")
rm(list=c("cons7", "res8"))

res9 <- data %>% filter(ANIO=="2017", MES=="05", NEW_ACCION==4) %>% mutate(rango_hora=cut(parse_double(HORAS), 
      breaks=c(-1,6,9,12,15,18,21,24))) %>% group_by(cedula, rango_hora) %>% summarise(conteo=n()) %>% 
      spread(key = rango_hora, value = conteo) %>% select(1,3:7)
colnames(res9) <- c("cedula", "nvisita_79_1m", "nvisita_1012_1m", "nvisita_1315_1m", "nvisita_1618_1m",
                    "nvisita_1921_1m")
cons9 <- left_join(cons8, res9, by = "cedula")
rm(list=c("cons8", "res9"))

write_excel_csv(cons9, path = "Var_Hist_1M_Jun17.csv", col_names = TRUE)
rm(list=c("cons9"))

## Consolidación archivos

# Septiembre 2016
base6m <- read_csv("Var_Hist_6M_Sep16.csv")
base3m <- read_csv("Var_Hist_3M_Sep16.csv")
base1m <- read_csv("Var_Hist_1M_Sep16.csv")
cons <- left_join(base6m, base3m, by = "cedula")
Hist_Sep16 <- left_join(cons, base1m, by = "cedula")
rm(list=c("base6m", "base3m", "base1m", "cons"))
write_excel_csv(Hist_Sep16, path = "Var_Hist_Sep16.csv", col_names = TRUE)

# Diciembre 2016
base6m <- read_csv("Var_Hist_6M_Dic16.csv")
base3m <- read_csv("Var_Hist_3M_Dic16.csv")
base1m <- read_csv("Var_Hist_1M_Dic16.csv")
cons <- left_join(base6m, base3m, by = "cedula")
Hist_Dic16 <- left_join(cons, base1m, by = "cedula")
rm(list=c("base6m", "base3m", "base1m", "cons"))
write_excel_csv(Hist_Dic16, path = "Var_Hist_Dic16.csv", col_names = TRUE)

# Marzo 2017
base6m <- read_csv("Var_Hist_6M_Mar17.csv")
base3m <- read_csv("Var_Hist_3M_Mar17.csv")
base1m <- read_csv("Var_Hist_1M_Mar17.csv")
cons <- left_join(base6m, base3m, by = "cedula")
Hist_Mar17 <- left_join(cons, base1m, by = "cedula")
rm(list=c("base6m", "base3m", "base1m", "cons"))
write_excel_csv(Hist_Mar17, path = "Var_Hist_Mar17.csv", col_names = TRUE)

# Junio 2017
base6m <- read_csv("Var_Hist_6M_Jun17.csv")
base3m <- read_csv("Var_Hist_3M_Jun17.csv")
base1m <- read_csv("Var_Hist_1M_Jun17.csv")
cons <- left_join(base6m, base3m, by = "cedula")
Hist_Jun17 <- left_join(cons, base1m, by = "cedula")
rm(list=c("base6m", "base3m", "base1m", "cons"))
write_excel_csv(Hist_Jun17, path = "Var_Hist_Jun17.csv", col_names = TRUE)



## Cruce días vencidos
dven <- read_tsv("Dven2017.txt")
dven_mar <- dven %>% filter(FECHA_CORTE=='201703') %>% select(4:6)
colnames(dven_mar) <- c("c_id_deudor", "dias_mora", "fecha_corte")
muestra_mar <- data %>% filter(MES=='03', ANIO=='2017') %>% select(5,2)
muestra_mar$c_id_deudor <- parse_integer(muestra_mar$c_id_deudor)
cruce_mar <- inner_join(muestra_mar, dven_mar, by='c_id_deudor') %>% group_by(cedula) %>% 
      summarise(dias_vencido=max(dias_mora))
write_excel_csv(cruce_mar, path="dias_ven_mar17.csv", col_names = TRUE)


dven_jun <- dven %>% filter(FECHA_CORTE=='201706') %>% select(4:6)
colnames(dven_jun) <- c("c_id_deudor", "dias_mora", "fecha_corte")
muestra_jun <- data %>% filter(MES=='06', ANIO=='2017') %>% select(5,2)
muestra_jun$c_id_deudor <- parse_integer(muestra_jun$c_id_deudor)
cruce_jun <- inner_join(muestra_jun, dven_jun, by='c_id_deudor') %>% group_by(cedula) %>% 
      summarise(dias_vencido=max(dias_mora))
write_excel_csv(cruce_jun, path="dias_ven_jun17.csv", col_names = TRUE)
