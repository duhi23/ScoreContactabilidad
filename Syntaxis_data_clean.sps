﻿* Encoding: UTF-8.
* SYNTAXIS - MODELO DE CONTACTABILIDAD

* EXTRACCION DE HORA.
STRING  S_HORA (A8).
COMPUTE S_HORA=CHAR.SUBSTR(hora,1,2).
EXECUTE.

* EXTRACCIÖN DEL MES.
STRING  MES (A8).
COMPUTE MES=CHAR.SUBSTR(fecha,6,2).
EXECUTE.

* EXTRACCION DEL AÑO.
STRING  ANIO (A8).
COMPUTE ANIO=CHAR.SUBSTR(fecha,1,4).
EXECUTE.

COMPUTE PTO_OBS=0.
IF ((ANIO='2016' AND (MES='12' OR MES='09')) OR (ANIO='2017' AND (MES='03' OR MES='06'))) PTO_OBS=1.
EXECUTE.
VARIABLE LABELS  PTO_OBS 'Registros al punto de observación'.
VALUE LABELS PTO_OBS
  0 'NO'
  1 'SI'.
EXECUTE.

*RECODIFICACION - ACCION.
COMPUTE NEW_ACCION=0.
IF(accion='CONTACTO INDIRECTO' AND (respuesta='BUZON CELULAR' OR respuesta='FAMILIAR' OR respuesta='BUZON DE VOZ' OR 
respuesta='DEJA MENSAJE' OR respuesta='TRABAJO')) NEW_ACCION=1.
IF(accion='CONTACTO INDIRECTO' AND respuesta='SMS') NEW_ACCION=2.
IF(accion='CONTACTO INDIRECTO' AND respuesta='ENVIADO') NEW_ACCION=3.
IF(accion='CONTACTO INDIRECTO' AND (respuesta='AMIGO' OR respuesta='VECINO')) NEW_ACCION=4.
IF(accion='NC-TELEFONO CEULAR' AND (respuesta='EQUIVOCADO' OR respuesta='IVR' OR respuesta='NO CONTESTA' OR 
respuesta='NUMERO AVERIADO' OR respuesta='OCUPADO' OR respuesta='NO ASIGNADO')) NEW_ACCION=1.
IF(accion='NC-TELEFONO CEULAR' AND respuesta='SMS') NEW_ACCION=2.
IF(accion='NO CONTACTADO' OR accion='CONTACTO DIRECTO' OR accion='NO CONTESTA' OR accion='BUZON CELULAR' OR accion='NC-TELEFONO FIJO' OR 
accion='MENSAJE A TERCEROS' OR accion='NUMERO NO CORRESPOND' OR accion='CA-TELEFONO CELULAR') NEW_ACCION=1.
IF(accion='VISITA DOMICILIARIA') NEW_ACCION=4.
IF(accion='NO EXITOSO' AND (respuesta='INCONFORME CON DEUDA' OR respuesta='NO VA A PAGAR' OR respuesta='NUNCA TUVO PRODUCTO')) NEW_ACCION=1.
IF(accion='NO EXITOSO' AND respuesta='SE HACE NEGAR') NEW_ACCION=4.
IF(accion='MAIL') NEW_ACCION=3.
IF(accion='OFRECIMIENTO AL CORT' OR accion='CD-COMPROMISO DE PAG' OR accion='PC-VOLVER A LLAMAR' OR accion='MENSAJE A TERCERO' OR accion='ACUERDO DE PAGO' OR
accion='COMPROMISO DE PAGO' OR accion='CD-NEGOCIACION' OR accion='CONTAC SIN ARREGLO M' OR accion='NO CONTACTO' OR accion='PC-NO DA INFORMACION' OR
accion='PNC-VOLVER A LLAMAR' OR accion='INFORMA YA PAGO' OR accion='AUN NO CONTACTADO') NEW_ACCION=1.
IF(accion='SMS' OR accion='IVR') NEW_ACCION=2.
IF(accion='REFINANCIA' OR accion='CD-YA REALIZO PAGO' OR accion='CA-TELEFONO FIJO' OR accion='PNC-NO AYUDA CON INF' OR accion='PC-COMPROMISO DE PAG' OR
accion='CD-RENUENTE AL PAGO' OR accion='CT-VOLVER A LLAMAR' OR accion='CT-YA NO LABORA AHI' OR accion='CT-NO PUEDE ACERCARS' OR 
accion='PC-FUERA DEL PAIS' OR accion='PC-CLIENTE FALLECIDO' OR accion='PNC-SE ENCUENTRA FUE' OR accion='CONTACTADO') NEW_ACCION=1.
IF(accion='BUZON DOMICILIO' OR accion='NOTIFICADO') NEW_ACCION=4.
EXECUTE.
VARIABLE LABELS  NEW_ACCION 'Recodificación Variable Accion'.
VALUE LABELS NEW_ACCION
  0 'No Identificado'
  1 'Llamada'
  2 'SMS'
  3 'Mail'
  4 'Visita Domiciliaria'.
EXECUTE.

COMPUTE REG_IVR=1.
IF(NEW_ACCION=1 AND respuesta='IVR') REG_IVR=0.
EXECUTE.

TEMPORARY.
SELECT IF(REG_IVR=1).
FREQUENCIES VARIABLES=NEW_ACCION
  /ORDER=ANALYSIS.

TEMPORARY.
SELECT IF(REG_IVR=1 AND PTO_OBS=1).
FREQUENCIES VARIABLES=MES
  /ORDER=ANALYSIS.


COMPUTE VAR_DEP=0.
IF(NEW_ACCION=1 AND (respuesta='BUZON CELULAR' OR respuesta='NO CONTACTADO' OR respuesta='NO CONTESTA' OR 
respuesta='NUMERO EQUIVOCADO' OR respuesta='NéMERO NO PERTENECE' OR respuesta='EQUIVOCADO' OR respuesta='NéMERO ERRADO' OR
respuesta='NO CONTACTO' OR respuesta='NO DEJA MENSAJE' OR respuesta='NUMERO AVERIADO' OR respuesta='NUMERO NO CORRESPOND' OR
respuesta='OCUPADO' OR respuesta='EQUIVOC/DESC NUM' OR respuesta='DIRECCION NO CORRESPON' OR
respuesta='LINEA AVERIADA')) VAR_DEP=3.
IF(NEW_ACCION=1 AND (respuesta='DEJA MENSAJE' OR respuesta='FAMILIAR' OR respuesta='RECEPTA EL MENSAJE' OR respuesta='DARA MENSAJE' OR
respuesta='NO RECEPTA MENSAJE' OR respuesta='VOLVER A LLAMAR' OR respuesta='TRABAJO' OR respuesta='NO CONCRETA' OR respuesta='FAMILIARES' OR
respuesta='HIJOS / PADRES' OR respuesta='NO DESEA ENTREGAR INFO' OR respuesta='ESPOSA (O)' OR respuesta='NO PUEDE ENTREGAR INF' OR
respuesta='COMPROM PAGO TERCERO' OR respuesta='ACUERDO PAGO TERCERO' OR respuesta='LLAMAR EN LA TARDE' OR 
respuesta='FALLECIDO' OR respuesta='LLAMAR EN LA MANANA' OR respuesta='LLAMAR EN LA NOCHE' OR respuesta='VIAJE')) VAR_DEP=2.
IF(NEW_ACCION=1 AND (respuesta='Compromiso de Pago' OR respuesta='SIN COMPROMISO D PAG' OR respuesta='TELEFONO CELULAR' OR 
respuesta='OFRECIMIENTO DE PAGO T' OR respuesta='INFORMA YA PAGO'  OR respuesta='PAGO TOTAL' OR 
respuesta='CONTEST. CEL/ CONVEN' OR respuesta='NO VA A PAGAR' OR respuesta='ACUERDO DE PAGO TT' OR
respuesta='COMPROMISO PAGO TT' OR respuesta='INCONFORME CON DEUDA' OR respuesta='OFRCIMIENTO DE ARREGLO' OR respuesta='ABONO' OR
respuesta='REFINANCIA' OR respuesta='BANCO' OR respuesta='NO REALIZO PEDIDO' OR respuesta='OFRE PAG DESP CORTE' OR
respuesta='COMPROMISO DE PAGO' OR respuesta='OFREC PAG PAR DENTRO C' OR respuesta='DESEA ACUERDO' OR
respuesta='ACUERDO DE PAGO' OR respuesta='SIN COMPROMIS DE PAG')) VAR_DEP=1.
IF(NEW_ACCION=2 AND (respuesta='SMS' OR respuesta='ENVIADO')) VAR_DEP=1.
IF(NEW_ACCION=2 AND (respuesta='SMSNO ENVIADO' OR respuesta='NO ENVIADO')) VAR_DEP=3.
IF(NEW_ACCION=3 AND (respuesta='ENVIADO' OR respuesta='MAILENVIADO')) VAR_DEP=1.
IF(NEW_ACCION=3 AND (respuesta='MAILNO ENVIADO' OR respuesta='NO ENVIADO')) VAR_DEP=3.
IF(NEW_ACCION=4 AND (respuesta='MSJ FAM - TER' OR respuesta='CONIND MSJE FAM-TER' OR respuesta='SE HACE NEGAR' OR respuesta='AMIGO' OR
respuesta='VECINO' OR respuesta='DEJA MENSAJE')) VAR_DEP=2.
IF(NEW_ACCION=4 AND (respuesta='DIRECCION INCORRECTA' OR respuesta='DIRECCCION INCORRECTA')) VAR_DEP=3.
IF(NEW_ACCION=4 AND (respuesta='CONDIR COMP DE PAGO' OR respuesta='CONDIR SIN COMP PAG' OR respuesta='CONDIR REALIZA PAGO' OR respuesta='NOTIFICADO' OR
respuesta='CONDIR INF YA PAGO')) VAR_DEP=1. 
EXECUTE.
VARIABLE LABELS  VAR_DEP 'Variable Dependiente'.
VALUE LABELS VAR_DEP
  0 'No Identificado'
  1 'Contacto Directo'
  2 'Contacto Indirecto'
  3 'No Contacto'.
EXECUTE.

FREQUENCIES VARIABLES=VAR_DEP
  /ORDER=ANALYSIS.

TEMPORARY.
SELECT IF(PTO_OBS=1 AND REG_IVR=1).
CROSSTABS 
  /TABLES=NEW_ACCION BY MES
  /FORMAT=AVALUE TABLES 
  /CELLS=COUNT 
  /COUNT ROUND CELL.

TEMPORARY.
SELECT IF(PTO_OBS=1 AND REG_IVR=1).
CROSSTABS 
  /TABLES=NEW_ACCION BY VAR_DEP
  /FORMAT=AVALUE TABLES 
  /CELLS=COUNT 
  /COUNT ROUND CELL.

COMPUTE REG_VALIDO=0.
IF (VAR_DEP>0 AND (NEW_ACCION=1 OR NEW_ACCION=4)) REG_VALIDO=1.
EXECUTE.
VARIABLE LABELS  REG_VALIDO 'Registros para el modelamiento'.
VALUE LABELS REG_VALIDO
  0 'NO'
  1 'SI'.
EXECUTE.

TEMPORARY.
SELECT IF(PTO_OBS=1 AND REG_IVR=1 AND REG_VALIDO=1).
CROSSTABS 
  /TABLES=VAR_DEP BY MES
  /FORMAT=AVALUE TABLES 
  /CELLS=COUNT 
  /COUNT ROUND CELL.


* GENERACION DE VARIABLES HISTORICAS - 6 MESES.

COMPUTE HISTORIA_6M_SEP=0.
IF(ANIO='2016' AND (MES='03' OR MES='04' OR MES='05' OR MES='06' OR MES='07' OR MES='08')) HISTORIA_6M_SEP=1.
EXECUTE.

COMPUTE HISTORIA_6M_DIC=0.
IF(ANIO='2016' AND (MES='06' OR MES='07' OR MES='08' OR MES='09' OR MES='10' OR MES='11')) HISTORIA_6M_DIC=1.
EXECUTE.

COMPUTE HISTORIA_6M_MAR=0.
IF(ANIO='2016' AND (MES='09' OR MES='10' OR MES='11' OR MES='12')) HISTORIA_6M_MAR=1.
IF(ANIO='2017' AND (MES='01' OR MES='02')) HISTORIA_6M_MAR=1.
EXECUTE.

COMPUTE HISTORIA_6M_JUN=0.
IF(ANIO='2016' AND MES='12') HISTORIA_6M_JUN=1.
IF(ANIO='2017' AND (MES='01' OR MES='02' OR MES='03' OR MES='04' OR MES='05')) HISTORIA_6M_JUN=1.
EXECUTE.

* GENERACION DE VARIABLES HISTORICAS - 3 MESES.

COMPUTE HISTORIA_3M_SEP=0.
IF(ANIO='2016' AND (MES='06' OR MES='07' OR MES='08')) HISTORIA_3M_SEP=1.
EXECUTE.

COMPUTE HISTORIA_3M_DIC=0.
IF(ANIO='2016' AND (MES='09' OR MES='10' OR MES='11')) HISTORIA_3M_DIC=1.
EXECUTE.

COMPUTE HISTORIA_3M_MAR=0.
IF(ANIO='2016' AND MES='12') HISTORIA_3M_MAR=1.
IF(ANIO='2017' AND (MES='01' OR MES='02')) HISTORIA_3M_MAR=1.
EXECUTE.

COMPUTE HISTORIA_3M_JUN=0.
IF(ANIO='2017' AND (MES='03' OR MES='04' OR MES='05')) HISTORIA_3M_JUN=1.
EXECUTE.



CROSSTABS 
  /TABLES= HISTORIA_6M_SEP HISTORIA_6M_DIC HISTORIA_6M_MAR HISTORIA_6M_JUN HISTORIA_3M_SEP HISTORIA_3M_DIC HISTORIA_3M_MAR HISTORIA_3M_JUN BY MES
  /FORMAT=AVALUE TABLES 
  /CELLS=COUNT 
  /COUNT ROUND CELL.

