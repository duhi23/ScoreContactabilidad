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
IF ((MES ='02' OR MES ='04' OR MES ='06') AND ANIO='2017') PTO_OBS=1.
EXECUTE.
VARIABLE LABELS  PTO_OBS 'Registros al punto de observación'.
VALUE LABELS PTO_OBS
  0 'NO'
  1 'SI'.
EXECUTE.

*RECODIFICACION - ACCION.
COMPUTE NEW_ACCION=0.
IF(accion='CONTACTO INDIRECTO' AND (respuesta='BUZON CELULAR' OR respuesta='FAMILIAR' OR respuesta='BUZON DE VOZ' OR respuesta='DEJA MENSAJE' OR respuesta='TRABAJO')) NEW_ACCION=1.
IF(accion='CONTACTO INDIRECTO' AND respuesta='SMS') NEW_ACCION=2.
IF(accion='CONTACTO INDIRECTO' AND respuesta='ENVIADO') NEW_ACCION=3.
IF(accion='CONTACTO INDIRECTO' AND (respuesta='AMIGO' OR respuesta='VECINO')) NEW_ACCION=4.
IF(accion='NC-TELEFONO CEULAR' AND (respuesta='EQUIVOCADO' OR respuesta='IVR' OR respuesta='NO CONTESTA' OR respuesta='NUMERO AVERIADO' OR respuesta='OCUPADO' OR 
respuesta='NO ASIGNADO')) NEW_ACCION=1.
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
accion='CD-RENUENTE AL PAGO') NEW_ACCION=1.
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


FREQUENCIES VARIABLES=NEW_ACCION
  /ORDER=ANALYSIS.

TEMPORARY.
SELECT IF(PTO_OBS=1).
CROSSTABS 
  /TABLES=NEW_ACCION BY MES
  /FORMAT=AVALUE TABLES 
  /CELLS=COUNT 
  /COUNT ROUND CELL.


TEMPORARY.
SELECT IF(NEW_ACCION=4).
CROSSTABS 
  /TABLES=respuesta BY contacto
  /FORMAT=AVALUE TABLES 
  /CELLS=COUNT 
  /COUNT ROUND CELL.


COMPUTE VAR_DEP=0.
IF(NEW_ACCION=1 AND (respuesta='IVR' OR respuesta='BUZON CELULAR' OR respuesta='NO CONTACTADO' OR respuesta='NO CONTESTA' OR 
respuesta='NUMERO EQUIVOCADO' OR respuesta='NéMERO NO PERTENECE' OR respuesta='EQUIVOCADO' OR respuesta='NéMERO ERRADO' OR
respuesta='NO CONTACTO' OR respuesta='NO DEJA MENSAJE' OR respuesta='NUMERO AVERIADO' OR respuesta='NUMERO NO CORRESPOND')) VAR_DEP=3.
IF(NEW_ACCION=1 AND (respuesta='DEJA MENSAJE' OR respuesta='FAMILIAR' OR respuesta='RECEPTA EL MENSAJE' OR respuesta='DARA MENSAJE' OR
respuesta='NO RECEPTA MENSAJE' OR respuesta='VOLVER A LLAMAR' OR respuesta='TRABAJO' OR respuesta='NO CONCRETA' OR respuesta='FAMILIARES' OR
respuesta='HIJOS / PADRES' OR respuesta='NO DESEA ENTREGAR INFO' OR respuesta='ESPOSA (O)' OR respuesta='NO PUEDE ENTREGAR INF')) VAR_DEP=2.
IF(NEW_ACCION=1 AND (respuesta='Compromiso de Pago' OR respuesta='SIN COMPROMISO D PAG' OR respuesta='TELEFONO CELULAR' OR 
respuesta='OFRECIMIENTO DE PAGO T' OR respuesta='CONTEST. CEL/ CONVEN' OR respuesta='NO VA A PAGAR' OR respuesta='ACUERDO DE PAGO TT' OR
respuesta='COMPROMISO PAGO TT' OR respuesta='INCONFORME CON DEUDA' OR respuesta='OFRCIMIENTO DE ARREGLO' OR respuesta='ABONO' OR
respuesta='REFINANCIA')) VAR_DEP=1.
IF(NEW_ACCION=2 AND (respuesta='SMS' OR respuesta='ENVIADO')) VAR_DEP=1.
IF(NEW_ACCION=2 AND (respuesta='SMSNO ENVIADO' OR respuesta='NO ENVIADO')) VAR_DEP=3.
IF(NEW_ACCION=3 AND (respuesta='ENVIADO' OR respuesta='MAILENVIADO')) VAR_DEP=1.
IF(NEW_ACCION=3 AND (respuesta='MAILNO ENVIADO' OR respuesta='NO ENVIADO')) VAR_DEP=3.
IF(NEW_ACCION=4 AND (respuesta='MSJ FAM - TER' OR respuesta='CONIND MSJE FAM-TER' OR respuesta='SE HACE NEGAR' OR respuesta='AMIGO' OR
respuesta='VECINO' OR respuesta='DEJA MENSAJE')) VAR_DEP=2.
IF(NEW_ACCION=4 AND respuesta='DIRECCION INCORRECTA') VAR_DEP=3.
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

TEMPORARY.
SELECT IF(PTO_OBS=1).
CROSSTABS 
  /TABLES=VAR_DEP BY MES
  /FORMAT=AVALUE TABLES 
  /CELLS=COUNT 
  /COUNT ROUND CELL.