# -----------------------------------------------------------------------------
#                              Conciliation project
# 
# Code author: Antonio Carbonell
# Date: August 28, 2024
#
# Code modifications: 
# Date of modifications:
#
# Objective: Identify false drops 
#
# Data inputs: worker_first, worker_hearing_request_characteristics
# Outputs: false_drop_base
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#                           Libraries and settings
# -----------------------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(here, tidyverse, fixest, knitr, kableExtra, vtable, haven, RCT)

# -----------------------------------------------------------------------------
#                               Read in datasets
# -----------------------------------------------------------------------------


hearings_features <- read.csv(here("01_Data",
                                   "02_Created",
                                   "worker_first.csv"))


worker_hearing_request_characteristics <-  read_csv(here("01_Data",
                                                          "02_Created",
                                                          "worker_hearing_request_characteristics.csv"))

# -----------------------------------------------------------------------------
#                               Merge data sets
# -----------------------------------------------------------------------------

# Observations before merge 
# hearing features: 31822
# worker_hearing_request_characteristics: 103394

# level of the data
# hearing features: first hearing-one worker-request
# worker_hearing_request_characteristics: worker-hearing-request

final_base <- left_join(hearings_features,worker_hearing_request_characteristics,  by=c("solicitud_id","audiencia_id","parte_id"))

# final base merge:
# Observations matched: 29848
# Observations not matched 1974

# The possibility of a mistmath is confusing, as there should be data for every worker. 
# But remember on 01_clean_conciliation we winsorized workers data.

# -----------------------------------------------------------------------------
#                               Identification
# -----------------------------------------------------------------------------

# To identify a false drop, it is necessary to have the curp of the worker, as it is the only intifier that lets us know if 
# a worker went twice to the conciliaiton center 

# filtering for curp 
final_base <- final_base %>%
  filter(!is.na(curp_trabajador))

# prelimnar count of worker repetition on the data set 
final_base <- final_base %>%
  group_by(curp_trabajador) %>%
  mutate(cuanto_repiten_curp_trabajador=n()) %>%
  ungroup() %>%
  group_by(rfc_trabajador) %>%
  mutate(cuanto_repiten_rfc_trabajador=n()) %>%
  ungroup() 

# is this curp repeated somewhere in the data set dummy
final_base <- final_base %>%
  mutate(curp_repetido=if_else(cuanto_repiten_curp_trabajador>1,1,0))

#  number of cases where we have a worker that came more than once (take into account that they are counting double or tripple
# , the data is at the level hearing so every hearing where I "Antonio" came to the heraing counts as once observation)

 table(final_base$curp_repetido)
 
# Create a variable that might be interesting (Number of summoned vs. number of attendees)
 final_base <- final_base %>%
   mutate(diferencia_citados_comaprecientes=numero_citados-citados_comparecen_audiencia)

# The idea to identify a false drop is that immediately after a case has been archived, 
# the same worker has made another request. However, 
# to identify this phenomenon and others that involve variables from one request to another, 
# it is necessary to shift the information from the following request to the previous one,
# so that all the information is in the same row.
 
# For example, if I want to check the date of the first request and the last request,
# it is not possible in the current format of the database,
# since the date of the last request belongs to another row.
# To solve this, the information from subsequent hearings is shifted by grouping by worker,
# sorting according to the date, and then leading the data.
 
 final_base <- final_base %>%
   # grouping by worker 
   group_by(curp_trabajador) %>%
   # sorting according to the date
   arrange(ymd(fecha_audiencia)) %>%
   # leading information about the date of the request
   mutate(fecha_siguiente_solicitud=lead(fecha_solicitud))%>%
   # leading information about the date of the gender (I did it like a sanity check )
   mutate(genero_next_claim=lead(genero))%>%
   # leading information about the date of the wage claimed
   mutate(sal_diario_next_claim=lead(sal_diario))%>%
   # leading information about the date of the tenure
   mutate(antig_next_claim=lead(antig))%>%
   # leading information about the workers type of shift
   mutate(diurna_next_claim=lead(diurna))%>%
   # leading information about the workers payment time frame
   mutate(quincenal_next_claim=lead(quincenal))%>%
   # leading information about the workers shift
   mutate(horas_semanales_next_claim=lead(horas_semanales))%>%
   # leading information about the Number of summoned
   mutate(numero_citados_next_claim=lead(numero_citados))%>%
   # leading information about the concilaitor on the next request
   mutate(conciliador_next_claim=lead(conciliador))%>%
   # leading information about the the entrance date
   mutate(fecha_ingreso_next_claim=lead(fecha_ingreso))%>%
   # leading information about the exit date
   mutate(fecha_salida_next_claim=lead(fecha_salida))%>%
   # leading information about next claim outcomes
   mutate(max_hubo_convenio_next_claim=lead(max_hubo_convenio))%>%
   mutate(max_archivado_next_claim=lead(max_archivado))%>%
   mutate(max_no_hubo_convenio_next_claim=lead(max_no_hubo_convenio))%>%
   mutate(max_numero_audiencia_next_claim=lead(max_numero_audiencia))%>%
   # leading information about summoned and atendance to the hearings
   mutate(presentado_next_claim=lead(presentado))%>%
   mutate(citados_comparecen_audiencia_next_claim=lead(citados_comparecen_audiencia))%>%
   mutate(diferencia_citados_comaprecientes_next_claim=lead(diferencia_citados_comaprecientes))%>%
   ungroup()
 
# creating a variable that denotes if the conciliator changed 
 final_base <-  final_base %>%
   mutate(different_conciliator=if_else(conciliador_next_claim!=conciliador,1,0))
 
 
 # -----------------------------------------------------------------------------
 #                              Using lead infromation to compute false drops
 # -----------------------------------------------------------------------------
 
 # calculating the difference in days between tha repetead cases
 
 final_base <- final_base %>%
   # time between hearing and next request
   mutate(tiempo_entre_audiencia_y_siguiente_solicitud=difftime(fecha_siguiente_solicitud, fecha_audiencia, units = "days") ) %>%
   # time between request and request
   mutate(tiempo_entre_solicitud_y_siguiente_solicitud=difftime(fecha_siguiente_solicitud, fecha_solicitud, units = "days") ) %>%
   # time between request and hearing
   mutate(tiempo_entre_solicitud_y_audiencia_solicitada=difftime(fecha_audiencia, fecha_solicitud, units = "days") ) 
 
 # Those cases where we have a drop and a new request presented in a time lapse of 25 days WILL BE CONSIDERED FALSE DROPS
 
 final_base <- final_base %>%
   # if time between hearing and next request is less than 25 days and the file got dropped and the time between the hearing and 
   # the next request is positive ---> then we have a false drop 
   mutate(false_drop_time_25_dropped=if_else(tiempo_entre_audiencia_y_siguiente_solicitud<=25 &
                                               max_archivado==1 &
                                               tiempo_entre_audiencia_y_siguiente_solicitud>=0,1,0)) 
 
# It is not enough to identify false drops; to conduct any worthwhile analysis, we need other variables—controls or outcomes.
# We ensured this by previously leading with other variables; however, for all observations that are not
# false drops we need to keep the old value 
 
 final_base <-  final_base %>%
   # If the observation is a false drop, put the next claim, if not put the same claim
   mutate(numero_citados_asump_time_25=if_else(false_drop_time_25_dropped==0,numero_citados,numero_citados_next_claim))%>%
   mutate(max_hubo_convenio_asump_time_25=if_else(false_drop_time_25_dropped==0,max_hubo_convenio,max_hubo_convenio_next_claim))%>%
   mutate(max_no_hubo_convenio_asump_time_25=if_else(false_drop_time_25_dropped==0,max_no_hubo_convenio,max_no_hubo_convenio_next_claim))%>%
   mutate(max_numero_audiencia_asump_time_25=if_else(false_drop_time_25_dropped==0,max_numero_audiencia,max_numero_audiencia_next_claim))%>%
   mutate(max_archivado_asump_time_25=if_else(false_drop_time_25_dropped==0,max_archivado,max_archivado_next_claim))%>%
   mutate(citados_comparecen_audiencia_asump_time_25=if_else(false_drop_time_25_dropped==0,citados_comparecen_audiencia,citados_comparecen_audiencia_next_claim))%>%
   mutate(diferencia_citados_comaprecientes_asump_time_25=if_else(false_drop_time_25_dropped==0,diferencia_citados_comaprecientes,diferencia_citados_comaprecientes_next_claim))%>%
   mutate(diff_con_asump_time_25_dropped=if_else(false_drop_time_25_dropped==0,0,different_conciliator)) %>% 
   mutate(presentado_25_dropped=if_else(false_drop_time_25_dropped==0,presentado,presentado_next_claim))
 
# write 
 write.csv( final_base,here("01_Data",
                              "02_Created",
                              "base_with_fake_drops.csv"))
 
