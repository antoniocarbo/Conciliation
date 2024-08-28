#' @name: clean_conciliation.R
#' 
#' @author: Marco Medina and Antonio Carbonell
#' 
#' @description: clean conciliation hearings data.
#' 
#' @In: completo-datos-fuente_2023-07-25.csv
#'      completo-datos-pagos-fuente_2023-07-25.csv
#' 
#' @Out: 
#' conciliator.csv
#' worker_first.csv
#' worker_hearing_request_characteristics.csv
#' worker_hearing.csv
#' worker_payment_order.csv
# Load libraries
pacman::p_load(here, readr, lubridate, dplyr, tidyr, stringr, readxl)


# CLEAN HEARINGS DATA ----------------------------------------------------------
# This data is at worker-hearing level. A conciliation request may involve several
# workers, and several hearings. Some variables are at request level, some at worker
# and some at hearing level.

# Input the last update date
#update_date <- "2023-07-25"
update_date <- "2023-10-17"

# Load conciliation hearings data
hearings_clean <- read_csv(str_c("01_Data/01_Raw/completo-datos-fuente_",
                                 update_date,
                                 ".csv")) %>%
  
  # Drop hearings from conciliation requests that were never confirmed.
  filter(estatus_solicitud != "SIN RATIFICAR", confirmada == TRUE) %>%
  
  # Drop observation before the conciliation center opened and functioned normally  # primero de enero del 2023 
  # (I corrected this but I made a mistake and put 2022 now it is okay)
  filter(fecha_solicitud >= date("2023-01-01")) %>%
  
  # Drop collective or labor union requests, and employers request
  filter(tipo_solicitud == "Trabajador") %>%
  
  # Drop requests that were confirmed in September and October 2023. Since our sample was given
  # in late October, we want to keep requests that have had at least a month or two to 
  # go through the process
  filter(fecha_confirmacion < date("2023-09-01")) %>%
  
  # Drop inmeadite conciliation hearings (ratificaciones con convenio)
  filter(inmediata == FALSE) %>%
  
  # Clean conciliator names.Their names are repeated by the system.
  # e.g. GABRIELA MORENO MARTÍNEZ SUBDIR. B|GABRIELA MORENO MARTÍNEZ SUBDIR. B
  mutate(conciliador = ifelse(str_detect(conciliador, pattern = "\\|"),
                              str_extract(conciliador, pattern = ".*(?=\\|)"),
                              conciliador)) %>%
  # We have to apply i, since some names are repeated three times
  mutate(conciliador = ifelse(str_detect(conciliador, pattern = "\\|"),
                              str_remove(conciliador, pattern = "\\|.*"),
                              conciliador)) %>%
  # Some names have the suffix "SUBDIR. A/B/C)". We delete those
  mutate(conciliador = str_remove(conciliador, pattern = "( )?( |\\()SUB(D)?IR(\\.)? (A|B|C)(|\\))")) %>%
  
  # Delete observations from a conciliatior we don't have the complete name from.
  filter(conciliador != "M HERNÁNDEZ BAJA") %>%
  
  # Drop observations where we don't know the worker's gender
  filter(!is.na(genero)) %>%
  
  # Drop observation with non-numeric age
  mutate(edad = as.numeric(edad)) %>%
  filter(!is.na(edad)) %>%
  
  # Drop observations from a weird request that has a 2020/2022 hearing date
  filter(solicitud_id != 26533,
         solicitud_id != 31719,
         solicitud_id != 34989,
         solicitud_id != 33841,
         solicitud_id != 35403,
         solicitud_id != 41103)

hearings_features <- hearings_clean %>%
  # Cleaning the hearing hour of each hearing, as it will be usefull to determine if the hearing was quasirandomly assigned
  mutate(hearing_hour = hour(hora_inicio_audiencia)) %>%
  
  mutate(hearing_hour_30m= round(as.numeric(hora_inicio_audiencia)/1800)/2) %>%
  
  mutate(hora_inicio_audiencia_as_numeric=as.numeric(hora_inicio_audiencia))


# Code manually the conciliator gender
women_names <- c("ABISH", "AIDE", "ALINA", "ALMA", "ANA", "ANDREA", "CINTHIA", "CLARA", 
                 "DULCE", "EDITH", "EDNNA", "ELSA", "ESTHELA", "ESTHER", "GABRIELA",
                 "HILLARY", "INGRID", "JENNIFER", "JENNY", "JESSICA", "JHELENI", 
                 "JOSEFINA", "KAREN", "LAURA", "LORENA", "LUZ", "MARGARITA", "MARIA", 
                 "MARIANA", "MARISOL", "MARLA", "MARLEN", "MARÍA", "MIRIAM", "MITZY", 
                 "MÓNICA", "NADIA", "NANCY", "ROSA", "SUSANA", "TERESA", "VALERIA", "VIANEY")

# Function to check if the name is associated with women
is_woman <- function(full_name, women_names) {
  any(str_detect(tolower(full_name), str_c("\\b", str_to_lower(women_names), "\\b")))
}


hearings_features <- hearings_features %>%
  
  # Code manually the conciliator gender
  mutate(conciliador_gen = ifelse(sapply(conciliador, is_woman, women_names = women_names), 1, 0)) %>%

# Create dummies for what can be control and outcome variables
mutate(genero = as.numeric(genero == "FEMENINO"),
       inmediata = as.numeric(inmediata),
       comercio = as.numeric(codigo_scian_2digit == 43 | codigo_scian_2digit == 46),
       corporativos = as.numeric(codigo_scian_2digit == 55)) %>%
  
  # Create a variable for the number of workers in each request
  group_by(audiencia_id) %>%
  mutate(num_trabajadores = n()) %>%
  ungroup() %>%
  group_by(solicitud_id) %>%
  mutate(num_trabajadores = max(num_trabajadores)) %>%
  ungroup() %>%
  # Create a dummy where the request has more than one worker (i dont use this variable)
  mutate(dos_o_mas_trab = as.numeric(num_trabajadores > 1)) %>%
  #keep only observations with one worker
  group_by(solicitud_id) %>%
  #rember the data set was at the level hearing-worker, so now the data set should be at the level hearing. 
  filter(num_trabajadores == 1) %>%
  ungroup()  %>%
  # Create a variable with the time between the last and the first hearing of a request.
  group_by(solicitud_id,parte_id) %>%
  mutate(fecha_ultima_audiencia_sol_par=max(fecha_audiencia))  %>%
  mutate(fecha_primera_audiencia_sol_par=min(fecha_audiencia))  %>%
  mutate(tiempo_en_resolver=fecha_ultima_audiencia_sol_par-fecha_primera_audiencia_sol_par)  %>%
  ungroup()

#Add comparecencias data base to the hearing features data

comparecientes <- read.csv(here("01_Data","01_Raw","comparecientes.csv")) 

#for some reason there are repreted observations, etiher they got updated or they got eliminated
comparecientes <- comparecientes %>%
  mutate(presentado=1) %>%
  distinct(parte_id,audiencia_id, .keep_all = TRUE) %>%
  select("audiencia_id","parte_id","presentado") 

worker_request_fake_drops <- left_join(hearings_features,comparecientes, by=c("audiencia_id","parte_id")) %>%
  mutate(presentado=if_else(is.na(presentado),0,if_else(presentado==1,1,NA))) %>%
  mutate(citado_trabajador_comparece=if_else(is.na(citado_trabajador_comparece),1,citado_trabajador_comparece))  

# Only first hearings are quasi random, so ill reduce this data base -------------------------
# to the level reqest- first hearing ---------------------------------------------------------

# before that lets save the worker-hearing-request data base
# Save the worker - hearing database.
write_csv(hearings_features, file = here("01_Data",
                                         "02_Created",
                                         "worker_hearing.csv"))

# reduce hearing features to the first hearing
hearings_features <- hearings_features %>%
  
  # Get the final outcome of the request
  group_by(parte_id) %>%
  mutate(max_hubo_convenio = max(hubo_convenio),
         max_no_hubo_convenio = max(no_hubo_convenio),
         max_archivado = max(archivado),
         max_numero_audiencia = max(numero_audiencia)) %>%
  ungroup() %>%
  
  # Keep one observation request only the first hearing
  # Keep only first hearings
  filter(numero_audiencia == 1) %>%
  filter(audiencia_finalizada == TRUE) %>%
  
  # Some workers have several first hearings (why?)
  # Keep the first appearance in the database, since it is ordered by creation date
  distinct(parte_id, .keep_all = TRUE)  %>%
  # remove hearings that happend before 2023
  filter(fecha_audiencia> "2023-01-01") %>%
  # remove hearings that are nos quasirandom or, what is the same, assigned by an algorithm .
  mutate(hora_inicio_audiencia_as_numeric_aux_filter=(hora_inicio_audiencia_as_numeric+3600)) %>%
  
  mutate(hora_inicio_audiencia_as_numeric_aux_filter= hora_inicio_audiencia_as_numeric_aux_filter %% 5400) %>%
  
  filter(hora_inicio_audiencia_as_numeric_aux_filter == 0 & hora_inicio_audiencia_as_numeric <=50400)


# Save the request-first hearing- one worker features
write_csv(hearings_features, file = here("01_Data",
                                         "02_Created",
                                         "worker_first.csv"))

# CLEAN PAYMENTS DATA ----------------------------------------------------------
# This data is at worker-payment level. A conciliation process that ends up in an
# agreement may involve several payments, which can be made in installments. This
# database has the agreed payment date, and for payments due, if they were in fact paid.

# Load conciliation payments data
payments <- read_csv(str_c("01_Data/01_Raw/completo-datos-pagos-fuente_",
                           update_date,
                           ".csv")) %>%
  
  # Make the payment date actually a date
  mutate(fecha_cumplimiento = date(fecha_cumplimiento)) %>%
  group_by(parte_id) %>%
  # Collapse the database at worker level, i.e. outcome level
  summarise(monto = sum(monto, na.rm = T),
            fecha_cumplimiento = min(fecha_cumplimiento, na.rm = T),
            pagado = max(pagado, na.rm = T),
            diferido = max(diferido, na.rm = T)) %>%
  
  # Replace the cases where we have a -Inf with NA. The -Inf means all within
  # group observations were NA's.
  mutate(pagado = ifelse(pagado == -Inf, NA, pagado)) %>%
  ungroup()

# Save the worker - payment order total database.
write_csv(payments, file = here("01_Data",
                                "02_Created",
                                "worker_payment_order.csv"))


# CLEAN CHARACTERISTICS AND PROPOSALS DATA -------------------------------------

# Load salario minimo
salario_minimo <-  read_csv("01_Data/01_Raw/salario_minimo.csv") %>%
  select(anio_termino,sal_min) %>%
  filter(!is.na(sal_min))

# This data is at worker-employer-hearing level. This is because the data includes
# the termination of the hearing, as well as the CURP for each worker and the RFC
# for each employer (if available)
charac_proposal <- read_delim(str_c("01_Data/01_Raw/reporte-econometria-itam_",
                                    update_date,
                                    ".csv"),
                              delim = "|") %>%
  # Create daily wage and tenure
  mutate(sal_diario = case_when(
    periodicidad_remuneracion == "Mensual" ~ remuneracion / 30,
    periodicidad_remuneracion == "Quincenal" ~ remuneracion / 15,
    periodicidad_remuneracion == "Semanal" ~ remuneracion / 7,
    .default = remuneracion
  ),
  antig = as.numeric(fecha_salida - fecha_ingreso),
  diurna = as.numeric(jornada == "DIURNA"),
  quincenal = as.numeric(periodicidad_remuneracion == "Quincenal")) %>%
  
  # Clean certain problems with the variables
  # Replace with NA's if tenure is negative, and trim the top 0.005% tenure value.
  mutate(antig = case_when(antig <= 0 ~ NA,
                           antig > quantile(antig, na.rm = T, probs = 0.995) ~ NA, 
                           T ~ antig),
         # Replace with NA's if the daily wage is zero, and trim the top 0.005% wages.
         sal_diario = case_when(sal_diario == 0 ~ NA,
                                sal_diario > quantile(sal_diario, na.rm = T, probs = 0.995) ~ NA,
                                T ~ sal_diario)
  ) %>%
  
  # Create variable minimo de ley 
  # Get the minimum wage for the year the worker ended it's work relation
  mutate(anio_termino = case_when(!is.na(fecha_salida) ~ year(fecha_salida)),
         anio_ingreso = case_when(!is.na(fecha_ingreso) ~ year(fecha_ingreso))) %>%
  left_join(salario_minimo, by = "anio_termino") %>%
  
  # Calculate the proportion of this year worked
  mutate(
    antiguedad_anio_actual = case_when(
      !is.na(fecha_salida) & anio_termino!=anio_ingreso ~ round(as.numeric(fecha_salida - as_date(paste(anio_termino,"01","01",sep="-")))/365, 2),
      !is.na(fecha_salida) & anio_termino==anio_ingreso ~ round(as.numeric((fecha_salida-fecha_ingreso))/ 365,2),
      .default = NA
    ),
    
    # Tenure in years
    antiguedad_anios = round(antig/365, 2),
    
    # Calculate the days of vacation according to tenure
    # If you are fired within your first year, you get the proportional vacations for that year
    dias_vacaciones = case_when(
      antiguedad_anios < 1 ~ 6*antiguedad_anios,
      antiguedad_anios < 5 ~ 6 + (floor(antiguedad_anios) - 1)*2,
      T ~ 12 + floor(antiguedad_anios/5)*2
    )) %>%
  
  # Calculate payments that should be made to the worker
  mutate(c_indemnizacion = round(90*sal_diario, 2),
         c_prima_antig = case_when(
           sal_diario < 2*sal_min ~ round(antiguedad_anios*12*sal_diario, 2),
           T ~ round(antiguedad_anios*12*2*sal_min, 2)
         ),
         c_aguinaldo = round(antiguedad_anio_actual*15*sal_diario, 2),
         c_vacaciones = round(dias_vacaciones*1.25*sal_diario, 2)) %>%
  
  # Law minimum variable
  mutate(minimo_de_ley = c_indemnizacion + c_prima_antig + c_aguinaldo + c_vacaciones)


# Create characteristics database at worker-request level
# from the charcateristics keep only important variables, and winsorize data. 
worker_characteristics <- charac_proposal %>%
  select(solicitud_id, parte_trabajador_id, audiencia_id,numero_audiencia, horas_semanales, sal_diario, antig, diurna,
         quincenal, c_indemnizacion, c_prima_antig, c_aguinaldo, c_vacaciones, minimo_de_ley, texto_conciliador,fecha_salida,fecha_ingreso,
         curp_patron,curp_trabajador,rfc_trabajador,rfc_patron) %>%
  rename(parte_id = parte_trabajador_id) %>%
  distinct(solicitud_id, parte_id, audiencia_id, numero_audiencia, horas_semanales, sal_diario, antig, diurna,
           quincenal, c_indemnizacion, c_prima_antig, c_aguinaldo, c_vacaciones, minimo_de_ley, .keep_all = TRUE) %>%
  filter(horas_semanales < quantile(horas_semanales,c(.05,.99),na.rm=TRUE)[2]) %>%
  filter(sal_diario < quantile(sal_diario,c(.05,.99),na.rm=TRUE)[2]) %>%
  filter(antig < quantile(antig,c(.05,.99),na.rm=TRUE)[2]) 


# worker-hearing-request-level----------------------------------------------------------------------------------
# remeber this data base is at the level worker-hearing-request-employer
# so from all hearings with a employer I want only one charcateristics of the worker

worker_hearing_request_characteristics <- worker_characteristics %>%
  group_by(solicitud_id,parte_id,audiencia_id, numero_audiencia) %>%
  summarise( horas_semanales = mean(horas_semanales),
             sal_diario = mean(sal_diario),
             antig = mean(antig),
             diurna = mean(diurna),
             quincenal = mean(quincenal),
             c_indemnizacion = mean(c_indemnizacion, na.rm = T),
             c_prima_antig = mean(c_prima_antig, na.rm = T),
             c_aguinaldo = mean(c_aguinaldo, na.rm = T),
             c_vacaciones = mean(c_vacaciones, na.rm = T),
             minimo_de_ley = mean(minimo_de_ley, na.rm = T),
             numero_de_trabajadores = n()) %>%
  ungroup()

# add again the data that cannot be summarized, rfc curp etc.
worker_characteristics <- worker_characteristics %>%
  select(parte_id,curp_patron,curp_trabajador,rfc_trabajador,rfc_patron,fecha_salida,fecha_ingreso)  %>%
  distinct(parte_id,.keep_all = T)

# worker hearing request characteristics is at the level worker hearing request, therefore if I merge the data at the level worker, ill have the 
# parte_id,curp_patron,curp_trabajador,rfc_trabajador,rfc_patron,fecha_salida,fecha_ingreso of the worker 
#be careful, rfc and other employer features might not be correct as the abstraction level made us miss the employer level
worker_hearing_request_characteristics <- left_join(worker_hearing_request_characteristics,worker_characteristics, by=c("parte_id"))

# Save the worker-hearing-request characteristics database.
write_csv(worker_hearing_request_characteristics, file = here("01_Data",
                                                              "02_Created",
                                                              "worker_hearing_request_characteristics.csv"))

## CONCILIATOR LEVEL DATA ------------------------------------------------------
conciliator <- hearings_features %>%
  
  # Get the first hearing and last hearing date for each conciliator
  group_by(conciliador) %>%
  mutate(first_hearing = min(fecha_audiencia),
         last_hearing = max(fecha_audiencia)) %>%
  ungroup() %>%
  
  # Now get the number of hearings per day
  group_by(conciliador, fecha_audiencia) %>%
  mutate(audiencias = n()) %>%
  ungroup() %>%
  
  # Now get the average hearings per day
  group_by(conciliador) %>%
  summarise(audiencias = mean(audiencias),
            first_hearing = mean(first_hearing),
            last_hearing = mean(last_hearing)) %>%
  ungroup() %>%
  
  # Calculate the experience of each conciliator
  mutate(experience = last_hearing - first_hearing)

# Save the conciliator database.
write_csv(conciliator, file = here("01_Data",
                                   "02_Created",
                                   "conciliator.csv"))





