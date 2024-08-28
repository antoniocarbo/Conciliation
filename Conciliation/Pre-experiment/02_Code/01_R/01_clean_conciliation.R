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
#' 

# Input the last update date
#update_date <- "2023-07-25"
update_date <- "2023-10-17"

# Load libraries
pacman::p_load(here, readr, lubridate, dplyr, tidyr, stringr, readxl)


# CLEAN HEARINGS DATA ----------------------------------------------------------
# This data is at worker-hearing level. A conciliation request may involve several
# workers, and several hearings. Some variables are at request level, some at worker
# and some at hearing level.

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




