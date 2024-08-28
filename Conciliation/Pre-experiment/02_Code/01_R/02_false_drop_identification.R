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
# Outputs: final_base
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


