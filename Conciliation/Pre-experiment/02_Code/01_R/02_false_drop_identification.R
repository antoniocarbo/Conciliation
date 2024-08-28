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
                                                          "worker_hearing_request_characteristics.csv.csv"))
