# -----------------------------------------------------------------------------
#                              Conciliation project
# 
# Code author: Antonio Carbonell
# Date: August 28, 2024
#
# Code modifications: 
# Date of modifications:
#
# Objective: Create figures and tables of false drop interaction 
#
# Data inputs: base_with_fake_drops.csv
# Outputs: 
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


base_analisis <- read.csv(here("01_Data",
                           "02_Created",
                           "base_with_fake_drops.csv"))

# -----------------------------------------------------------------------------
#                               Numbers
# -----------------------------------------------------------------------------

numero_de_observaciones <- as.numeric(nrow(base_analisis))




