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






