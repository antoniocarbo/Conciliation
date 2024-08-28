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

cat(numero_de_observaciones, file=here("06_Numbers",
                                       "numero_observaciones_base_with_fake_drops"))

numero_de_observaciones_fd <- base_analisis %>%
  filter(false_drop_time_25_dropped==1) 


numero_de_observaciones_fd <- as.numeric(nrow(numero_de_observaciones_fd))

cat(numero_de_observaciones_fd, file=here("06_Numbers",
                                       "numero_de_fd_observaciones_base_with_fake_drops"))

number_of_times_worker <-base_analisis%>%
  filter(cuanto_repiten_curp_trabajador>1)  

number_of_times_worker <- as.numeric(nrow(number_of_times_worker))

cat(number_of_times_worker, file=here("06_Numbers",
                                       "numero_de_casos_que_repiten_trab"))


# -----------------------------------------------------------------------------
#                               Figures
# -----------------------------------------------------------------------------


# Histogram of the number of times a person visited the conciliation center

base_analisis %>%
  distinct(curp_trabajador, .keep_all = T) %>%
  filter(cuanto_repiten_curp_trabajador>1) %>%
  ggplot(aes(x =factor(cuanto_repiten_curp_trabajador))) +
  geom_bar(  fill = "#004586") +
  xlab("Number of times a person \n went to the conciliation center") +
  ylab("Frequency") 

ggsave("04_Figures/fin_repeated_curp_hist.pdf")


#time between audiencia and next solicitud

base_analisis %>%
  filter(cuanto_repiten_curp_trabajador>1) %>%
  ggplot(aes(x=tiempo_entre_audiencia_y_siguiente_solicitud)) +
  geom_density(fill = "#004586")+
  xlab("Time between the hearing and the next \n request made by the same worker") +
  ylab("Density") +
  scale_x_continuous(breaks = c(0, 25, 50,75,100,125,150,175,200,225,250,275,300))

ggsave("04_Figures/fin_time_hearing_request_curp.pdf")






