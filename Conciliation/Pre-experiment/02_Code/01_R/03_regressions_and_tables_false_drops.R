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

pacman::p_load(here, tidyverse, fixest, knitr, kableExtra, vtable, haven, RCT,stargazer)

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

writeLines(toString(numero_de_observaciones), here("06_Numbers",
                                       "numero_observaciones_base_with_fake_drops.tex"))

numero_de_observaciones_fd <- base_analisis %>%
  filter(false_drop_time_25_dropped==1) 


numero_de_observaciones_fd <- as.numeric(nrow(numero_de_observaciones_fd))

writeLines(toString(numero_de_observaciones_fd), here("06_Numbers",
                                       "numero_de_fd_observaciones_base_with_fake_drops.tex"))

number_of_times_worker <-base_analisis%>%
  filter(cuanto_repiten_curp_trabajador>1)  

number_of_times_worker <- as.numeric(nrow(number_of_times_worker))

writeLines(toString(number_of_times_worker), here("06_Numbers",
                                       "numero_de_casos_que_repiten_trab.tex"))


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


# -----------------------------------------------------------------------------
#                               Tables
# -----------------------------------------------------------------------------

# Change the scale of values so the regressions are interpretable

base_analisis <- base_analisis %>%
  mutate(sal_diario=sal_diario/100) %>%
  mutate(horas_semanales = horas_semanales/10) %>%
  mutate(antig = antig/(365*5)) %>%
  mutate(edad = edad/10) 

# summary stats

summary_stats_false_drop_time_25_dropped <- base_analisis %>%
  filter(false_drop_time_25_dropped==1) %>%
  select(genero,edad,despido, horas_semanales,sal_diario,antig)

stargazer(as.data.frame(summary_stats_false_drop_time_25_dropped),type= "text" , out=here("03_Tables","fin_summary_stats_false_drop_time_25_dropped.tex") )


#general balance table 

regresion_balance_time_25_dropped <- feols(false_drop_time_25_dropped~ genero + edad + despido + horas_semanales +sal_diario+antig+numero_citados, data=base_analisis)

etable(regresion_balance_time_25_dropped,
       dict = c("antig" = "Tenure 5",
                "horas_semanales" = "Weekly Hours 10",
                "despido" = "Disissal","edad" = "Age 10","genero" = "Gender",
                "sal_diario" = "Wage 100",
                "false_drop_time_25_dropped"="False drop 25",
                "citados_mayor_comparecientes"="One employer didnt showed up",
                "numero_citados"="N. of summoned",
                "(Intercept)" = "Constant"),
       order = c("!Constant"),
       title = "Effect of request characteristics on false drop",
       digits = "r3",
       fitstat = ~ my + n,
       replace = TRUE,
       file = here("03_Tables",  "fin_false_drop_time_25_balance_reg_dropped.tex"))

# general comparison charcateristics

regresion_false_drop_time_25_nc_as_dropped <- feols(numero_citados_asump_time_25~false_drop_time_25_dropped, data=base_analisis)
regresion_false_drop_time_25_cc_as_dropped <- feols(citados_comparecen_audiencia_asump_time_25~false_drop_time_25_dropped, data=base_analisis)
regresion_false_drop_time_25_dnc_as_dropped <- feols(diferencia_citados_comaprecientes_asump_time_25~false_drop_time_25_dropped, data=base_analisis)

etable( regresion_false_drop_time_25_nc_as_dropped,
        regresion_false_drop_time_25_cc_as_dropped,
        regresion_false_drop_time_25_dnc_as_dropped,
        dict = c("numero_citados_asump_time_25" = "N. of summoned",
                 "citados_comparecen_audiencia_asump_time_25" = "N. that appeared",
                 "diferencia_citados_comaprecientes_asump_time_25" = "Diff. Sum. A.",
                 "false_drop_time_25_dropped"="False drop 25",
                 "(Intercept)" = "Constant"),
        order = c("!Constant"),
        title = "Effect of false drop on next claim characteristics",
        digits = "r3",
        fitstat = ~ my + n,
        replace = TRUE,
        file = here("03_Tables",  "fin_false_drop_time_25_charac_as_reg_dropped.tex"))

# general comparison oucomes


regresion_false_drop_time_25_conevenio_as_dropped <- feols(max_hubo_convenio_asump_time_25~false_drop_time_25_dropped, data=base_analisis)
regresion_false_drop_time_25_no_convenio_as_dropped <- feols(max_no_hubo_convenio_asump_time_25~false_drop_time_25_dropped, data=base_analisis)
regresion_false_drop_time_25_numero_audiencia_as_dropped <- feols(max_numero_audiencia_asump_time_25~false_drop_time_25_dropped, data=base_analisis)
regresion_false_drop_time_25_archivado_as_dropped <- feols(max_archivado_asump_time_25~false_drop_time_25_dropped, data=base_analisis)

etable(  regresion_false_drop_time_25_conevenio_as_dropped,
         regresion_false_drop_time_25_no_convenio_as_dropped,
         regresion_false_drop_time_25_numero_audiencia_as_dropped,
         regresion_false_drop_time_25_archivado_as_dropped,
         dict = c("max_numero_audiencia_asump_time_25" = "N. of hearings",
                  "max_no_hubo_convenio_asump_time_25" = "No conciliation",
                  "max_hubo_convenio_asump_time_25" = "Conciliation",
                  "max_archivado_asump_time_25"="Drop",
                  "false_drop_time_25_dropped"="False drop 25",
                  "(Intercept)" = "Constant"),
         order = c("!Constant"),
         title = "Effect of false drop on next claim outcomes",
         digits = "r3",
         fitstat = ~ my + n,
         replace = TRUE,
         file = here("03_Tables",  "fin_false_drop_time_25_outcomes_reg_as_dropped.tex"))


#Notes to talk about tonigh 
#Problems with notification analysis, aunque te dieron el dato por trabajdor, te dieron el numero total de patrones y el numero total de comparecientes y citados entonces no puedo saber cual es cual, es decir un patron en particular no asistio, ademas elimine los casos con muchos trabjadores cuando esos casos podii1ía ser en realidad casos con un trabajador pero muchos patrones 
# to solve this problem meanwhile ill do an analysis on cases where I only have one notificado 
#problema oblivamente el filtro no es aletorio, mas citados, menos prob de notifcarlos a todos mas prob de false drop

#Creating some variables for analysis
# If all summos where notified by who the casefiles says, dummy 1,0

base_analisis <- base_analisis %>%
  mutate(citados_mayor_comparecientes=if_else(numero_citados>citados_comparecen_audiencia,1,0))  %>%
  mutate(citacion_por_trabajador=if_else(citatorios_entrga_solicitante==numero_citados,1,0)) %>%
  mutate(citacion_por_notificador=if_else(citatorios_entrga_notificador==numero_citados,1,0)) %>%
  mutate(citacion_conjunta=if_else(citatorios_notificador_solicitante==numero_citados,1,0)) %>%
  mutate(citacion_electronica=if_else(citatorios_buzon_electronico==numero_citados,1,0)) 

base_analisis <- base_analisis %>%
  group_by(curp_trabajador)  %>%
  mutate(fecha_ultima_audiencia=max(as.Date(fecha_ultima_audiencia_sol_par))) %>%
  mutate(fecha_primera_audiencia=min(as.Date(fecha_primera_audiencia_sol_par))) %>%
  mutate(time_bt_firs_and_last=fecha_ultima_audiencia-fecha_primera_audiencia) %>%
  mutate(overall_false_drop25=max(false_drop_time_25_dropped)) %>%
  ungroup()

#---------------------------------------------------------------------------------------------


base_analisis_un_citado <- base_analisis %>%
  filter(numero_citados==1)

reg_balance_not_no_com_false_drop <- feols(false_drop_time_25_dropped ~ presentado, data=base_analisis)
reg_balance_not_type_false_drop <- feols(false_drop_time_25_dropped ~ citacion_por_trabajador + citacion_por_notificador + citacion_conjunta, data=base_analisis)


etable(  reg_balance_not_no_com_false_drop,
         reg_balance_not_type_false_drop,
         dict = c("presentado" = "Employer did show up",
                  "citacion_por_trabajador" = "Worker",
                  "citacion_por_notificador" = "Notifier",
                  "citacion_conjunta"="Joint summons",
                  "false_drop_time_25_dropped"="False drop 25",
                  "(Intercept)" = "Constant"),
         order = c("!Constant"),
         title = "Effect of notification characteristics on false drop",
         digits = "r3",
         fitstat = ~ my + n,
         replace = TRUE,
         file = here("03_Tables",  "fin_notifcation_false_drop.tex"))

#time delay due to false drops (the analysis should be made at level curp)

base_analisis_one_curp_level <- base_analisis %>% 
  distinct(curp_trabajador, .keep_all = T)

reg_false_drop_time_delay <- feols(as.numeric(time_bt_firs_and_last)~overall_false_drop25,data=base_analisis_one_curp_level)

etable(  reg_false_drop_time_delay,
         dict = c("as.numeric(time_bt_firs_and_last)" = "Total process time",
                  "overall_false_drop25"="False drop 25",
                  "(Intercept)" = "Constant"),
         order = c("!Constant"),
         title = "Effect of notification characteristics on false drop",
         digits = "r3",
         fitstat = ~ my + n,
         replace = TRUE,
         file = here("03_Tables",  "fin_time_false_drop.tex"))


