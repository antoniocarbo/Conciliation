# -----------------------------------------------------------------------------
#                              Conciliation project
# 
# Code author: Antonio Carbonell
# Date: August 28, 2024
#
# Code modifications: 
# Date of modifications:
#
# Objective: Create figures of conciliators characteristics
#
# Data inputs:
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

conciliator <-  read.csv(here("01_Data",
                                   "02_Created",
                                   "conciliator.csv"))


## HISTOGRAM OF NUMBER OF HEARING PER REQUEST ---------------------------------
# On 01_clean we calculated the number of hearing per request and save it under the name max_numero_audiencia before we filtered for first hearings
# Therefore now we are calculating how many hearings are observations had
summary_df <- base_analisis %>%
  group_by(max_numero_audiencia) %>%
  summarise(
    Relative_Frequency = n()/nrow(base_analisis)*100)

# plot results
summary_df %>%
  ggplot(aes(x= max_numero_audiencia, y= Relative_Frequency)) +
  geom_bar(stat = "identity",color="blue4", fill="blue4")  +
  labs(y ='Relative Frequency', x='Number of hearings')


## AVERAGE NUMBER OF ONE WORKER HEARINGS PER CONCILIATOR AND DAY---------------------------------
#Order concliators according to the number of hearings per day and per conciliator, take in mind that audiencias is a variables created on 01_clean
# that variable was created after we filtered for first hearings so, the number of hearing per conciliator and day, is only taking into account
# one worker and first hearings 
conciliator <- conciliator[order(conciliator$audiencias, decreasing = F), ] %>%
  mutate(id = c(1:nrow(conciliator)))
#plot results
ggplot(conciliator, 
       aes(x = id)) +
  geom_col(aes(x = id, y = audiencias),
           fill = "#004586",
           color = "white") +
  labs(x = "Conciliator", y = "Average Daily One Worker-First Hearing \n Hearings") +
  theme_classic()

ggsave("04_Figures/conciliator_avg_hearings.pdf")

## AVERAGE NUMBER OF HEARINGS PER REQUEST AND CONCILIATOR---------------------------------
# As the data base has only first hearings, and the variable max_numero_audiencia was created at the level request, we can calculate the mean
# per request and conciliator by taking the mean at the level conciliator
base_analisis_summary <- base_analisis %>%
  group_by(conciliador) %>%
  summarise(numero_de_audiencia=mean(max_numero_audiencia))
#order data
base_analisis_summary <- base_analisis_summary[order(base_analisis_summary$numero_de_audiencia, decreasing = F), ] %>%
  mutate(id = c(1:nrow(conciliator)))
# plot results
ggplot(base_analisis_summary, 
       aes(x = id)) +
  geom_col(aes(x = id, y = numero_de_audiencia),
           fill = "#004586",
           color = "white") +
  labs(x = "Conciliator", y = "Average Request One Worker Hearings") +
  theme_classic()


## GRAPH WITH EXPERIENCE OF THE CONCILIATOR IN DAYS -----------------------------------------------

# change the format of dates 
conciliator <- conciliator %>%
  mutate(first_hearing=as.Date(first_hearing)) %>%
  mutate(last_hearing=as.Date(last_hearing)) 
# order  conciliators on first hearing
conciliator <- conciliator[order(conciliator$first_hearing, decreasing = T), ] %>%
  mutate(id = c(1:nrow(conciliator)))
# plots
ggplot(conciliator, 
       aes(y = id)) +
  geom_segment(aes(x = last_hearing, xend = first_hearing, yend = id), 
               color = "black",
               alpha = 0.5) +
  geom_point(aes(x = first_hearing), color = "#579d1c", size = 3, alpha = 0.5) +
  geom_point(aes(x = last_hearing), color = "#004586", size = 3, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Conciliator") +
  theme_classic() +
  theme(panel.grid.major.x = element_line(color = "gray", linetype = "dotted"))


## DENSITY OF THE EXPERIECNE OF THE CONCILAITOR AT THE DAY OF THE FIRST HEARING-------------------------------

base_plot <- left_join(base_analisis,conciliator, by="conciliador")

base_plot <- base_plot %>%
  mutate(experience_first_hearing=as.Date(fecha_audiencia)- first_hearing)

base_plot %>%
  ggplot(aes(x=as.numeric(experience_first_hearing))) +
  geom_density(fill = "dark blue") +
  labs(x ='Experiennce of the conciliator \n at the day of the hearing', y='Density')




