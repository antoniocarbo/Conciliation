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


# Hisotgrams of gender percentage for different number of hearings 

base_analisis <- base_analisis %>%
  mutate(maxx_audiencia=case_when(max_numero_audiencia == 1 ~ "One hearing",
                                  max_numero_audiencia == 2 ~ "Two hearings",
                                  max_numero_audiencia >= 3 ~ "Three or more hearings",
                                  .default = NA
  ))

base_analisis$maxx_audiencia <- factor(base_analisis$maxx_audiencia, levels = c("One hearing", "Two hearings", "Three or more hearings"))

summary_audiencia_genero <- base_analisis %>%
  group_by(maxx_audiencia,genero) %>%
  summarise(count=n())
  

summary_audiencia_genero <- summary_audiencia_genero %>%
  group_by(maxx_audiencia) %>%
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  mutate(porcentaje=count/suma)

summary_audiencia_genero <- summary_audiencia_genero %>%
  mutate(genero=case_when(genero == 0 ~ "Male worker",
                          genero == 1 ~ "Female worker",
                          .default = NA))


ggplot(summary_audiencia_genero, aes(x = as.factor(genero), y = porcentaje, fill = maxx_audiencia)) +
  geom_bar(stat = "identity") +
  facet_wrap(~maxx_audiencia) +  # Crea gráficos separados para cada audiencia
  labs( y = "Percentage",
       x = "Gender",
       fill = "Hearing") +  theme_minimal()  

ggsave("04_Figures/distribution_gender_hearing.pdf")

rm(summary_audiencia_genero)

  
# Hisotgrams of dismissed for different number of hearings 


summary_audiencia_despido <- base_analisis %>%
  group_by(maxx_audiencia,despido) %>%
  summarise(count=n())


summary_audiencia_despido <- summary_audiencia_despido %>%
  group_by(maxx_audiencia) %>%
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  mutate(porcentaje=count/suma)

summary_audiencia_despido <- summary_audiencia_despido %>%
  mutate(despido=case_when(despido == 0 ~ "Not dismissed",
                          despido == 1 ~ "Dismissed",
                          .default = NA))

ggplot(summary_audiencia_despido, aes(x = as.factor(despido), y = porcentaje, fill = maxx_audiencia)) +
  geom_bar(stat = "identity") +
  facet_wrap(~maxx_audiencia) +  # Crea gráficos separados para cada audiencia
  labs( y = "Percentage",
        x = "Dismissal",
        fill = "Hearing") +  theme_minimal()  

ggsave("04_Figures/distribution_dismissal_hearing.pdf")

rm(summary_audiencia_despido)

# Desnity of wage distrbution for different number of hearings 


ggplot(base_analisis, aes(x = sal_diario, fill=maxx_audiencia)) +
  geom_density() +  # Agregar la curva de densidad con color
  facet_wrap(~maxx_audiencia) +  # Crea gráficos separados para cada audiencia
  labs( y = "Density",
       x = "Wage",
       fill="Hearing") +  # Etiquetas de los ejes
  theme_minimal()


ggsave("04_Figures/distribution_wage_hearing.pdf")

# Desnity of tenure distrbution for different number of hearings 

ggplot(base_analisis, aes(x = antig, fill=maxx_audiencia)) +
  geom_density() +  # Agregar la curva de densidad con color
  facet_wrap(~maxx_audiencia) +  # Crea gráficos separados para cada audiencia
  labs( y = "Density",
        x = "Tenure",
        fill="Hearing") +  # Etiquetas de los ejes
  theme_minimal()


ggsave("04_Figures/distribution_tenure_hearing.pdf")

# Desnity of weekly hours distrbution for different number of hearings 



ggplot(base_analisis, aes(x = horas_semanales, fill=maxx_audiencia)) +
  geom_density() +  # Agregar la curva de densidad con color
  facet_wrap(~maxx_audiencia) +  # Crea gráficos separados para cada audiencia
  labs( y = "Density",
        x = "Weekly hours",
        fill="Hearing") +  # Etiquetas de los ejes
  theme_minimal()


ggsave("04_Figures/distribution_weekly_h__hearing.pdf")

# Desnity of workers age distrbution for different number of hearings 


ggplot(base_analisis, aes(x = edad, fill=maxx_audiencia)) +
  geom_density() +  # Agregar la curva de densidad con color
  facet_wrap(~maxx_audiencia) +  # Crea gráficos separados para cada audiencia
  labs( y = "Density",
        x = "Workers age",
        fill="Hearing") +  # Etiquetas de los ejes
  theme_minimal()


ggsave("04_Figures/distribution_workers_age_hearing.pdf")



