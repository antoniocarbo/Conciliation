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

summary_df <- base_analisis %>%
  group_by(max_numero_audiencia) %>%
  summarise(
    Relative_Frequency = n()/nrow(base_analisis)*100)

summary_df %>%
  ggplot(aes(x= max_numero_audiencia, y= Relative_Frequency)) +
  geom_bar(stat = "identity",color="blue4", fill="blue4")  +
  labs(y ='Relative Frequency', x='Number of hearings')


## AVERAGE NUMBER OF ONE WORKER HEARINGS PER CONCILIATOR ---------------------------------

conciliator <- conciliator[order(conciliator$audiencias, decreasing = F), ] %>%
  mutate(id = c(1:nrow(conciliator)))

ggplot(conciliator, 
       aes(x = id)) +
  geom_col(aes(x = id, y = audiencias),
           fill = "#004586",
           color = "white") +
  labs(x = "Conciliator", y = "Average Daily One Worker Hearings") +
  theme_classic()

ggsave("04_Figures/conciliator_avg_hearings.pdf")







