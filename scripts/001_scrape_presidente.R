# Load packages ###############################################################

library(jsonlite)
library(dplyr)
library(stringr)
library(rio)
library(here)

# Get names and ids of candidates and parties #################################

candidatos_partidos_list <- fromJSON("https://data-resultados.te.gob.pa/presentacion/eventos/100/candidatos/1/index.json?sv=2023-01-03&ss=btqf&srt=sco&st=2024-03-23T14%3A48%3A10Z&se=2024-12-31T05%3A00%3A00Z&sp=rl&sig=qcdoSERIfwIvRYX3M0HyeZyOvbNjA4MMWVRI8MCsfOA%3D")

candidatos <- candidatos_partidos_list$circunscripcion$candidatos
partidos <- candidatos_partidos_list$circunscripcion$partidos

candidatos_df <- bind_rows(candidatos)
partidos_df <- bind_rows(partidos)

# Clean candidate and party names #############################################

candidatos_df <- candidatos_df %>%
    select(-c(foto, logo)) %>%
    rename(independiente = ind) %>%
    mutate(
        across(c(nombre, alias, alianza), ~ str_to_title(.)),
        independiente = ifelse(independiente == TRUE, 1, 0),
        partido = unlist(partidos),
        partido = ifelse(is.na(partido), nomina, partido)
    ) %>%
    select(-partidos)

# Save data ###################################################################

export(candidatos_df, here("data/clean/candidatos_presidente.csv"))
export(partidos_df, here("data/clean/partidos_presidente.csv"))