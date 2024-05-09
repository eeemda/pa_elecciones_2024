# Load packages ###############################################################

library(jsonlite)
library(dplyr)
library(stringr)
library(rio)
library(here)

# Get names and ids of candidates and parties #################################

candidatos_partidos_json <- fromJSON("https://data-resultados.te.gob.pa/presentacion/eventos/100/candidatos/1/index.json?sv=2023-01-03&ss=btqf&srt=sco&st=2024-03-23T14%3A48%3A10Z&se=2024-12-31T05%3A00%3A00Z&sp=rl&sig=qcdoSERIfwIvRYX3M0HyeZyOvbNjA4MMWVRI8MCsfOA%3D")

candidatos <- candidatos_partidos_json$circunscripcion$candidatos
partidos <- candidatos_partidos_json$circunscripcion$partidos

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

# Get names and ids of provincias, corregimientos, y centros de votacion ######

provincias_json <- fromJSON("https://data-resultados.te.gob.pa/presentacion/eventos/100/circunscripciones/index.json?sv=2023-01-03&ss=btqf&srt=sco&st=2024-03-23T14%3A48%3A10Z&se=2024-12-31T05%3A00%3A00Z&sp=rl&sig=qcdoSERIfwIvRYX3M0HyeZyOvbNjA4MMWVRI8MCsfOA%3D")

provincias_list <- provincias_json$descendientes

provincias_df <- provincias_list %>%
    select(-descendientes) %>%
    rename(nombre_de_provincia = descripcion) %>%
    mutate(
        nombre_de_provincia = str_to_title(nombre_de_provincia),
        nombre_de_provincia = case_when(
            nombre_de_provincia == "Voto Rere" ~ "Voto RERE",
            nombre_de_provincia == "Voto Reva" ~ "Voto REVA",
            TRUE ~ nombre_de_provincia
        )
    )
# Save data ###################################################################

export(candidatos_df, here("data/clean/candidatos_presidente.csv"))
export(partidos_df, here("data/clean/partidos_presidente.csv"))
export(provincias_df, here("data/clean/provincias.csv"))