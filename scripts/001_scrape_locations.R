# Load packages ###############################################################

library(jsonlite)
library(dplyr)
library(stringr)
library(future.apply)
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

# Get names and ids of all subnational units ####################################
# Provincias, distritos, corregimientos, centros, y mesas

provincias_json <- fromJSON("https://data-resultados.te.gob.pa/presentacion/eventos/100/circunscripciones/index.json?sv=2023-01-03&ss=btqf&srt=sco&st=2024-03-23T14%3A48%3A10Z&se=2024-12-31T05%3A00%3A00Z&sp=rl&sig=qcdoSERIfwIvRYX3M0HyeZyOvbNjA4MMWVRI8MCsfOA%3D")

provincias_list <- provincias_json$descendientes

provincias_df <- provincias_list %>%
    select(-descendientes) %>%
    rename(nombre_provincia = descripcion) %>%
    mutate(
        nombre_provincia = str_to_title(nombre_provincia),
        nombre_provincia = case_when(
            nombre_provincia == "Voto Rere" ~ "Voto RERE",
            nombre_provincia == "Voto Reva" ~ "Voto REVA",
            TRUE ~ nombre_provincia
        )
    )

# Get distritos
distritos_list <- future_lapply(
    seq_along(provincias_list$descendientes),
    function(i) provincias_list$descendientes[i]
)

distritos_df <- distritos_list %>%
    bind_rows() %>%
    select(-descendientes) %>%
    rename(nombre_provincia = descripcion) %>%
    mutate(
        nombre_provincia = str_to_title(nombre_provincia)
    )

# Get list of corregimientos
corregimientos_list <- list()
for(i in 1:length(distritos_list)) {
    corregimientos_list[[i]] <- distritos_list[[i]][[1]]$descendientes %>%
        bind_rows()
}

corregimientos_df <- bind_rows(corregimientos_list) %>%
    select(-descendientes) %>%
    rename(nombre_corregimiento = descripcion) %>%
    mutate(
        nombre_corregimiento = str_to_title(nombre_corregimiento),
        cabecera = ifelse(str_detect(nombre_corregimiento, "Cabec."), 1, 0),
        nombre_corregimiento = str_replace_all(
            nombre_corregimiento, "\\(Cabec\\.\\)", ""
        )
    )

# Get list of centros
centros_list <- list()
for(i in 1:length(corregimientos_list)) {
    centros_list[[i]] <- corregimientos_list[[i]]$descendientes %>%
        bind_rows()
}

centros_df <- bind_rows(centros_list) %>%
    rename(nombre_centro = descripcion) %>%
    mutate(nombre_centro = str_to_title(nombre_centro))

# Save data ###################################################################

export(candidatos_df, here("data/clean/candidatos_presidente.csv"))
export(partidos_df, here("data/clean/partidos_presidente.csv"))
export(provincias_df, here("data/clean/provincias.csv"))
export(distritos_df, here("data/clean/distritos.csv"))
export(corregimientos_df, here("data/clean/corregimientos.csv"))
export(centros_df, here("data/clean/centros.csv"))
