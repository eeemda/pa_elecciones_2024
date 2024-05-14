# Name: 003_clean_locations.R

# Load packages ###############################################################

library(groundhog)
groundhog_day <- "2024-05-01"
pkgs <- c("dplyr", "janitor", "rio", "here")
groundhog.library(pkgs, groundhog_day)

# Load data ###################################################################

provincias_df <- import(here("data/raw/provincias.csv"))
distritos_df <- import(here("data/raw/distritos.csv"))
corregimientos_df <- import(here("data/raw/corregimientos.csv"))
centros_df <- import(here("data/raw/centros.csv"))

# Join all location data ######################################################

centro_locations_df <- provincias_df %>%
    right_join(., distritos_df, by = "provinciaId") %>%
    right_join(., corregimientos_df, by = c("provinciaId", "distritoId")) %>%
    right_join(
        ., centros_df,
        by = c("provinciaId", "distritoId", "corregimientoId")
    )

# Clean location data #########################################################

centro_locations_df <- centro_locations_df %>%
    clean_names() %>%
    select(-c(starts_with("key_"), starts_with("codigo_"))) %>%
    rename(
        provincia_nombre = nombre_provincia_x,
        distrito_nombre = nombre_provincia_y,
        corregimiento_nombre = nombre_corregimiento,
        centro_votacion_nombre = nombre_centro
    )

# Saving data #################################################################

export(centro_locations_df, here("data/clean/centros.csv"))