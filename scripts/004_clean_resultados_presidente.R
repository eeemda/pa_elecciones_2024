# Load packages ###############################################################

library(groundhog)
groundhog_day <- "2024-05-01"
pkgs <- c(
    "dplyr", "tidyr", "stringr", "janitor", "data.table", "future.apply", "rio",
    "here"
)
groundhog.library(pkgs, groundhog_day)

# Load all results data #######################################################

resultados_df <- import(here("data/raw/resultados_totales.csv"))
partidos_df <- import(here("data/raw/partidos_presidente.csv"))
candidatos_df <- import(here("data/raw/candidatos_presidente.csv"))
centros_df <- import(here("data/clean/centros.csv"))

# Clean presidential results ##################################################

resultados_presidente_df <- resultados_df %>%
    filter(cargoId == 1) %>%
    left_join(., partidos_df, by = c("nomina" = "partidoId")) %>%
    left_join(., candidatos_df, by = c("nomina" = "partido")) %>%
    clean_names() %>%
    select(
        -c(cargo_id, escrutado, inconsistencia, inconsistencia_tipo,
            fecha_grabacion, alianza_x, time_scraped, orden, cabecera_id_x,
            candidato_id, cedula, residuo, independiente, cabecera_id_y)
    ) %>%
    rename(
        partido_id = nomina,
        partido_nombre = nombre_x,
        candidato_nombre = nombre_y,
        cabecera_id = nomina_y,
        alianza_nombre = alianza_y,
        centro_votacion_id = centro_id,
        porcentaje_votos_dentro_alianza = porcentaje
    ) %>%
    mutate(
        partido_nombre = str_to_title(partido_nombre),
        partido_nombre = case_when(
            partido_nombre == "Prd" | partido_nombre == "Moca" |
            partido_nombre == "Molirena" |
            partido_nombre == "Pais" ~ str_to_upper(partido_nombre),
            TRUE ~ partido_nombre
        ),
        porcentaje_votos = votos / votos_validos
    )

# Add location de centros #####################################################

resultados_presidente_df <- resultados_presidente_df %>%
    left_join(
        ., centros_df,
        by = c("provincia_id", "distrito_id", "corregimiento_id", "circuito_id",
            "centro_votacion_id"
        )
    ) %>%
    select(
        starts_with("provincia_"), starts_with("distrito_"),
        starts_with("corregimiento_"), starts_with("circuito_"),
        starts_with("centro_votacion_"), starts_with("candidato_"), alias,
        starts_with("partido_"), siglas, alianza_nombre, everything()
    )

export(resultados_presidente_df, here("data/clean/resultados_presidente.csv"))
