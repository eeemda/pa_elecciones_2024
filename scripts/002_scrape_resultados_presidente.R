# Load packages ###############################################################

library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(clock)
library(future.apply)
library(rio)
library(here)

# Load list of centros ########################################################

centros_df <- import(here("data/raw/centros.csv"))

# Codigos de provincias, distritos, corregimientos y centros ##################

codigos_provincias <- centros_df %>%
    distinct(provinciaId) %>%
    pull(provinciaId)

numero_centros_pais <- nrow(centros_df)

url_prefix <- "https://data-resultados.te.gob.pa/presentacion/eventos/100/centros/"
url_suffix <- "/index.json?sv=2023-01-03&ss=btqf&srt=sco&st=2024-03-23T14%3A48%3A10Z&se=2024-12-31T05%3A00%3A00Z&sp=rl&sig=qcdoSERIfwIvRYX3M0HyeZyOvbNjA4MMWVRI8MCsfOA%3D"

# Scrape mesas ################################################################

count <- 1
json_list <- list()
for(b in 1:length(codigos_provincias)) {

        current_provincia <- codigos_provincias[[b]]
        
        codigos_distritos <- centros_df %>%
            filter(provinciaId == current_provincia) %>%
            distinct(distritoId) %>%
            pull(distritoId)

        print(paste0("Distritos ", codigos_distritos))

        for(c in 1:length(codigos_distritos)) {

            current_distrito <- codigos_distritos[[c]]

            codigos_corregimientos <- centros_df %>%
                filter(provinciaId == current_provincia & distritoId == current_distrito) %>%
                distinct(corregimientoId) %>%
                pull(corregimientoId)

            print(paste0("Corregimientos ", codigos_corregimientos))

            for(d in 1:length(codigos_corregimientos)) {

                current_corregimiento <- codigos_corregimientos[[d]]

                codigos_centros <- centros_df %>%
                    filter(provinciaId == current_provincia & distritoId == current_distrito & corregimientoId == current_corregimiento) %>%
                    distinct(centroVotacionId) %>%
                    pull(centroVotacionId)

                print(paste0("Centros ", codigos_centros))

            for(e in 1:length(codigos_centros)) {

                current_centro <- codigos_centros[[e]]

                json_list[[count]] <- paste0(
                        url_prefix, current_provincia, "/", current_distrito,
                        "/", current_corregimiento, "/", current_centro,
                        url_suffix
                    )

                print(
                    paste0(
                        "Provincia ", current_provincia, "; Distrito ",
                        current_distrito, "; Corregimiento ",
                        current_corregimiento, "; Centro ", current_centro
                    )
                )

                print(paste0("Count: ", count, " of ", numero_centros_pais))
                count <- count + 1
            }
        }
    }
}

resultadosExtract <- function(link) {

    json <- fromJSON(link)
    count <- 1

    numero_mesas <- length(json$mesas)
    results_list <- list()

    for(i in 1:numero_mesas) {

        numero_cargos <- length(json$mesas[[i]])

        for(j in 1:numero_cargos) {

            mesa_votos <- json$mesas[[i]][[j]]$votos
            mesa_meta <- json$mesas[[i]][[j]]
            mesa_meta$votos <- NULL
            mesa_meta <- as.data.frame(mesa_meta)

            numero_partidos_cabezera <- length(mesa_votos$detalle)

            aliados_df <- lapply(1:numero_partidos_cabezera, function(x) mesa_votos$detalle[[x]]) %>%
                bind_rows()

            total_df <- mesa_votos[, 1:5]

            temp <- bind_rows(total_df, aliados_df) %>%
                add_count(nomina) %>%
                mutate(
                    keep = case_when(
                        partido == TRUE & n == 1 ~ 1,
                        partido == TRUE & n == 2 ~ 1,
                        partido == FALSE & n == 1 ~ 1,
                        TRUE ~ 0
                    ),
                    alianza = ifelse(partido == TRUE, 1, 0)
                ) %>%
                filter(keep == 1) %>%
                select(-c(keep, partido, orden, n)) %>%
                arrange(nomina)

            temp_cols <- names(temp)
            mesa_meta_cols <- names(mesa_meta)

            results_list[[count]] <- bind_cols(temp, mesa_meta) %>%
                select(all_of(mesa_meta_cols), all_of(temp_cols))

            count <- count + 1
        }
    }
    return(results_list)
}

json_list <- "https://data-resultados.te.gob.pa/presentacion/eventos/100/centros/1/0/1/3/index.json?sv=2023-01-03&ss=btqf&srt=sco&st=2024-03-23T14%3A48%3A10Z&se=2024-12-31T05%3A00%3A00Z&sp=rl&sig=qcdoSERIfwIvRYX3M0HyeZyOvbNjA4MMWVRI8MCsfOA%3D"

resultados_totales_df <- future_lapply(
        json_list,
        function(x) resultadosExtract(x)
    ) %>%
    bind_rows()

resultados_totales_df$time_scraped <- date_now("US/Eastern")

# Save data ###################################################################

export(resultados_totales_df, here("data/raw/resultados_totales.csv"))