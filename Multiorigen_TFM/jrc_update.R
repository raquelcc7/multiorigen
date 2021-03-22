## Multiorigen Covid-19
## Raquel Colorado, Universidad Complutense de Madrid

# datos obtenidos de Joint Research Centre data del siguiente repositorio de Github:
# https://raw.githubusercontent.com/ec-jrc/COVID-19

# cargamos librerías

#pkgs <- c('utils', 'dplyr', 'RMySQL', 'Hmisc', 'stringr')

#if(!require(pkgs))install.packages(pkgs, repos = 'http://cran.us.r-project.org')

library(utils)
library(dplyr)
#library(RMySQL)
library(Hmisc)
library(stringr)

# Función para actualizar datos input jrc acorde al formato de la base de datos

update_jrc = function(input_df, tag) {
  input_df =  input_df %>% 
    group_by(iso3) %>% 
    mutate(casos  = abs(CumulativePositive - lag(CumulativePositive))) # Calcular variable casos absolutos
  input_df = input_df %>% 
    group_by(iso3) %>% 
    mutate(fallecidos  = abs(CumulativeDeceased- lag(CumulativeDeceased)))
  borrar_col <- c("CumulativeRecovered", "CurrentlyPositive", "Hospitalized", "IntensiveCare", "EUcountry",
                  "EUCPMcountry", "NUTS")
  input_df = input_df[, !(names(input_df) %in% borrar_col)]  # Borrar columnas que no usaremos
  input_df = input_df %>% # Renombrar variables con los nombres establecidos y añadir columnas "bbdd" y "region"
    rename(fecha = Date, pais = CountryName, casos_acumulados = CumulativePositive, fallecidos_acumulados = CumulativeDeceased) %>%
    mutate(bbdd = "JRC", region = "Global")
    data.frame(input_df)
}



jrc_cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-country/jrc-covid-19-all-days-by-country.csv"))
jrc_cases[is.na(jrc_cases)]=0
jrc_cases = update_jrc(jrc_cases, "cases")
#if (total_cases!=sum(jrc_cases[nrow(jrc_cases),1:(ncol(jrc_cases)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

# datos de paises
paises = read.csv("input_data/paises_poblacion.csv", sep = ';')

# verificar paises jrc corresponden con los nombres de los datos de paises
jrc_paises_list = names(jrc_cases)[grepl("_cases", names(jrc_cases))] %>% str_replace_all(., "_cases", "") 
if (all(jrc_paises_list %in% paises$pais)==FALSE) {
  stop(paste0("Error: mapping data lacking for the following countries: ",jhu_country_list[(jhu_country_list %in% countries$jhu_ID)==FALSE]))
}


# save file
write.csv(jrc_cases, "input_data/jrc_data.csv", row.names=F)
#rm(list = ls())





