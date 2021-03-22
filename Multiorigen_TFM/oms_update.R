## Multiorigen Covid-19
## Raquel Colorado, Universidad Complutense de Madrid

# datos obtenidos de la página oficial de World Health Organization:
# https://covid19.who.int/WHO-COVID-19-global-data.csv

# cargamos librerías

#pkgs <- c('utils', 'dplyr', 'RMySQL', 'Hmisc', 'stringr')

#if(!require(pkgs))install.packages(pkgs, repos = 'http://cran.us.r-project.org')

library(utils)
library(dplyr)
#library(RMySQL)
library(Hmisc)
library(stringr)

# Función para actualizar datos input ecdc acorde al formato de la base de datos

update_oms = function(input_df,tag) {
  input_df = input_df %>%
    mutate(fecha = as.Date(Date_reported, format ="%d/%m/%Y"))%>%
    filter(WHO_region == "EURO")
  borrar_col <- c("Country_code", "WHO_region", "Date_reported")
  input_df = input_df[, !(names(input_df) %in% borrar_col)]  # Borrar columnas que no usaremos
  input_df = input_df %>% # Renombrar variables con los nombres establecidos y añadir columnas "bbdd" y "region"
    rename(pais = Country, casos = New_cases, fallecidos = New_deaths, casos_acumulados = Cumulative_cases, fallecidos_acumulados = Cumulative_deaths) %>%
    mutate(bbdd = "OMS", region = "Global")
  data.frame(input_df)
  
}



oms_cases <- as.data.frame(data.table::fread("https://covid19.who.int/WHO-COVID-19-global-data.csv"))
#ecdc_cases = subset(ecdc_cases, !is.na(lat))
#ecdc_cases[is.na(ecdc_cases)]=0
oms_cases = update_oms(oms_cases, "cases")
#if (total_cases!=sum(jrc_cases[nrow(jrc_cases),1:(ncol(jrc_cases)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

# datos de paises
paises = read.csv("input_data/paises_poblacion.csv", sep = ';')

# verificar paises jrc corresponden con los nombres de los datos de paises
#jrc_paises_list = names(jrc_cases)[grepl("_cases", names(jrc_cases))] %>% str_replace_all(., "_cases", "") 
#if (all(jrc_paises_list %in% paises$pais)==FALSE) {
#  stop(paste0("Error: mapping data lacking for the following countries: ",jhu_country_list[(jhu_country_list %in% countries$jhu_ID)==FALSE]))
#}


# save file
write.csv(oms_cases, "input_data/oms_data.csv", row.names=F)
#rm(list = ls())



