###########################################/
# Workshop análisis de redes -----
# Laboratorio de ciencias sociales UDP
# Dudas: jose.conejeros1@mail.udp.cl
###########################################/


# 1. Settings ------

# Limpiamos el enviroment y desactivamos la notación científica 
rm(list = ls())
options(scipen = 999)

# Cargamos las librerías que utilizaremos en el análisis
settings_packages <- function(packages){
  # Cargamos las tablas de datos
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

settings_packages(
  packages=c("rio", "dplyr", "broom", "ggplot2", "jtools", 
             "ggraph", "tidygraph", "GGally", "texreg",
             "igraph", "network", "sna", "ergm", # FUNDAMENTALES
             "ggmcmc", "patchwork", "DT", "ggnetwork", "visNetwork")
)


# 2. Estructura de los datos de red ------