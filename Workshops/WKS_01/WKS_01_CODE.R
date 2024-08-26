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

# Vamos a crear una función en R que nos ayudará a procesar la data
net_preparation <- function(path, 
                            edges, 
                            nodes, 
                            directed, 
                            filtro_inicial_paises){
  # 1. Importamos las tablas de datos
  links <- rio::import(paste0(path, edges)) 
  
  nodos <- rio::import(paste0(path, nodes)) 
  
  # 2. Construímos la tabla en formato red 
  net <- igraph::graph_from_data_frame(
    d=links,
    vertices = nodos,
    directed = directed
  ) |> 
    igraph::simplify(
      remove.multiple = TRUE,
      remove.loops = TRUE
    ) 
  
  net <- induced_subgraph(net, vids = V(net)[countryName %in% filtro_inicial_paises])
  
  # Guardamos todos los resultados en una lista
  objetos_red <- list()
  objetos_red$enlaces <- links
  objetos_red$nodos <- nodos
  objetos_red$red <- net
  
  objetos_red
}

# Ahora vamos aplicar nuestra función
paises_lat <- c("Argentina", "Brazil", "Chile", "Costa rica", "Cuba", "Honduras",
                "Mexico", "Panama", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

red <- net_preparation(
  path = './Datos/',# en local : 'Datos/',
  edges = 'edges.csv',
  nodes = 'nodes.csv',
  directed = T,
  filtro_inicial_paises = paises_lat
)



# 2. Descripción de la estructura de una red ------




# 3. Formación de redes: modelos aleatorios ------




# 4. Modelos exponenciales de grafos aleatorios (ERGMs) --------



