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

net <- net_preparation(
  path = './Datos/',# en local : 'Datos/',
  edges = 'edges.csv',
  nodes = 'nodes.csv',
  directed = T,
  filtro_inicial_paises = paises_lat
)

# Veamos los objetos que acabos de generar
 # Tabla de datos con los enlaces 
net$nodos # Tabla de datos con los nodos
net$red # Datos en formato de red


# 2. Exploremos la red ------

## Nodos  -----
nodos <- net$nodos
nodos

## Enlaces  -----
links <- net$enlaces
links

## Objeto tipo red -----
net <- net$red
net 

## Matriz de adyacencia -----
as_adjacency_matrix(net)[c(1:20), c(1:20)]

## Grafiquemos la red -----
# Podemos plotear con ggplot2
plot <- ggplot(
  ggnetwork(net), 
  aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "gray") +
  geom_nodes(aes(color = gender), size = 3, alpha = 0.8) +
  #scale_colour_manual(values = palette("Alphabet")) +
  labs(title = "Red de figuras públicas para América Latina") +
  theme_blank() +
  theme(legend.position = "top",
        legend.title = element_blank())
plot

# Otra alternativa más dinámica
visNetwork::visIgraph(net) %>%
  visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

# Otra alternativa más dinámica
plot <- ggnet2(net,
               size = 1, 
               # size.min = cota,
               max_size = 3,  
               edge.size = 0.1, 
               edge.color = "grey",
               node.color = "color_nodo",
               node.alpha = 0.8,
               label = FALSE
               
) +
  ggtitle("Red de figuras públicas para América Latina") +
  theme(legend.position = "none") 

# Aquí viene lo interactivo
atributos <- vertex_attr(net, index = plot$data$label)
plot$layers[[2]]$mapping$Name        <- atributos$Name
plot$layers[[2]]$mapping$countryName <- atributos$countryName
plot$layers[[2]]$mapping$domain2 <- atributos$domain2

plotly::ggplotly(plot, tooltip = c("Name", "countryName", "domain2"))


# 4. Descriptivos de la red ------

## Grado ----
sort(igraph::degree(net))

## Densidad ----
edge_density(net)

## Clustering ----
round(transitivity(net, type="global"),3)

## Componentes ----
componentes <- clusters(net)
componentes

# Identificamos el componente gigante
g <- which.max(componentes$csize) 
subred <- induced_subgraph(net, which(componentes$membership == g)) # nos quedamos con el componente gigante

# Comparamos: 
V(net)
V(subred)

## Distancias ----
gsize(net)
gorder(net)
net.distancia <- round(mean_distance(net),3) 
net.distancia

## Medidas de centralidad ----

# Calcular grado de centralidad (degree centrality) 
centralidad_grado <- igraph::degree(net) 

# Calcular centralidad de cercanía 
centralidad_cercania <- igraph::closeness(net, normalized = T) 

# Calcular la centralidad de intermediación
centralidad_intermediacion <- igraph::betweenness(net, normalized = T)

# Calcular la centralidad de vector propio
centralidad_eigen <-igraph::eigen_centrality(net)

centralidades <- cbind(centralidad_grado,
                       round(centralidad_cercania,3),
                       round(centralidad_intermediacion,3),
                       round(centralidad_eigen$vector,3)) 
colnames(centralidades) <- c("grado", "cercania", "intermediacion", "eigen")

centralidades |> as_tibble() |> arrange(desc(grado))
centralidades |> as_tibble() |> arrange(desc(cercania))
centralidades |> as_tibble() |> arrange(desc(intermediacion))
centralidades |> as_tibble() |> arrange(desc(eigen))

# Nodos de mayor centralidad
top5_degree <- head(sort(centralidad_grado, decreasing = TRUE), 5)
top5_degree

top5_betweenness <- head(sort(centralidad_intermediacion, decreasing = TRUE), 5)
top5_betweenness

top5_closeness <- head(sort(centralidad_cercania, decreasing = TRUE), 5)
top5_closeness

top5_eigenvector <- head(sort(centralidad_eigen$vector, decreasing = TRUE), 5)  # 
top5_eigenvector

# 5. Formación de redes: modelos aleatorios ------

# Obtenemos la base de la red para simular
size <- length(V(net))
dens <- edge_density(net)

## Erdos Renyi -----
random_graph <- erdos.renyi.game(size, dens)
avg_distance <- round(mean_distance(random_graph), 3)
clustering <- round(transitivity(random_graph, type = "global"), 3)

g1 <- ggplot(
  ggnetwork(random_graph), 
  aes(x, y, xend = xend, yend = yend)
) +
  geom_edges(color = "gray") +
  geom_nodes(size = 3, alpha = 0.8, color = "black") +
  labs(title = "Modelo de red aleatoria",
       caption=paste("Distancia promedio:", avg_distance, "\n",
                     "Clustering:", clustering)) +
  theme_blank() +
  theme(legend.position = "none")
g1

## Strogatz-Watts (mundo pequeño)----
random_graph <- watts.strogatz.game(1, size, 3, 0.1)
avg_distance <- round(mean_distance(random_graph), 3)
clustering <- round(transitivity(random_graph, type = "global"), 3)

g2 <- ggplot(
  ggnetwork(random_graph), 
  aes(x, y, xend = xend, yend = yend)
) +
  geom_edges(color = "gray") +
  geom_nodes(size = 3, alpha = 0.8, color = "black") +
  labs(title = "Modelo de red aleatoria",
       caption=paste("Distancia promedio:", avg_distance, "\n",
                     "Clustering:", clustering)) +
  theme_blank() +
  theme(legend.position = "none")
g2

## Barabási (libre escala) ----
random_graph <- barabasi.game(size, power = 1, m = 2, directed = FALSE)
avg_distance <- round(mean_distance(random_graph), 3)
clustering <- round(transitivity(random_graph, type = "global"), 3)

g3 <- ggplot(
  ggnetwork(random_graph), 
  aes(x, y, xend = xend, yend = yend)
) +
  geom_edges(color = "gray") +
  geom_nodes(size = 3, alpha = 0.8, color = "black") +
  labs(title = "Modelo de red aleatoria",
       caption=paste("Distancia promedio:", avg_distance, "\n",
                     "Clustering:", clustering)) +
  theme_blank() +
  theme(legend.position = "none")
g3

## Comparemos -----

# Agregamos información al plot original
g0 <- plot +  labs(title = "Red Original",
                   caption=paste("Distancia promedio:", round(mean_distance(net), 3), "\n",
                                 "Clustering:", round(transitivity(net, type="global"),3)))

# Vemos todos los gráficos
g0 | g1 | g2 | g3

# 6. Modelos exponenciales de grafos aleatorios (ERGMs) --------

## Datos para el modelamiento ----
net_models <- net_preparation(
  path = './Datos/',# en local : 'Datos/',
  edges = 'edges.csv',
  nodes = 'nodes.csv',
  directed = TRUE,
  filtro_inicial_paises = paises_lat
)

net_models <- intergraph::asNetwork(net_models$red)

## ERGMS ----
mod1 <- ergm(net_models ~ edges, verbose = FALSE)
mod2 <- ergm(net_models ~ edges + mutual, verbose = FALSE)
mod3 <- ergm(net_models ~ edges + mutual + 
               nodecov("agespan") + 
               nodematch("gender") + 
               nodefactor("domain"), 
             verbose = FALSE)
modelos <- list(mod1, mod2, mod3)

texreg::screenreg(modelos)

## Visualizamos efectos ----
max <- ceiling(max(c(abs(coef(mod1)), abs(coef(mod2)), abs(coef(mod3)))))

jtools::plot_summs(mod1, mod2, mod3,
                   model.names = c("Modelo 1", "Modelo 2", "Modelo 3")) +
  scale_x_continuous(limits = c(-max-1, max+1)) +
  scale_y_discrete(labels = function(x) gsub(".*\\.", "", x)) +
  labs(x="Efectos", y=NULL,
       caption=paste0(
         "Modelo 1: ", "AIC=", round(AIC(mod1), 2), "; ", 
         "BIC=", round(BIC(mod1)[1],2), "\n",
         "Modelo 2: ", "AIC=", round(AIC(mod2), 2), "; ", 
         "BIC=", round(BIC(mod2)[1],2), "\n",
         "Modelo 2: ", "AIC=", round(AIC(mod3), 2), "; ", 
         "BIC=", round(BIC(mod3)[1],2)
       )) +
  theme_light() +
  theme(legend.position = "top",
        legend.title = element_blank())

## Evaluación de supuestos y bondad de ajuste----

# Evaluemos los supuestos del modelo completo
mcmc.diagnostics(mod3, which = c("plots"))

# Bonda de ajuste 
fit <- gof(mod3)
plot(fit)

# 7. Detección de comunidades --------

## Girvan-Newman ------
red <- net 
clustering <- cluster_edge_betweenness(net)
V(red)$comm <- factor(membership(clustering))

ggplot(
  ggnetwork(red), 
  aes(x, y, xend = xend, yend = yend, color = comm, label = Name)) +
  geom_edges(color = "gray") +
  geom_nodes(size = 10, alpha = 0.25) +
  geom_nodes(aes(color = comm), size = 3, alpha = 0.8) +
  theme_blank() +
  theme(legend.position = "right")

## Label Propagation ------

red <- net 
clustering <- cluster_label_prop(net)
V(red)$comm <- factor(membership(clustering))

ggplot(
  ggnetwork(red), 
  aes(x, y, xend = xend, yend = yend, color = comm, label = Name)) +
  geom_edges(color = "gray") +
  geom_nodes(size = 10, alpha = 0.25) +
  geom_nodes(aes(color = comm), size = 3, alpha = 0.8) +
  theme_blank() +
  theme(legend.position = "right")

## Louvain ------

red <- net 
clustering <- cluster_louvain(net)
V(red)$comm <- factor(membership(clustering))

ggplot(
  ggnetwork(red), 
  aes(x, y, xend = xend, yend = yend, color = comm, label = Name)) +
  geom_edges(color = "gray") +
  geom_nodes(size = 10, alpha = 0.25) +
  geom_nodes(aes(color = comm), size = 3, alpha = 0.8) +
  theme_blank() +
  theme(legend.position = "right")

## Fast Greedy ------

red <- net 
clustering <- cluster_fast_greedy(red)
V(red)$comm <- factor(membership(clustering))

ggplot(
  ggnetwork(red), 
  aes(x, y, xend = xend, yend = yend, color = comm, label = Name)) +
  geom_edges(color = "gray") +
  geom_nodes(size = 10, alpha = 0.25) +
  geom_nodes(aes(color = comm), size = 3, alpha = 0.8) +
  theme_blank() +
  theme(legend.position = "right")

## Infomap ------

red <- net 
clustering <- cluster_infomap(net)
V(red)$comm <- factor(membership(clustering))

ggplot(
  ggnetwork(red), 
  aes(x, y, xend = xend, yend = yend, color = comm, label = Name)) +
  geom_edges(color = "gray") +
  geom_nodes(size = 10, alpha = 0.25) +
  geom_nodes(aes(color = comm), size = 3, alpha = 0.8) +
  theme_blank() +
  theme(legend.position = "right")

## Walktrap ------

red <- net 
clustering <- cluster_walktrap(net)
V(red)$comm <- factor(membership(clustering))

ggplot(
  ggnetwork(red), 
  aes(x, y, xend = xend, yend = yend, color = comm, label = Name)) +
  geom_edges(color = "gray") +
  geom_nodes(size = 10, alpha = 0.25) +
  geom_nodes(aes(color = comm), size = 3, alpha = 0.8) +
  theme_blank() +
  theme(legend.position = "right")



