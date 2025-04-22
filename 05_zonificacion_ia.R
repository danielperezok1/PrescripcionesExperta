# Carga de funciones específicas
source("src/aux_zonificacion_ia.R")
  
library(sf)

 datos <-
   na.omit(read.table("C:/Users/daperez/Desktop/AGD TUCUMAN DAPEREZ/Rstudio AGD/Ambientaciones/Protocolos_Ambientacion_R/data/rincon.csv",
                      head=T, sep=","))
 
datos <- datos[, c(1:9)]
 names(datos)[1] <- "x"
 names(datos)[2] <- "y"
 
 
# ## Conversion datos espaciales
 datos <- st_as_sf(datos, coords = c("x", "y"), crs = 4326)
datos <- st_transform(datos, crs = 32720)
mapview::mapview(datos)

datos <- sf::st_read("13.shp")
# Saca columna 7 y 8
datos_ss <- datos[, c(5,6)]

zonificacion <- 
  zonificacion_ia(datos = datos_ss,
                  # 1 o 2
                  dim_IA = 2,
                  dmax = 20,
                  # 0, 1, 2
                  invert = 2,
                  exp_difuso = 1.5,
                  zmin = 2,
                  zmax = 6,
                  zplot = 3)

zonificacion$biplot

zonificacion$indices

zonificacion$mapasZonas

head(zonificacion$datosCluster)

st_write(zonificacion$datosCluster,
         dsn = 'data/rinco.shp')
