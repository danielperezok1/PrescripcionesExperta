
source("src/aux_zonificacion_ia.R")
library(sf)

#Importar csv
datos <-
  na.omit(read.table("data/RYA.csv",
                     head=T, sep=","))
#Importar shp
datos <- sf::st_read("13.shp")
#-----------------------------------------------------------------------

datos <- datos[, c(1:10)]
names(datos)[1] <- "x"
names(datos)[2] <- "y"
datos <- st_as_sf(datos, coords = c("x", "y"), crs = 4326)
datos <- st_transform(datos, crs = 32720)
mapview::mapview(datos)
datos_ss <- datos[, c(5,6,8)]

zonificacion <- 
  zonificacion_ia(datos = datos_ss,
                  # 1 o 2
                  dim_IA = 2,
                  dmax = 20,
                  # 0, 1, 2
                  invert = 0,
                  exp_difuso = 1.5,
                  zmin = 2,
                  zmax = 6,
                  zplot = 3)

zonificacion$biplot

zonificacion$indices

zonificacion$mapasZonas

head(zonificacion$datosCluster)

st_write(zonificacion$datosCluster,
         dsn = 'data/rya.shp')
