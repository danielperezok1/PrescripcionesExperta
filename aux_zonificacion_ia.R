
source("src/cmeans_vectorized.R")
source("src/fclustIndex.R")
source("src/invert_variable.R")
source("src/make_clasification.R")
source("src/rescale_variable.R")
source("src/summarize_clusters.R")
source("src/summarize_clusters_metrics.R")
source("src/summarize_indices.R")
source("src/normalize.R")

library(sf)
# library(adespatial)
# library(factoextra)
# library(spdep)
# 
# library(stars)
# library(tmap)
# library(ade4)
# library(readr)
# library(mapview)




zonificacion_ia <- function(datos = datos,
                            # 1 o 2
                            dim_IA = 2,
                            dmax = 20,
                            # 0, 1, 2
                            invert = 1,
                            exp_difuso = 1.5,
                            zmin = 2,
                            zmax = 6,
                            zplot = 2) {
  if (length(dim_IA) != 1 | !is.numeric(dim_IA)) {
    stop("dim_IA debe ser un único valor numérico")
  }
  
  ## Analisis de Componentes Principales (PCA)
  pca <-
    paar:::dudy_pca(
      # dudi.pca(
      st_drop_geometry(datos),
      center = T,
      scannf = F,
      nf = 2
    )
  
  
  biplot <- factoextra::fviz_pca_var(
    pca,
    col.var = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
  )
  
  
  ## Calculo IA
  datos <- cbind(datos, pca$li[seq_len(dim_IA)])
  CP <- pca$li[seq_len(dim_IA)]
  CP <- data.frame(sapply(
    CP,
    rescale_variable,
    min_new = 0,
    max_new = 100
  ))

  if (class(invert) == "numeric" &
      invert != 0) {
    CP[invert] <- sapply(CP[invert], invert_variable)
  }
  
  if (invert == "all") {
    CP <- data.frame(sapply(CP, invert_variable))
  }
  
  
  if (dim_IA == 1) {
    datos$IA = as.vector(CP[, 1])
    
  } else {
    datos$IA = (as.matrix(CP) %*% (pca$eig[seq_len(dim_IA)] / pca$rank))
  }
  
  ## Zonificación
  zonificacion <-
    make_clasification(
      sf::st_drop_geometry(datos[, names(CP)]),
      number_cluster = c(zmin:zmax),
      fuzzyness = exp_difuso,
      "euclidean"
    )
  
  datos <- cbind(datos, zonificacion$cluster)
  
  ## Graficos
  datos_raster <- stars::st_rasterize(datos, dx = dmax, dy = dmax)
  
  tmap::tmap_mode('view')
  mapaIA <-
    tmap::tm_basemap(
      c(
        Satelite = "Esri.WorldImagery",
        Politico = "Esri.WorldGrayCanvas",
        Topo = "Esri.WorldTopoMap"
      )
    ) +
    tmap::tm_shape(datos_raster) +
    tmap::tm_raster(
      col = "IA",
      title = "IA (%)",
      style = "quantile",
      palette = c("#ff1a02", "#fff000", "#00d600"),
      n = 8
    ) +
    tmap::tm_layout(legend.format = list(
      scientific = TRUE,
      format = "f",
      digits = 1
    ))
  # mapaIA
  
  mapaZona <-
    tmap::tm_basemap(
      c(
        Satelite = "Esri.WorldImagery",
        Politico = "Esri.WorldGrayCanvas",
        Topo = "Esri.WorldTopoMap"
      )
    ) +
    tmap::tm_shape(datos_raster) +
    tmap::tm_raster(
      col = paste0("Cluster_", zplot),
      title = "Zona",
      style = "cat",
      palette = "Dark2"
    ) +
    tmap::tm_layout(legend.format = list(
      scientific = TRUE,
      format = "f",
      digits = 0
    ))
  # mapaZona
  
  plotindices <-
    ggplot2::ggplot(zonificacion$indices ,
                    ggplot2::aes(x = `Num. Cluster`, y = `Summary Index`)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Numero de Zonas", y = "Índice Resumen")
  
  mapaZonas <- tmap::tmap_arrange(mapaIA,
                                  mapaZona,
                                  ncol = 2,
                                  sync = TRUE)
  list(biplot = biplot,
       indices = plotindices,
       mapasZonas = mapaZonas,
       datosCluster = datos)
}
# Guardar base de datos con IA y Zonas
# st_write(datos, "datos_IA_Zonas.gpkg")

