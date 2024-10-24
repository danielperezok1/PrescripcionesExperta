summarize_clusters_metrics <- function(clasifications, number_cluster) {
  res_iter <- lapply(clasifications, '[[', "iter")
  res_scdd <- lapply(clasifications, '[[', "withinerror")
  
  data.frame("Clusters" = number_cluster,
             "Iterations" = unlist(res_iter),
             "SSDW" = unlist(res_scdd))
}