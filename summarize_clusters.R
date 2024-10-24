summarize_clusters <- function(clasifications, number_cluster) {
  res_clas <-
    lapply(clasifications, '[[', "cluster")
  res_clas <- data.frame(res_clas)
  names(res_clas) <-
    paste("Cluster", "_",
          number_cluster,
          sep = "")
  res_clas
}