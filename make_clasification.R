make_clasification <- function(data, number_cluster, fuzzyness, distance) {
  clasifications <- cmeans_vectorized(data,
                                      number_cluster,
                                      dist = distance,
                                      m = fuzzyness)
  indices <- lapply(clasifications, '[[', "indices")
  indices <- summarize_indices(indices, number_cluster)
  
  
  clasifications <- lapply(clasifications, '[[', "cluster")
  
  
  
  
  clasifResults <-
    summarize_clusters_metrics(clasifications, number_cluster)
  FinalCluster <-
    summarize_clusters(clasifications, number_cluster)
  
  list("summaryResults" = clasifResults,
       "indices" = indices,
       "cluster" = FinalCluster)
}