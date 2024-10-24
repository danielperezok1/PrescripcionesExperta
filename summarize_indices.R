summarize_indices <- function(indices, number_cluster) {
  
  indices <- do.call("rbind", indices)
  
  IndN <- indices
  if (nrow(indices) > 1) {
    IndN <- apply(indices, 2, normalize)
    IndN <- apply(IndN, 1, function(xx) {
      sqrt(sum(xx ^ 2))
    })
  }
  
  indicesresults <- data.frame(number_cluster, indices, IndN)
  names(indicesresults) = c(
    "Num. Cluster",
    "Xie Beni",
    # "Fukuyama Sugeno",
    "Partition Coefficient",
    "Entropy of Partition",
    "Summary Index"
  )
  indicesresults
}