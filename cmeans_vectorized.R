cmeans_vectorized <-
  function(data,
           nclusters,
           ...,
           index = c(
             "xie.beni",
             # "fukuyama.sugeno",
             "partition.coefficient",
             "partition.entropy"
           )) {
    lapply(nclusters, function(center, x, index, ...) {
      myClusters <- e1071::cmeans(
        x = data,
        centers = center,
        method = "cmeans",
        iter.max = 100,
        ...
      )
      
      myIndices <- fclustIndex(myClusters,
                               x = data,
                               index = index)
      
      list("cluster" = myClusters,
           "indices" = myIndices)
    },
    x = data,
    index = index,
    ...)
    
    
  }