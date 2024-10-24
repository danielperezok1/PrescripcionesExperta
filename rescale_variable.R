rescale_variable <- function(x, min_old, min_new = 0, max_old, max_new = 1) {
  if (missing(min_old)) {
    min_old <- min(x, na.rm = TRUE)
  }
  if (missing(max_old)) {
    max_old <- max(x, na.rm = TRUE)
  }
  x_rescaled <- (max_new-min_new)/(max_old-min_old)*(x-max_old)+max_new
  return(x_rescaled)
}