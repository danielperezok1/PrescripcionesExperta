normalize <- function(x) {
  if (length(x) > 1) {
    return(x / max(x))
  } else {x}
  
}