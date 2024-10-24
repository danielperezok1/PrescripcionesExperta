invert_variable <- function(x, min_value = NULL, max_value = NULL) {
  if (is.null(max_value)) {
    max_value <- max(x, na.rm = TRUE)
  }
  if (is.null(min_value)) {
    min_value <- min(x, na.rm = TRUE)
  }
  x_inv <- max_value + min_value - x
  # label
  lbl <- attr(x, which = "label", exact = TRUE)
  if (!is.null(lbl)) {
    attr(x_inv, which = "label") <- paste0(lbl, " (inverted)")
  }
  return(x_inv)
}