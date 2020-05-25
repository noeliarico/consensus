#' Title
#'
#' @param vector 
#'
#' @return
#' @export
#'
#' @examples
permutations <- function(vector) {
  #result <- integer(length(vector) * factorial(length(vector)))
  .C("permutation",
     arr = as.integer(vector),
     pstart = as.integer(0),
     pend = as.integer(length(vector)-1))
  invisible(0)
}