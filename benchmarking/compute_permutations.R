#' Title
#'
#' @param vector 
#'
#' @return
#' @export
#'
#' @examples
permutations <- function(vector, save = FALSE, as_por = FALSE, keepFile = FALSE) {
  #result <- integer(length(vector) * factorial(length(vector)))
  
  if(as_por) {
    save <- TRUE    
  }
  
  if(save) {
    path <- file.path(getwd(), paste0("permutations", length(vector), ".csv"))
    sink(file = path)
  }
  .C("permutation",
    arr = as.integer(vector),
    pstart = as.integer(0),
    pend = as.integer(length(vector)-1))
  if(save) {
    sink()
  }
  
  if(as_por & !keepFile) {
    file.remove(path)
  }
  invisible(0)
}