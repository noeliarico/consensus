#' Schulze
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
schulze <- function(profileOfRankings) {
  m <- beatpath(profileOfRankings)
  m <- sapply(1:ncol(m), function(x) {sapply(1:ncol(m), f, x)})
  values <- rowSums(m)
  return(ranking(values, desc = TRUE))
}

f <- function(x, y) return(x > y)