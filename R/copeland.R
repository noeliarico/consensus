#' Copeland
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
copeland <- function(profileOfRankings) {
  votrix <- votrix(profileOfRankings)
  nhalf <- 4 / 2
  
  points <- apply(votrix, 1, function(x) {
    dplyr::if_else(x > nhalf, 1, dplyr::if_else(x == nhalf, 0.5, 0))
  })
  rowSums(points)
}