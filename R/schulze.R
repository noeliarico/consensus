#' Schulze
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
schulze <- function(profileOfRankings) {
  v <- votrix(profileOfRankings)
  half <- sum(profileOfRankings$numberOfVoters)/2
  values <- rowSums((v > half) * v)
  return(ranking(values))
}