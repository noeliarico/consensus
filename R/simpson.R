#' Simpson
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
simpson <- function(profileOfRankings) {
  
  v <- votrix(profileOfRankings)
  nhalf <- sum(profileOfRankings$numberOfVoters)/2
  
  # Matrix that contains only the defeats
  v <- (v < nhalf ) * v
  # The biggest defeat is the max of the matrix containing only defeats
  s <- apply(v, 1, max)
  
  # Winning ranking sorting the scores from highest to lowest
  return(ranking(s, desc = TRUE))
}