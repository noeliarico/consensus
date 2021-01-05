#' Simpson
#'
#' Simpson ranks candidates according to their maximum defeat. This means that 
#' the best ranked candidates are the ones that are not clearly defeated by any 
#' other candidate. 
#' 
#' Using the votrix, for each candidate it is necessary to seek for the value 
#' of the row that represents the candidate which is the highest of those that 
#' are lower than half of the voters of the profile of rankings. Then, the 
#' winning ranking is obtained by ordering the candidates by increasingly value 
#' of the biggest defeat.
#' Simpson ranking rule is also a Condorcet method. As it ranks candidates 
#' according to their maximum defeat, the best ranked candidates are the ones
#' that are not clearly defeated by any other candidate. 
#' 
#' Nevertheless, relevant characteristic of this method is that, although it 
#' is a Condorcet method (if there is a Condorcet winner it is elected winner), 
#' a Condorcet loser might be also elected winner.
#' 
#' @param profileOfRankings 
#' 
#' @return
#' @export
#'
#' @examples
simpson <- function(profileOfRankings) {
  
  v <- votrix(profileOfRankings)
  diag(v) <- Inf
  # The biggest defeat is the max of the matrix containing only defeats
  s <- apply(v, 1, min)
  
  # Winning ranking sorting the scores from highest to lowest
  return(ranking(s, desc = TRUE))
}