#' Copeland
#' 
#' Condorcet method that takes into account the number of victories, ties and 
#' defeats of each candidate in relation to the other candidates, in order to 
#' determine a winning ranking. A common setting considers the following 
#' scoring assessment:
#' - 1 point per victory
#' - 0.5 points per tie
#' - 0 points per defeat
#'
#' @param profileOfRankings 
#'
#' @return An object of the class `ranking` that represents the winning ranking
#' after of the profile of rankings given as parameter 
#' @export
#'
#' @examples
copeland <- function(profileOfRankings,
                     seePoints = FALSE,
                     seeTrace = FALSE) {
  
  if(!is.por(profileOfRankings)) {
    stop("The argument profileOfRankings must be a por object.")
  }
  
  if(seeTrace) {
    seePoints <- TRUE
  }
  
  votrix <- votrix(profileOfRankings)
  nhalf <- sum(profileOfRankings$numberOfVoters)/2
  
  points <- t(apply(votrix, 1, function(x) {
    dplyr::if_else(x > nhalf, 1, dplyr::if_else(x == nhalf, 0.5, 0))
  }))
  
  colnames(points) <- profileOfRankings$candidates
  rownames(points) <- profileOfRankings$candidates
  
  if(seeTrace) {
    cat("Points given for each value:\n")
    print(points)
  }
  
  points <- rowSums(points)
  
  if(seePoints) {
    cat("\nScore obtained by each candidate:\n")
    print(points)
    cat("\n")
  }
  
  return(ranking(points, desc = TRUE))
}