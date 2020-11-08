#' Condorcet
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
condorcet <- function(profileOfRankings) {
  v <- votrix(profileOfRankings)
  half <- sum(profileOfRankings$numberOfVoters)/2
  # Check if the candidates are preferred by at least half of the votes
  # in relation to other candidates
  v <- apply(v, 1:2, function(x) x > half)
  v <- rowSums(v, na.rm = TRUE)
  if(any(v == (length(v)-1))) {
    if(all(1:(length(v)-1) %in% v)) {
      cat("There Condorcet ranking is:\n")
      return(ranking(v, desc = TRUE))
    }
    else {
      cat("There is Condorcet winner but not a Condorcet ranking.\n")
      return(NULL)
    }
  }
  else {
    cat("There is not a Condorcet winner.\n")
    return(NULL)
  }
}

#' Condorcet winner
#' 
#' A candidate is preferred by more than half of the voters to all the other 
#' candidates, then it is presumptively the best and then it is selected as
#' Condorcet winner
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
condorcet_winner <- function(profileOfRankings) {
  v <- votrix(profileOfRankings)
  half <- sum(profileOfRankings$numberOfVoters)/2
  # Check if the candidates are preferred by at least half of the votes
  # in relation to other candidates
  v <- apply(v, 1:2, function(x) x > half)
  v <- rowSums(v, na.rm = TRUE)
  if(any(v == (length(v)-1))) {
    cat("There is a Condorcet winner:\n")
    return(v[v== (length(v)-1)])
  }
  else {
    cat("There is not a Condorcet winner.\n")
    return(NULL)
  }
}