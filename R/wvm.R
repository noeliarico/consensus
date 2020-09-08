#' Weigted Voting Method (WVM)
#'
#' Ranking rule
#' 
#' 
#' This procedure is based on the proportion of victories/defeats of each couple 
#' of candidates. It candidate receives a score resulting of the aggregation of 
#' all the proportions of victories and ranks the candidates according to this score.
#' We consider the matrix \eqn{P} of proportions of victories/defeats where \eqn{P_{ij}} is 
#' the proportion of times that candidate \eqn{c_i} is preferred to candidate \eqn{c_j}{cj}, 
#' for any \eqn{i != j}{ij}, and \eqn{P_{ii} = 0.5}. The victories and the defeats are separated 
#' in two matrices \eqn{P^+}  and \eqn{P^−}
#' 
#' t must be remarked that this method comes from the field of group decision making [33].
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
wvm <- function(profileOfRankings, alpha = 0.5) {
  
  votrix <- votrix(profileOfRankings)
  ncandidates <- sum(profileOfRankings$numberOfVoters)
  
  p <- votrix/ncandidates
  
  if(alpha != 0.5) {
    
    half <- ncandidates/2
    
    victories <- p - 0.5
    victories <- ifelse(victories <= 0, 0, victories)
    
    defeats <- p - 0.5
    defeats <- ifelse(defeats >= 0, 0, defeats)
    
    print("Victories")
    print(fractions(victories))
    print("Defeats")
    print(fractions(defeats))
    
    p <- (alpha * victories) + ((1 - alpha) * defeats)
    diag(p) <- 0
    
  }
  else {
    diag(p) <- 0.5
  }
  
  return(fractions(p))
  
}