#' Votrix
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
votrix <- function(profileOfRankings) {
  
  votes <- profileOfRankings$numberOfVoters
  candidates <- profileOfRankings$candidates
  por <- profileOfRankings$profileOfRankings
  
  print(as.integer(t(por)))
  print(integer(ncol(por)^2))
  
  v <- matrix(.C("votrix",
     profileOfRankings = as.integer(t(por)),
     votes = as.integer(votes),
     totalvotes = as.integer(sum(votes)),
     ncandidates = as.integer(length(candidates)),
     nrankings = as.integer(length(votes)),
     results = integer(ncol(por)^2)
  )$results, nrow = length(candidates))

  colnames(v) <- candidates
  rownames(v) <- candidates
  
  profileOfRankings$votrix <- v
  
  return(profileOfRankings)
}