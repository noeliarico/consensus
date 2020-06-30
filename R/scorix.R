#' Scorix
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
scorix <- function(profileOfRankings) {
  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get votes
  votes <- splittedPOF$votes
  # Get rankings
  profileOfRankings <- as.matrix(splittedPOF$rankings)
  # Get the candidates
  candidates <- splittedPOF$candidates
  print(profileOfRankings)
  
  print(profileOfRankings)
  print(as.integer(t(profileOfRankings)))
  .C("scorix",
     profileOfRankings = as.integer(t(profileOfRankings)),
     votes = as.integer(votes),
     ncandidates = as.integer(ncol(profileOfRankings)),
     nrankings = as.integer(length(votes)),
     results = integer(length(as.integer(t(profileOfRankings))))
  )$results
}