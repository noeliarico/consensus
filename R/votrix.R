#' Votrix
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
votrix <- function(profileOfRankings) {
  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get votes
  votes <- splittedPOF$votes
  # Get rankings
  profileOfRankings <- as.matrix(splittedPOF$rankings)
  # Get the candidates
  candidates <- splittedPOF$candidates
  
  matrix(
  .C("votrix",
     profileOfRankings = as.integer(t(profileOfRankings)),
     votes = as.integer(votes),
     ncandidates = as.integer(ncol(profileOfRankings)),
     nrankings = as.integer(length(votes)),
     results = integer(ncol(profileOfRankings)^2)
  )$results, nrow = ncol(profileOfRankings))
}