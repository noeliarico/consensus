#' Scorix
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
scorix <- function(profileOfRankings) {

  votes <- profileOfRankings$numberOfVoters
  candidates <- profileOfRankings$candidates
  por <- profileOfRankings$profileOfRankings

  s <- matrix(
  .C("scorix",
     profileOfRankings = as.integer(t(por)),
     votes = as.integer(votes),
     ncandidates = as.integer(ncol(por)),
     nrankings = as.integer(length(votes)),
     results = integer(ncol(por)^2)
  )$results, byrow = TRUE, nrow = ncol(por))
  
  colnames(s) <- paste0("p", 1:length(candidates))
  rownames(s) <- candidates
  
  arg <- as.character(sys.call())[2]
  profileOfRankings$scorix <- s
  assign(arg, profileOfRankings, envir = globalenv())
  
  return(s)
}