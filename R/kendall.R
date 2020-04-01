#' Title
#'
#' @param ranking1 
#' @param ranking2 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
kendall <- function(ranking1, ranking2, verbose = FALSE) {
  .C("kendall",
     ranking = as.integer(ranking1),
     reference = as.integer(sapply(1:length(ranking2), function(x) which(ranking2 == x))),
     candidates = length(ranking1),
     result = integer(1)
  )$result
}

#' Calculate all the possible rankings 
#'
#' @param candidates 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
reversals <- function(candidates, verbose = FALSE) {
  reversals <- gtools::permutations(n = length(candidates), 
                                    r = length(candidates), 
                                    v = 1:length(candidates))
  colnames(reversals) <- candidates
  return(reversals)
}

#' Kendall distance in a profile of rankings
#'
#' @param ranking1 
#' @param ranking2 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
kendall_ranking_to_por <- function(profileOfRankings, ranking, verbose = FALSE) {
  
  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get rankings
  rankings <- splittedPOF$rankings
  
  out <- sapply(1:nrow(profileOfRankings), function(x) kendall(get_ranking(profileOfRankings, x), ranking))
  out <- cbind(rankings, out)
  class(out) <- c("reversals", "data.frame")
  
  return(out)
  
  # .C("kendall", 
  #    ranking = as.integer(ranking1),
  #    ranking = as.integer(ranking2),
  #    candidates = length(ranking1),
  #    result = integer(1)
  # )$result
}

#' Title
#'
#' @param reversals 
#'
#' @return
#' @export
#'
#' @examples
format.reversals <- function(reversals) {

  # Split votes and rankings
  rankings <- reversals[,1:(ncol(reversals)-1)]
  # Get votes
  distance <- reversals[,ncol(reversals)]

  to <- apply(rankings, 1, format.ranking)
  to <- as.data.frame(to)

  gpor <- cbind(to, distance)
  invisible(gpor)

}

#' Title
#'
#' @param reversals 
#'
#' @return
#' @export
#'
#' @examples
print.reversals <- function(reversals) {
  print(format(reversals))
}