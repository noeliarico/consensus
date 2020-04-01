#' Title
#'
#' @param profileOfRankings 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
kemeny <- function(profileOfRankings, details = TRUE, verbose = FALSE) {

  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get votes
  votes <- splittedPOF$votes
  # Get rankings
  rankings <- splittedPOF$rankings
  # Get candidates
  candidates <- names(rankings)
                      
  rev <- reversals(candidates)
  
  if(details) {
    distances <- t(sapply(1:nrow(rev), function(i) {
      sapply(1:nrow(profileOfRankings), function(j) {
        kendall(get_ranking(profileOfRankings, j), ranking(rev[i,]))
      })
    }))
    colnames(distances) <- paste0("r", 1:ncol(distances))
    distance <- t(apply(distances, 1, function(x) x * votes))
    distance <- rowSums(distance)
    out <- cbind(rev, distances, distance)
  }
  else {
    distances <- sapply(1:nrow(rev), function(i) {
      sapply(1:nrow(profileOfRankings), function(j) {
        kendall(get_ranking(profileOfRankings, j), ranking(rev[i,]))
      })
    })
    distances <- sapply(distances, function(x) x * votes)
    distances <- rowSums(distances)
    out <- cbind(rev, distances)
  }
  
  return(out)
  
}

