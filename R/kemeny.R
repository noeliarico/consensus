#' Kemeny ranking rule
#'
#' @param profileOfRankings an object of the class "por"
#' @param verbose 
#'
#' @return kemeny returns an object of class kemeny
#' @export
#'
#' @examples
#' por <- parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
#'                                   5, b ≻ c ≻ a ≻ d,
#'                                   3, c ≻ d ≻ a ≻ b")
#' kemeny(por)
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
    ncores < detectCores()
    cl <- makeCluster(ncores)
    distances <- parSapply(cl, 1:nrow(rev), function(i) {
      sapply(1:nrow(profileOfRankings), function(j) {
        kendall(get_ranking(profileOfRankings, j), ranking(rev[i,]))
      })
    })
    stopCluster(cl)
    distances <- sapply(distances, function(x) x * votes)
    distances <- rowSums(distances)
    out <- cbind(rev, distances)
  }
  
  return(out)
}

