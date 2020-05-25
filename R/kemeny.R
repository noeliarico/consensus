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

  if(nrow(profileOfRankings) == 1) {
    out <- list(profileOfRankings = profileOfRankings,
                distances = NA, 
                winnerDistance = 0,
                winnerRanking = get_ranking(profileOfRankings, 1))
    class(out) <- c("kemeny", "list")
    return(out)
  }
  
  # out <- lapply(1:nrow(profileOfRankings), function (x) {
  #   name <- getHash(get_ranking(profileOfRankings,x))
  #   print(name)
  #   reg <- paste0(name, ".*")
  #   print(reg)
  #   objs <- ls(envir = hashKemeny3, pattern = reg)
  #   lapply(objs, function(y, sum) {
  #     sum <- sum + (hashKemeny3[[y]] * votes[x])
  #   }, sum = 0)
  # })
  
  
  # # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # # Get votes
  votes <- splittedPOF$votes
  # Get rankings
  rankings <- splittedPOF$rankings
  # Get candidates
  candidates <- names(rankings)
  number_of_candidates <- length(candidates)
  rev <- perm6
  #rev <- perm3
  colnames(rev) <- candidates
  rev <- as.por(rev)
  
  
  
  #if(details) {
    distances <- t(sapply(1:nrow(rev), function(i) {
      sapply(1:nrow(profileOfRankings), function(j) {
        #print(get_ranking(profileOfRankings, j))
        #print(get_ranking(rev, i))
        kendall(get_ranking(profileOfRankings, j), get_ranking(rev, i))
      })
    }))
    
    #
    print(distances)
    
    print(distances)
    colnames(distances) <- paste0("r", 1:ncol(distances))
    distance <- t(apply(distances, 1, function(x) x * votes))
    distance <- rowSums(distance)
    
    
    rev$numberOfVoters <- NULL
    out <- cbind(rev, distances, distance)
  # }
  # else {
  #   ncores < detectCores()
  #   cl <- makeCluster(ncores)
  #   distances <- parSapply(cl, 1:nrow(rev), function(i) {
  #     sapply(1:nrow(profileOfRankings), function(j) {
  #       kendall(get_ranking(profileOfRankings, j), get_ranking(rev, i))
  #     })
  #   })
  #   stopCluster(cl)
  #   distance <- t(apply(distances, 1, function(x) x * votes))
  #   distance <- rowSums(distance)
  #   out <- cbind(rev, distance)
  # }

  print(out)
  
  min_distance <- min(out$distance)
  
  winners <- out[distance == min_distance, 1:number_of_candidates]
  if(nrow(winners) == 1) {
    winner <- ranking(as.integer(winners), candidates)
  }
  else {
    winner <- c("More than one winner ranking.")
  }
  
  
  out <- list(profileOfRankings = profileOfRankings,
              distances = out, 
              winnerDistance = min_distance,
              winnerRanking = winner)
  class(out) <- c("kemeny", "list")
  return(out)
}

#' @export
print.kemeny <- function(x) {

  cat("\nGiven the profile of rankings:\n")
  print(x$profileOfRankings)
  

  if(!is.na(nrow(x$distances))) {
    cat("\nThe Kendall distance to all the possible permutation of the ranking are:\n")
    ncandidates <- ncol(x$profileOfRankings) - 1
    gr <- apply(x$distances[, 1:ncandidates], 1, format.ranking)
    distances <- x$distances[, (ncandidates+1):ncol(x$distances)]
    gr <- as.data.frame(gr)
    colnames(gr) <- c('ranking')
    gr <- cbind(gr, distances)
    print(gr)
  }
  else {
    cat("\nKendall distances not computed: profile with one ranking.\n")
    
  }
  
  cat(paste("\nWinner ranking applying Kemeny with a distance of", x$winnerDistance, "is:\n"))
  print(x$winnerRanking)
  
  invisible(x$winnerRanking)
}
