#' Instant runoff voting
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
irv <- function(profileOfRankings, break_ties = "random", seeTrace = FALSE) {
  
  rankings <- profileOfRankings$profileOfRankings
  
  candidates <- profileOfRankings$candidates
  ncandidates <- length(candidates)
  
  results <- rep(0, length(candidates))
  names(results) <- candidates
  
  nvoters <- sum(profileOfRankings$numberOfVoters)
  
  pos <- ncandidates
  
  for(i in 1:(ncandidates-1)) {
    if(seeTrace) {
      cat("------------------------------------\n")
      cat("Rankings:\n")
      print(rankings)
    }
    # Count the times that the candidate has been ranked at the first position
    times_first <- colSums((rankings == 1) * nvoters)
    if(seeTrace) {
      print(rankings == 1)
      print((rankings == 1) * nvoters)
    }
    # The worse candidate (the one that has been ranked in the best position the min number of times)
    worse <- which.min(times_first)
    results[worse] <- pos
    if(seeTrace) {
      print(results)
    }
    pos <- pos - 1
    rankings <- rankings[,-worse]
    if(i < (ncandidates-1)) {
      rankings <- t(apply(rankings, 1, rerank))
    } else {
      results[which.min(results)] <- 1
    }
  }
  return(ranking(results))
}

# TODO in case when two or more candidates are tied in last position removed them all

rerank <- function(v) {
  missing_pos <- which(!(1:length(v) %in% v))
  if(length(missing_pos) > 0) {
    return(ifelse(v > missing_pos, v - 1, v))
  }
  return(v)
}

