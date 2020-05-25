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
  
  if(any(names(ranking1) != names(ranking2))) {
    stop("The rankings must refer to the same candidates")
  }
  
  if(verbose) {
    print("Kendall distance between")
    print(ranking1)
    print(ranking2)
  }
  r2sort <- sort(ranking2) 
  r1sort <- ranking1[names(r2sort)]
  .C("kendall",
     ranking = as.integer(r1sort),
     reference = as.integer(sapply(1:length(r2sort), function(x) which(r2sort == x))),
     candidates = length(r1sort),
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

#' @export
all_reversals <- function(from = 4, up = 10) {
  
  parallel::mclapply(from:up, function(x) { 
    reversals(paste0("C", 1:x)) 
  })
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