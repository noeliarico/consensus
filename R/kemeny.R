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
kemeny <- function(profileOfRankings, 
                   parallel = FALSE, 
                   verbose = FALSE) {
  
  print("al entrar")
  # Get information of the profile of rankings
  votes <- profileOfRankings$numberOfVoters
  rankings <- profileOfRankings$profileOfRankings
  candidates <- profileOfRankings$candidates
  number_of_candidates <- length(candidates)
  
  # Load the correct permutations file
  file_name <- paste0("perm", number_of_candidates, ".csv")
  print("antes del read")
  rev <- read.csv(file.path("perms", file_name), header = FALSE)
  print("después del read")
  # Give the name of the candidates 
  colnames(rev) <- candidates
  # Create the correct profile of rankings
  rev <- profile_of_rankings(rev)
  
  print("antes del if")
  
  if(!parallel) {
    
    # Calculate the distance for each possible reversal
    distances <- t(sapply(1:nrow(rev$profileOfRankings), function(i) {
      # to each ranking in the profile of rankings
      sapply(1:nrow(rankings), function(j) {
        # print(get_ranking(profileOfRankings, j)) # debug
        # print(get_ranking(rev, i)) # debug
        kendall(get_ranking(profileOfRankings, j), get_ranking(rev, i))
      })
    }))
    
    # At this point, distances contains a matrix where each column 
    # represent a ranking of the profile of rankings. The matrix has
    # the same rows as reversals. For a ranking j in the original profile
    # of rankings and a ranking i in the reversals.. the value of the cell
    # in the row i and column j stores the distances from the ranking i to j
    
    # Identify the rankings in the profile of rankings
    colnames(distances) <- paste0("r", 1:ncol(distances))
    # Multiply by the number of voters that has each ranking in the por
    distance <- t(apply(distances, 1, function(x) x * votes))
    # Sum the distance from the profile of rankings 
    distance <- rowSums(distance)
    
    # Keep only the rankings
    rev <- rev$profileOfRankings
    # The output stores the reversals
    # the distance from each reversal to each ranking
    # and the total distance from the reversal to the profile of rankings
    out <- cbind(rev, distances, distance)
  }
  else {
    print("trying to get cores")
    ncores < parallel::detectCores()
    print(ncores)
    cl <- parallel::makeCluster(ncores)
    distances <- parallel::parSapply(cl, 1:nrow(rev$profileOfRankings), function(i) {
      sapply(1:nrow(rankings), function(j) {
        kendall(get_ranking(profileOfRankings, j), get_ranking(rev, i))
      })
    })
    parallel::stopCluster(cl)
    distance <- t(apply(distances, 1, function(x) x * votes))
    distance <- rowSums(distance)
    out <- cbind(rev, distance)
  }
  
  # The minimum value of the distance represents the closest
  # possible reversal
  min_distance <- min(out$distance)
  
  # Filter the rows of the output where the distance is equals
  # to the minimum possible distance
  winners <- out[distance == min_distance, 1:number_of_candidates]
  # There is only one winner ranking
  if(nrow(winners) == 1) {
    winner <- ranking(as.integer(winners), candidates)
  }
  # There are more than one rankings that achieve the minimum distance
  else {
    winner <- c("More than one winner ranking.")
  }
  
  # The output is a list that contains all the info
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
    cat("\nThe Kendall distances to all the possible permutations 
        of the ranking for those candidates are:\n")
    ncandidates <- ncol(x$profileOfRankings$profileOfRankings) - 1
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
