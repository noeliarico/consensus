#' Scoring ranking rules
#'
#' @param profileOfRankings object of the class profile of rankings
#' @param method
#' \itemize{
##'  \item{"plurality"}{Stuff}
##'  \item{"borda"}{Stuff}
##'  \item{"t"}{Stuff}
##' }
#' @param t only necessary when the \code{method} choosen is
#' @param verbose by default FALSE. Change to TRUE for seeing on the screen
#'                the workflow of the function
#'
#' @return the ranking generated after applying the ranking rule
#'
#' @examples
#' @export
#'
scoring <- function(profileOfRankings, method = NULL, t = 1, verbose = FALSE, seePoints = FALSE) {
  
  if(verbose) {
    cat('Executing a scoring ranking rule...\n')
  }
  
  attname <- deparse(substitute(profileOfRankings))
  
  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get votes
  votes <- splittedPOF$votes
  # Get rankings
  profileOfRankings <- splittedPOF$rankings
  # Get the candidates
  candidates <- splittedPOF$candidates
  
  # Result vectors
  v <- vector(length = ncol(profileOfRankings))
  names(v) <- names(profileOfRankings)
  
  # For each ranking in the profile of rankings
  for(i in 1:nrow(profileOfRankings)) {
    numVotersRow <- votes[i]
    ranking <- profileOfRankings[i,]
    
    p <- calculatePoints(ranking, method, t, verbose, seePoints)
    v <- v + (numVotersRow * p)
    
    if(verbose) {
      cat("-> The points for this ranking\n")
      print(p)
      cat("-> This ranking has ",numVotersRow," voters\n")
      cat("-> The current value of the total points is:")
      print(v)
    }
    
  }
  
  #if(seePoints) {
  
  #}
  
  v <- sort(v, decreasing = TRUE) # sort v from more votes to less
  if(verbose) {
    print(paste('Points rewarded by each candidate of the profile of rankings', "'", attname, "'"))
    print(v)
    print('Ranking:')
  }
  
  # vector that will store the final ranking
  ranking <- rep(0, length(candidates))
  names(ranking) <- candidates
  
  pos <- 1 # position in the ranking
  
  for(i in 1:(length(v)-1)) {
    
    thisElem <- v[i]
    nextElem <- v[i+1]
    
    # ranking
    index_of_candidate <- which(candidates == names(v)[i])
    ranking[index_of_candidate] <- pos
    
    if(thisElem > nextElem) {
      pos <- pos + 1
    }
    # else, nothing -> this means the two rankings are equals
    # so it's not necessary increment the position cause it will be tied
    # with the previous element
    
  }
  
  ranking[which(candidates == names(v)[i+1])] <- pos
  
  return(ranking(ranking))
  
}


#' Title
#'
#' @param profileOfRankings
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
plurality <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
  scoring(profileOfRankings, "plurality", verbose = verbose, seePoints = seePoints)
}

#' Title
#'
#' @param profileOfRankings
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
veto <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
  scoring(profileOfRankings, "veto", verbose = verbose, seePoints = seePoints)
}

#' Title
#'
#' @param profileOfRankings
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
tapproval <- function(profileOfRankings, t = 2, verbose = FALSE, seePoints = FALSE) {
  scoring(profileOfRankings, "t", t, verbose = verbose, seePoints = seePoints)
}

#' Title
#'
#' @param profileOfRankings
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
borda_count <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
  scoring(profileOfRankings, "borda", verbose = verbose)
}

#' Borda Winner
#'
#' Applies Borda Count in the profile of rankings given as first parameter and
#' then it takes the winner in the first position. 
#' 
#' @param profileOfRankings
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
borda_winner <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
  ranking <- scoring(profileOfRankings, "borda", verbose = verbose)
  names(ranking[which.max(ranking)])
}