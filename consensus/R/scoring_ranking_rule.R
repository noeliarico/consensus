#' Scoring ranking rules
#'
#' @param profileOfRankings object of the class profile of rankings
#' @param method
#' \itemize{
##'  \item{"plurality"}{Stuff}
##'  \item{"borda"}{Stuff}
##'  \item{"ones"}{Stuff}
##' }
#' @param ones only necessary when the \code{method} choosen is
#' @param verbose by default FALSE. Change to TRUE for seeing on the screen
#'                the workflow of the function
#'
#' @return the ranking generated after applying the ranking rule
#'
#' @examples
#'
#'
scoring <- function(profileOfRankings, method = NULL, ones = 1, verbose = FALSE) {

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

    p <- calculatePoints(ranking, method, ones)
    v <- v + (numVotersRow * p)

    if(verbose) {
      cat("-> The points for this ranking\n")
      print(p)
      cat("-> This ranking has ",numVotersRow," voters\n")
      cat("-> The current value of the total points is:")
      print(v)
    }

  }

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

calculatePoints <- function(ranking, method = NULL, ones = 0, verbose = F) {

  switch(method,

         plurality = {
           points <- as.integer(ranking == 1)
           return(points)
         },

         veto = {
           last_pos <- max(ranking)
           points <- as.integer(ranking == last_pos)
           return(points)
         },

         ones = {
           points <- as.integer(ranking %in% 1:ones)
           print(ones)
           print(points)
           return(points)
         },

         borda = {

           # number of different positions in the rankings
           n_of_candidates <- length(ranking)
           max_pos <- max(ranking)

           if(n_of_candidates == max_pos) {
             # the same result would be achieve with the following code
             # but here the operation is vectorized so execution time decreases
             if(verbose)
               cat("No ties for this ranking\n")
             return(n_of_candidates - ranking)
           }

           # vector that will store the puntuation for each category
           points_by_pos <- rep(-1, max_pos)

           # for each category...
           for(i in 1:max_pos) {
             max_interval <- n_of_candidates - 1 - sum(ranking < i)
             min_interval <- max_interval - (sum(ranking == i) - 1)
             points_by_pos[i] <- (max_interval+min_interval)/2
           }

           points_for_each_candidate <- ranking
           points_for_each_candidate <- sapply(ranking,
                                               function(x) {points_by_pos[x]})

           if(verbose) {
             cat("The points for the ranking...\n")
             print(ranking)
             cat("...are:\n")
             print(points_for_each_candidate)
           }
         },

         {
           stop(paste(method, "is not a valid scoring ranking rule"))
         }

  )

  return(points_for_each_candidate)
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
plurality <- function(profileOfRankings, verbose = FALSE) {
  scoring(profileOfRankings, "plurality")
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
veto <- function(profileOfRankings, verbose = FALSE) {
  scoring(profileOfRankings, "veto")
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
two <- function(profileOfRankings, verbose = FALSE) {
  scoring(profileOfRankings, "ones", ones = 2)
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
three <- function(profileOfRankings, verbose = FALSE) {
  scoring(profileOfRankings, "ones", ones = 2)
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
borda_count <- function(profileOfRankings, verbose = FALSE) {
  scoring(profileOfRankings, "borda")
}
