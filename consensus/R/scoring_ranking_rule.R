#' @export
#'
#' @title Plurality (first-past-the-post) ranking rule
#'
#' @description Candidates are ranked according to the number of voters that
#' considered this candidate their preferred option. It leads to a ranking rule
#' where only the main choice of each voter is taken into account.
#'
#' @param profileOfRankings The profile of ranking where the ranking rule is applied
#'
#' @keywords ranking rule, plurality
#'
#' @return The graphic representation of the ranking obtained after applying
#' plurality to the profile of rankings received as parameter
#'
#' # Create a profile of rankings
#' profileOfRankings <- profile.of.rankings(candidates = c('a','b','c','d'))
#' profileOfRankings <- add.ranking(profileOfRankings, c('c','b','a','d'), votes = 6)
#' profileOfRankings <- add.ranking(profileOfRankings, c('a','d','b','c'), votes = 5)
#' profileOfRankings <- add.ranking(profileOfRankings, c('b','a','d','c'), votes = 3)
#'
#' # Calculate the ranking
#' plurality(profileOfRankings)
#'
#' @export
plurality <- function(profileOfRankings, verbose = FALSE) {

  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get the votes
  votes <- splittedPOF$votes
  # Get the rankings
  profileOfRankings <- splittedPOF$rankings
  # Get the candidates
  candidates <- splittedPOF$candidates

  # Empty vector to fill to calculate the final solution
  # the name of each element is the name of one of the candidates
  # in the order given in the profile of rankings
  # Initially, all values will be 0 but during the function this vector will
  # store the number of voters that consider each of the elements the best
  v <- vector(length = ncol(profileOfRankings))
  names(v) <- names(profileOfRankings)

  # For each row of the profile of rankings, this is, for each ranking
  # Take the elements (plural cause there could be ties) in the first position
  # and in the vector of results sum the number of voters of that ranking to
  # this/those element
  for(i in 1:nrow(profileOfRankings)) { # for each ranking of the profile
    numVotersRow <- votes[i] # get how many voters have voted that ranking
    # get the candidates that are in the first position of that ranking
    indexesBestCandidates <- which(profileOfRankings[i,] == 1)
    # increment the number of voters that voted that candidate in first position
    v[indexesBestCandidates] <- v[indexesBestCandidates] + (numVotersRow * 1/length(indexesBestCandidates))
  }

  # At this point, v is a vector with the names of the candidate and for
  # each candidate it stores the number of voters that consider that candidate
  # the best

  v <- sort(v, decreasing = TRUE) # sort v from more votes to less
  gr <- names(v)[1] # graphic ranking

  # vector that will store the final ranking
  ranking <- rep(0, length(candidates))
  names(ranking) <- candidates

  pos <- 1 # position in the ranking

  for(i in 1:(length(v)-1)) {

    # gr
    thisElem <- v[i]
    nextElem <- v[i+1]

    # ranking
    index_of_candidate <- which(candidates == names(v)[i])
    ranking[index_of_candidate] <- pos

    if(thisElem > nextElem) {
      gr <- paste(gr, '>',names(v)[i+1])
      pos <- pos + 1
    }
    else { # this means the two rankings are equals
      gr <- paste(gr, '~',names(v)[i+1])
    }
  }

  # the last position isn't evaluated in the loop
  ranking[which(candidates == names(v)[i+1])] <- pos

  if(verbose == TRUE) {
   
    
    cat("The points for each candidate are:\n")
    print(v)
    cat("The ranking obtained by plurality is:\n")
    cat(gr, "\n")
  }

  return(ranking)

}
