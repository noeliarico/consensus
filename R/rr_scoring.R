#' Scoring ranking rules
#'
#' @param profileOfRankings object of the class profile of rankings
#' @param method
#' \itemize{
##'  \item{"plurality"}{Stuff}
##'  \item{"borda"}{Stuff}
##'  \item{"t"}{Stuff}
##' }
#' @param t only necessary when the \code{method} choosen is \code{t}
#' @param seeTrace by default FALSE. Change to TRUE for seeing on the screen
#'                the workflow of the function
#'
#' @return the ranking generated after applying the ranking rule
#'
#' @examples
#' 
#' 
#' @family ranking rule
#' @export 
scoring <- function(profileOfRankings, method = NULL, t = 1, seeTrace = FALSE, seePoints = FALSE) {
  
  if(seeTrace) {
    cat('Executing a scoring ranking rule...\n')
  }
  
  attname <- deparse(substitute(profileOfRankings))
  
  candidates <- profileOfRankings$candidates
  votes <- profileOfRankings$numberOfVoters
  por <- profileOfRankings$profileOfRankings
  
  # Result vectors
  v <- vector(length = ncol(por))
  names(v) <- names(por)
  
  # For each ranking in the profile of rankings
  if(method != "borda") {
    for(i in 1:nrow(por)) {
      numVotersRow <- votes[i]
      ranking <- por[i,]
      
      p <- calculatePoints(ranking, method, t, seeTrace, seePoints)
      v <- v + (numVotersRow * p)
      
      if(seeTrace) {
        cat("-> The points for this ranking\n")
        print(p)
        cat("-> This ranking has ",numVotersRow," voters\n")
        cat("-> The current value of the total points is:")
        print(v)
      }
      
    }
    
  } else { # Borda count
    # Count the number of times that each candidate is ahead of the remaining ones
    v <- rowSums(votrix(profileOfRankings))
    # v <- sort(v, decreasing = TRUE) # sort v from more votes to less
    if(seePoints || seeTrace) {
      cat("Points rewarded by each candidate of the profile of rankings:\n")
      print(v)
      cat('Winning ranking:\n')
    }
    return(ranking(v, desc = TRUE))
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
  class(ranking) <- "ranking"
  return(ranking)
  
}


#' Plurality ranking rule
#'
#' @param profileOfRankings
#' @param seeTrace
#'
#' @return
#'
#' @examples
#' @export
plurality <- function(profileOfRankings, seeTrace = FALSE, seePoints = FALSE) {
  scoring(profileOfRankings, "plurality", seeTrace = seeTrace, seePoints = seePoints)
}

#' Veto (a.k.a. antiplurality) ranking rule
#'
#' @param profileOfRankings
#' @param seeTrace
#'
#' @return
#'
#' @family ranking rule
#' @examples
#' @export
veto <- function(profileOfRankings, seeTrace = FALSE, seePoints = FALSE) {
  scoring(profileOfRankings, "veto", seeTrace = seeTrace, seePoints = seePoints)
}

#' t-approval ranking rule
#'
#' @param profileOfRankings
#' @param seeTrace
#'
#' @return
#' @export
#'
#' @family ranking rule
#' 
#' @examples
#' @export
tapproval <- function(profileOfRankings, t = 2, seeTrace = FALSE, seePoints = FALSE) {
  scoring(profileOfRankings, "t", t, seeTrace = seeTrace, seePoints = seePoints)
}

#' Borda Count ranking rule
#' 
#' Apply Borda Count ranking rule in a profile of ranking
#'
#' @param profileOfRankings
#' @param seeTrace
#'
#' @return
#' @export
#'
#' @family ranking rule
#' 
#' @examples
#' @export
borda_count <- function(profileOfRankings, seeTrace = FALSE, seePoints = FALSE) {
  scoring(profileOfRankings, "borda", seeTrace = seeTrace, seePoints = seePoints)
}

#' Borda Winner
#'
#' Applies Borda Count in the profile of rankings given as first parameter and
#' then it takes the winner in the first position. 
#' 
#' @param profileOfRankings
#' @param seeTrace
#'
#' @return
#' @export
#' 
#' @family ranking rule
#' 
#' @examples
#' @export
borda_winner <- function(profileOfRankings, seeTrace = FALSE, seePoints = FALSE) {
  ranking <- scoring(profileOfRankings, "borda", seeTrace = seeTrace)
  names(ranking[which.min(ranking)])
}

#' @export
scoring_rule <- function(profileOfRankings, points) {
  s <- scorix(profileOfRankings)
  if(length(points) == ncol(s)) {
    s <- s * points
    s <- rowSums(s)
    return(ranking(s, desc = TRUE))
  }
  else {
    stop("you must give a punctuation for each position")
  }
  
}