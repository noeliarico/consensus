#' Condorcet
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
condorcet <- function(profileOfRankings, seePoints = FALSE) {
  v <- votrix(profileOfRankings)
  half <- sum(profileOfRankings$numberOfVoters)/2
  # Check if the candidates are preferred by at least half of the votes
  # in relation to other candidates
  v <- apply(v, 1:2, function(x) x > half)
  v <- rowSums(v, na.rm = TRUE)
  
  if(seePoints) {
    print(v)
  }
  
  if(any(v == (length(v)-1))) {
    if(all(1:(length(v)-1) %in% v)) { # there is a Condorcet ranking
      out <- ranking(v, desc = TRUE)
    }
    else { # there is a Condorcet winner but not a Condorcet ranking
      out <- FALSE
    }
  }
  else { # there isn't a Condorcet winner neither a Condorcet ranking
    out <- FALSE
  }
  class(out) <- c("condorcet", "ranking")
  return(out)
}

#' Condorcet winner
#' 
#' A candidate is preferred by more than half of the voters to all the other 
#' candidates, then it is presumptively the best and then it is selected as
#' Condorcet winner
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
condorcet_winner <- function(profileOfRankings, weak = FALSE) {
  vo <- votrix(profileOfRankings)
  half <- sum(profileOfRankings$numberOfVoters)/2
  # Check if the candidates are preferred by at least half of the votes
  # in relation to other candidates
  if(!weak) {
    v <- apply(vo, 1:2, function(x) x > half)
  } else {
    v <- apply(vo, 1:2, function(x) x >= half)
  }
  v <- rowSums(v, na.rm = TRUE)
  if(any(v == (length(v)-1))) { # There is a Condorcet winner
    a <- v[v== (length(v)-1)]
    out <- which(v==(length(v)-1))
  }
  else { # There is not a Condorcet winner
    out <- FALSE
  }
  v <- apply(vo, 1:2, function(x) x == half)
  if(any(v)) class(out) <- c("condorcet", "winner", "weak")
  else class(out) <- c("condorcet", "winner")
  return(out)
}

#' Condorcet loser
#' 
#' A candidate is preferred by less or equal than half of the voters to all the other 
#' candidates, then it is presumptively the worst and then it is selected as
#' Condorcet loser
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
condorcet_loser <- function(profileOfRankings) {
  v <- votrix(profileOfRankings)
  half <- sum(profileOfRankings$numberOfVoters)/2
  # Check if the candidates are preferred by at least half of the votes
  # in relation to other candidates
  v <- apply(v, 1:2, function(x) x > half)
  v <- rowSums(v, na.rm = TRUE)
  if(any(v == 0)) { 
    # There is a Condorcet loser
    out <- which(v == 0)
    # If out has length greater than 1 then there are two or more candidates
    # that are not a Condorcet loser but they are preferred by all the other
    # candidates and tied among them
  }
  else { # There is not a Condorcet loser
    out <- FALSE
  }
  class(out) <- c("condorcet", "loser")
  return(out)
}


#' @method format condorcet
#' @export
format.condorcet <- function(x, ...) {
  
  c <- class(x)
  if(any("ranking" == c)) {
    if(any(x)) out <- paste("There is a Condorcet ranking:", format.ranking(x))
    else out <- "There is not a Condorcet ranking"
  }
  else if(any("winner" == c)) {
    if(length(x) == 1) {
      if(any("winner" == c)) {
        if(x) out <- paste("There is a weak Condorcet winner:", names(x))
        else out <- "There is not a Condorcet winner"
      }
      else {
        if(x) out <- paste("There is a Condorcet winner:", names(x))
        else out <- "There is not a Condorcet winner"
      }
    } else {
      out <- paste("There are weak Condorcet winners:", paste(names(x), collapse = ","))
    }
    
  }
  else if(any("loser" == c)) {
    if(length(x) == 1) {
      if(x) out <- paste("There is a Condorcet loser:", names(x))
      else out <- "There is not a Condorcet loser"
    }
    else {
      out <- paste0("There is not a Condorcet loser but the candidates [",
                   paste(names(x), collapse = ","), 
                   "] are preferred by all the other candidates and tied among them")
    }
  }
  
  return(out)
}

#' @method print condorcet
#' @export
print.condorcet <- function(x, ...) {
  m <- format.condorcet(x)
  cat(m, "\n")
  invisible(x)
}

default.condorcet <- function(ranking, ...) {
  stop("Error: method not defined for the class condorcet")
}
