#' Votrix
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
votrix <- function(profileOfRankings) {
  
  if (!is.null(profileOfRankings$votrix)) {
    return(profileOfRankings$votrix)
  }
  
  votes <- profileOfRankings$numberOfVoters
  candidates <- profileOfRankings$candidates
  por <- profileOfRankings$profileOfRankings
  
  #print(as.integer(t(por)))
  #print(integer(ncol(por)^2))
  
  v <- matrix(.C("votrix",
     profileOfRankings = as.integer(t(por)),
     votes = as.integer(votes),
     totalvotes = as.integer(sum(votes)),
     ncandidates = as.integer(length(candidates)),
     nrankings = as.integer(length(votes)),
     votrix = double(ncol(por)^2)
  )$votrix, nrow = length(candidates))

  colnames(v) <- candidates
  rownames(v) <- candidates
  
  arg <- as.character(sys.call())[2]
  profileOfRankings$votrix <- v
  assign(arg, profileOfRankings, envir = parent.frame())
  
  return(v)
}