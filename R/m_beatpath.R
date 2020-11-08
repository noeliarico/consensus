#' Beatpath
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
beatpath <- function(profileOfRankings) {
  
  votes <- profileOfRankings$numberOfVoters
  candidates <- profileOfRankings$candidates
  por <- profileOfRankings$profileOfRankings
  v <- votrix(profileOfRankings)
  
  b <- matrix(.C("beatpath",
                 votrix = as.integer(t(v)),
                 ncandidates = as.integer(length(candidates))
  )$votrix, nrow = length(candidates), byrow = TRUE)
  
  colnames(b) <- candidates
  rownames(b) <- candidates
  
  # Update the object in the environment to store the beatpath
  arg <- as.character(sys.call())[2]
  profileOfRankings$beatpath <- b
  profileOfRankings$votrix <- v
  assign(arg, profileOfRankings, envir = globalenv())
  
  return(b)
}