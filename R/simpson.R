#' Simpson
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
simpson <- function(profileOfRankings) {
  votrix <- votrix(profileOfRankings)
  ncandidates <- 4
  
  min_values <- rep(0, ncandidates)
  min_candidates <- rep(0, ncandidates)
  
  # Ojo porque esto puede que sea más de uno
  for(i in seq(nrow(votrix))) {
    row <- votrix[i,]
    min_values[i] <- min(row)
    min_candidates[i] <- which.min(row)
  }
  
  # TODO acabar
  
}