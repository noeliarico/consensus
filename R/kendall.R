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
  .C("kendall",
     ranking = as.integer(ranking1),
     reference = as.integer(sapply(1:length(ranking2), function(x) which(ranking2 == x))),
     candidates = length(ranking1),
     result = integer(1)
  )$result
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
kendallpor <- function(profileOfRankings, verbose = FALSE) {
  
  candidates <- colnames(profileOfRankings)[-1]
  reversals <- gtools::permutations(n = length(candidates), 
                                             r = length(candidates), 
                                             v = 1:length(candidates))
  colnames(reversals) <- candidates
  
  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get votes
  votes <- splittedPOF$votes
  # Get rankings
  profileOfRankings <- splittedPOF$rankings
  
  # TODO metodo en por para sacar los rankings porque aquí la clase es por
  # y se visualiza mal
  print(ranking(as.numeric(profileOfRankings[1, ])))
  #apply(reversals, 1, function(x) kendall(x, ))
  
  invisible(0)
  
  # .C("kendall", 
  #    ranking = as.integer(ranking1),
  #    ranking = as.integer(ranking2),
  #    candidates = length(ranking1),
  #    result = integer(1)
  # )$result
}