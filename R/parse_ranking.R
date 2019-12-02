#s <- "a > b ~ c > d > e"
#' Title
#'
#' @param string
#'
#' @return
#' @export
#'
parse_ranking <- function(string) {
  string <- str_replace_all(string, " ", "")
  candidates <- unlist(strsplit(string, ">|~"))
  candidates <- sort(candidates)
  ranking <- rep(0, length(candidates))
  names(ranking) <- candidates
  string <- strsplit(string, "")[[1]]

  pos <- 1
  for (elem in string) {
    if(elem == ">" || elem == "~") {
      if(elem == ">") {
        pos <- pos + 1
      }
    }
    else {
      ranking[which(names(ranking) == elem)] <- pos
    }

  }
  return(ranking(ranking))
}
