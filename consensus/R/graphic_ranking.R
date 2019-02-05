#' @export
#'
#' @title Print ranking
#'
#' @description Human-friendly representation of a ranking
#'
#' @param ranking the ranking to print
#'
#' @keywords proper ranking, correct ranking, format, ranking, print ranking
#'
#' @seealso translate.ranking \code{\link[consensus]{translate.ranking}}
#'
#' @return a character vector representing the ranking
#'
#' @examples
#' ranking <- translate.ranking(c('a','b','c','d'), c('b','d','a','c'))
#' graphic.ranking(ranking)
#'
graphic_ranking <- function(ranking) {

  #if(!is.ranking(ranking)) {
   # stop('The parameter must be a ranking')
  #}

  # Ensure that the ranking is ordered
  ranking <- sort(ranking)

  names <- as.character(names(ranking))
  gr <- names[1]
  for(i in 1:(length(ranking)-1)) {
    thisElem <- ranking[i]
    nextElem <- ranking[i+1]
    #print(paste('Comparing: ',thisElem,'--',nextElem))
    if(thisElem<nextElem) {
      gr <- paste(gr, '>',names[i+1])
    }
    else { # this means the two rankings are equals
      gr <- paste(gr, '~',names[i+1])
    }
  }

  gr
}
