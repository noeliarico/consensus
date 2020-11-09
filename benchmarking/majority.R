#' Majority
#'
#' @param profileOfRankings 
#' @param method 
#' @param c1 
#' @param c2 
#'
#' @return
#' @export
#'
#' @examples
majority <- function(profileOfRankings, method = NULL, c1 = NULL, c2 = NULL) {
  votrix <- votrix()
  
  if(!is.null(c1) & !is.null(c2)) {
    if(length(c1) == 1 & length(c2) == 1) {
      candidates <- colnames(votrix)
      pos_c1 <-  which(c1 == candidates)
      pos_c2 <-  which(c2 == candidates)
      if(length(pos_c1) != 0 & length(pos_c2)) {
        return(votrix[por_c1, pos_c2])
      } else {
        stop("c1 and c2 must be a candidate name")
      }
    } else {
      stop("c1 and c2 must be a candidate name")
    }
  }
  else if(is.null(c1) & is.null(c2)) {
    
  }
  else {
    stop("Both candidates must contain value or none")
  }
}