#' to be deleteD: as_ranking
#'
#' @param v
#' @param decreasing
#'
#' @return
#'
#' @examples
as_ranking <- function(v, decreasing = FALSE) {

  # print('ranking for')
  # print(v)

  # if the vector already has names, these names are used as the candidates
  if(!is.null(names(v))) {
    candidates <- names(v)
  }
  # otherwise, creates the names for the candidates
  else {
    candidates <- as.character(1:length(v))
    names(v) <- candidates
  }


  # Create the vector that will store the ranking
  ranking <- rep(0, length(v))
  names(ranking) <- candidates

  # Sort the vector, default higher to lower
  v <- sort(v, decreasing = decreasing)

  ordv <- rep(0, length(v))
  names(ordv) <- names(v)

  pos_ranking <- 1
  ordv[1] <- pos_ranking
  previous_elem <- v[1]
  for(index in 2:length(v)) {
    this_elem <- v[index]
    if(decreasing) {
      if(this_elem < previous_elem) {
        pos_ranking <- pos_ranking + 1
      }

    }
    else { # it can't be less cause they're ordered so this means it's equal
      if(this_elem > previous_elem) {
        pos_ranking <- pos_ranking + 1
      }
    }
    ordv[index] <- pos_ranking
    previous_elem <- v[index]
  }
  # Recorro el vector, sustituyo el mejor número por un 1, que es la posición
  # continúo y sustituyo el número real por la misma posición si es igual al
  # de la posición anterior y si no incremento y luego sustituyo

  # ahora tengo que hacer coincidir el vector del ranking con la posición
  # original basándome en los nombres de las columnas

  indexes <- match(names(ordv), candidates)

  i <- 1
  for(elem in indexes) {
    ranking[elem] <- ordv[i]
    i <- i + 1
  }

  class(ranking) <- c("ranking", "numeric")

  # print('the rankings is')
  # print(ranking)
  return(ranking)
}
