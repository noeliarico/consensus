ranking <- function(v, n = NULL, decreasing = FALSE) {

  # if it is a vector value, the method use those names as candidates
  # if the n param is not null but the
  if(!is.null(names(v))) {
    candidates <- names(v)
    # all the candidates must have a different name
    if(length(unique(candidates)) != length(candidates)) {
      warning("The name of each candidate must be unique")
    }
  }
  # if we get to this condition then that means that names(v) is null,
  # so if the n param is not null and it
  else if(!is.null(n)) {
    candidates <- n
    # all the candidates must have a different name
    if(length(unique(candidates)) != length(candidates)) {
      warning("The name of each candidate must be unique")
    }
  }
  # if the vector does not have names and the param n is null, then the method
  # gives a name to each candidate with the format C1, C2, C3...
  else {
    candidates <- 1:length(v)
    candidates <- paste("C", candidates, sep = "")
  }

  names(v) <- candidates

  # Create the vector that will store the final ranking
  ranking <- rep(0, length(v))
  names(ranking) <- candidates

  # Sort the vector with the given criteria
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

  class(ranking) <- "ranking"

  return(ranking)
}


# Generic methods for the class ranking
print.ranking <- function(ranking) {
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

  cat(gr, "\n")
  return(ranking)
}

default.ranking <- function(ranking) {

}
