as_ranking <- function(v, decreasing = FALSE) {

  # print('ranking for')
  # print(v)

  # si el vector tiene nombres los utilizo
  if(!is.null(names(v))) {
    candidates <- names(v)
  }
  # si el vector no tiene nombres los creo
  else {
    candidates <- as.character(1:length(v))
  }

  names(v) <- candidates

  # Creo el vector que será el ranking
  ranking <- rep(0, length(v))
  names(ranking) <- candidates

  # Ordenar el vector de mayor a menor
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

  # print('the rankings is')
  # print(ranking)
  return(ranking)
}
