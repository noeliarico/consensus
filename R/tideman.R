tideman <- function(profileOfRankings) {
  
  votrix <- votrix(profileOfRankings)
  
  # TODO arreglar esto para no ocupar memoria de más
  # En realidad si por ejemplo hay 3 rankings con 2, 3 y 4 votos
  # las combinaciones se 2, 3, 4, 5, 6, 7
  votes <- profileOfRankings$numberOfVoters
  # Cada posición de la lista contiene dos vectores, el vector from y el vector to
  
  couples <- vector(mode = "list", length = sum(votes))
  for(i in seq(nrow(votrix))) {
    for(j in seq(ncol(votrix))) {
      
      value <- votrix[i, j]
      if(is.null(couples[[value]])) {
        couples[[value]] <- matrix(1, 1)
      }
    }
  }
  
  
}