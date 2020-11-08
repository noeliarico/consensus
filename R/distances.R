#' Kemeny distance between two rankings
#'
#' @param r1 
#' @param r2 
#'
#' @return
#' @export
#'
#' @examples
kemeny_distance <- function(r1, r2, seeTrace = FALSE) {
  # Names of the candidates
  candidates <- names(r1)
  # Number of candidates
  ncandidates <- length(r1)
  # Initially, the distance is 0
  distance <- 0
  # Compare every pair of candidates i,j
  for(i in 1:(ncandidates-1)) {
    for(j in (i+1):ncandidates) {
      # If we are here that means that i!=j
      
      # If they have different order in both rankings add 1 point
      if((r1[i] > r1[j] && r2[i] < r2[j]) || 
         (r1[i] < r1[j] && r2[i] > r2[j])) {
            toadd <- 1
      # If they are ordered in one ranking and tied in other add 0.5 points
      } else if((r1[i] != r1[j] && r2[i] == r2[j]) || 
                (r1[i] == r1[j] && r2[i] != r2[j])) {
          toadd <- 0.5
      } else {
        toadd <- 0
      }
      # Increment the total distance with the points of the pair i,j
      distance = distance + toadd
      if(seeTrace) {
        cat(paste0("Distance between ", candidates[i], " (r1=", r1[i], ",r2=", 
                   r2[i], ") and ", candidates[j], " (r1=", r1[j], ",r2=", r2[j], 
                   ") is ", toadd, ". Total distance = ", distance, "\n"))
        
      }
    }
  }
  return(distance)
}


#' Litvak distance between two rankings
#'
#' @param r1 
#' @param r2 
#'
#' @return
#' @export
#'
#' @examples
litvak_distance <- function(r1, r2, seeTrace = FALSE) {
  # Names of the candidates
  candidates <- names(r1)
  # Number of candidates
  ncandidates <- length(r1)
  # Initially, the distance is 0
  distance <- 0
  # Compare the position of every candidate in both rankings
  for(i in 1:ncandidates) {
    dif <- abs(r1[i] - r2[i])
    distance <- distance + dif
  }
  if(seeTrace) {
    cat(paste0("Candidate ", candidates[i], " (r1=", r1[i], ",r2=", 
               r2[i], ") adds the value ", dif, " to the total distnace. Total distance = ", distance, "\n"))
    
  }
  return(distance)
}