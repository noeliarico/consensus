distance_rankings <- function(r1, r2) {
  candidates <- names(r1)
  ncandidates <- length(r1)
  distance <- 0
  for(i in 1:(ncandidates-1)) {
    for(j in (i+1):ncandidates) {
      if(i != j) {
        if((r1[i] > r1[j] && r2[i] < r2[j]) || 
           (r1[i] < r1[j] && r2[i] > r2[j])) {
          toadd = 1
        } else if((r1[i] != r1[j] && r2[i] == r2[j]) || 
                  (r1[i] == r1[j] && r2[i] != r2[j])) {
          toadd = 0.5
        } 
        else {
          toadd = 0
        }
        distance = distance + toadd
        print(paste0("Distance between ", candidates[i], " (r1=", r1[i], ",r2=", 
               r2[i], ") and ", candidates[j], " (r1=", r1[j], ",r2=", r2[j], 
               ") is ", toadd, ". Total distance = ", distance))
      }
    }
  }
  return(distance)
}
