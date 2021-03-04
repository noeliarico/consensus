smith_set <- function(por) {
  v <- votrix(por)
  nhalf <- (v[1,2]+v[2,1])/2
  print(nhalf)
  n <- ncol(v)
  # Get the points for each candidate
  points <- t(apply(v, 1, function(x) {
    dplyr::if_else(x > nhalf, 1, dplyr::if_else(x == nhalf, 0.5, 0))
  }))
  colnames(points) <- por$candidates
  rownames(points) <- por$candidates
  # Get the copeland ranking
  print(points)
  p <- order(rowSums(points), decreasing = T)
  print(p)
  res <- matrix(rep(0,n*n),ncol=n)
  for(i in 1:n) {
    res[i,] <- points[p[i],p]
  }
  print(res)
}
# que hace en este caso?
por <- parse_profile_of_rankings("
3, C2 ≻ C3 ≻ C4 ≻ C1,
3, C3 ≻ C4 ≻ C1 ≻ C2,
1, C3 ≻ C2 ≻ C1 ≻ C4,
3, C1 ≻ C2 ≻ C3 ≻ C4")
# numberOfVoters                ranking
# 1              2 C5 ≻ C4 ≻ C3 ≻ C2 ≻ C1
# 2              2 C2 ≻ C4 ≻ C1 ≻ C5 ≻ C3
# 3              3 C1 ≻ C3 ≻ C5 ≻ C4 ≻ C2
# 4              3 C5 ≻ C2 ≻ C1 ≻ C3 ≻ C4
# en este sí que funciona:
# numberOfVoters                ranking
# 1              3 C3 ≻ C4 ≻ C2 ≻ C1 ≻ C5
# 2              3 C2 ≻ C3 ≻ C4 ≻ C1 ≻ C5
# 3              1 C1 ≻ C4 ≻ C2 ≻ C3 ≻ C5
# 4              3 C4 ≻ C5 ≻ C2 ≻ C1 ≻ C3
