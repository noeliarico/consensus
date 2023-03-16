# test_that("Winner score > 3/2m", {
#   f <- function(por) {
#     v <- votrix(por)
#     b <- as.numeric(borda(por))
#     if(all(1:4 %in% b)) {
#       count <<- count + 1
#       alt1 <- rowSums(v)[b == 1] > (3 * sum(por$numberOfVoters) / 2)
#       alt2 <- rowSums(v)[b == 2] > (3 * sum(por$numberOfVoters) / 2)
#       # alt3 <- rowSums(v)[b == 3] < (3 * sum(por$numberOfVoters) / 2)
#       # alt4 <- rowSums(v)[b == 4] < (3 * sum(por$numberOfVoters) / 2)
#       # return(alt1 & alt2 & alt3 & alt4)
#       return(alt2)
#     }
#     else {
#       return(TRUE)
#     }
#   }
#   
#   
#   count <- 0
#   for(i in 20:30) {
#     set.seed(i)
#     r <- random_profile_of_rankings()
#     expect_true(f(r))
#     if(!f(r)) {
#       print(votrix(r))
#       print(as.numeric(borda(r)))
#     }
#   }
#   print(paste("count:", count))
#     
# })


  g <- function(por) {
    n <- length(por$candidates)
    m <- sum(por$numberOfVoters)
    b <- as.numeric(borda(por))
    boolean_matrix <- matrix(F, nrow = n, ncol = n)
    # from worst to best
    for(i in 4:1) {
      # get the alternative that is in position i
      alt <- which(b == i)
      # set the rows of the matrix to TRUE and the column to FALSE
      # in this way the alternative in the last position get overwritten by
      # the others
      boolean_matrix[alt, ] <- T
      boolean_matrix[, alt] <- F
    }
    return(boolean_matrix)
  }

test_that("Ganador > 3m", {
  count <- 0
  for(i in 1:1000) {
    set.seed(i)
    r <- random_profile_of_rankings()
    b <- as.numeric(borda(r))
    if(all(1:4 %in% b)) {
      v <- votrix(r)
      the_sum <- sum(v[g(r)])
      print(the_sum)
      expect_true(the_sum > 30)
    }
  }
})


# Crear perfil de rankings
# Calcular ranking de Borda
# Seguir si el ranking no tiene empates, si no, volver a empezar
# Almacenar las rowsums
# Almacenar las puntuaciones de Borda
# 

  
