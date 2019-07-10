# random_ranking <- function(elements, size, names = TRUE) {
#   dif <- size - elements
#   stopifnot(dif >= 0)
#   r <- c(sample(1:elements, elements), sample(1:elements, dif, replace = TRUE))
#   r <- matrix(r, nrow = 1, byrow = TRUE)
#   colnames(r) <- LETTERS[1:size]
#   r <- profile_of_rankings(r)
#   return(r)
# }
#
# sink("tapproval_examples")
# e1 <- matrix(c(1, 1, 2, 2), nrow = 1, byrow = TRUE)
# colnames(e1) <- letters[1:4]
# e1 <- profile_of_rankings(e1)
# e1
# plurality(e1, seePoints = TRUE)
# two(e1, seePoints = TRUE)
# three(e1, seePoints = TRUE)
# five(e1, seePoints = TRUE)
# seven(e1, seePoints = TRUE)
#
# e2 <- matrix(c(1, 1, 2, 2, 2), nrow = 1, byrow = TRUE)
# colnames(e2) <- letters[1:5]
# e2 <- profile_of_rankings(e2)
# e2
# plurality(e2, seePoints = TRUE)
# two(e2, seePoints = TRUE)
# three(e2, seePoints = TRUE)
# five(e2, seePoints = TRUE)
# seven(e2, seePoints = TRUE)
#
# e3 <- matrix(c(1, 1, 1, 1, 2), nrow = 1, byrow = TRUE)
# colnames(e3) <- letters[1:5]
# e3 <- profile_of_rankings(e3)
# e3
# plurality(e3, seePoints = TRUE)
# two(e3, seePoints = TRUE)
# three(e3, seePoints = TRUE)
# five(e3, seePoints = TRUE)
# seven(e3, seePoints = TRUE)
#
# e4 <- matrix(c(1, 2, 3, 3, 4, 5), nrow = 1, byrow = TRUE)
# colnames(e4) <- letters[1:6]
# e4 <- profile_of_rankings(e4)
# e4
# plurality(e4, seePoints = TRUE)
# two(e4, seePoints = TRUE)
# three(e4, seePoints = TRUE)
# five(e4, seePoints = TRUE)
# seven(e4, seePoints = TRUE)
#
# e5 <- matrix(c(1, 2, 3, 3, 3, 4), nrow = 1, byrow = TRUE)
# colnames(e5) <- letters[1:6]
# e5 <- profile_of_rankings(e5)
# e5
# plurality(e5, seePoints = TRUE)
# two(e5, seePoints = TRUE)
# three(e5, seePoints = TRUE)
# five(e5, seePoints = TRUE)
# seven(e5, seePoints = TRUE)
#
# e6 <- matrix(c(1, 2, 3, 3, 3, 3), nrow = 1, byrow = TRUE)
# colnames(e6) <- letters[1:6]
# e6 <- profile_of_rankings(e6)
# e6
# plurality(e6, seePoints = TRUE)
# two(e6, seePoints = TRUE)
# three(e6, seePoints = TRUE)
# five(e6, seePoints = TRUE)
# seven(e6, seePoints = TRUE)
#
# e7 <- matrix(c(1, 1, 1, 2, 2, 3, 4, 5), nrow = 1, byrow = TRUE)
# colnames(e7) <- letters[1:8]
# e7 <- profile_of_rankings(e7)
# e7
# plurality(e7, seePoints = TRUE)
# two(e7, seePoints = TRUE)
# three(e7, seePoints = TRUE)
# five(e7, seePoints = TRUE)
# seven(e7, seePoints = TRUE)
#
# e8 <- matrix(c(1, 2, 2, 2, 2, 3), nrow = 1, byrow = TRUE)
# colnames(e8) <- letters[1:6]
# e8 <- profile_of_rankings(e8)
# e8
# plurality(e8, seePoints = TRUE)
# two(e8, seePoints = TRUE)
# three(e8, seePoints = TRUE)
# five(e8, seePoints = TRUE)
# seven(e8, seePoints = TRUE)
# sink()
