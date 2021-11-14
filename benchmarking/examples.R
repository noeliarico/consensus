n <- 5

c(1,
  2,
  3, # qué pasa cuando es =?
  4, # matrix 2x2
  5, # matrix 3x3
  6, 
)

## 11
# 1 condorcet
# 2 winner y loser sin ranking
# 3 loser
# 200 matrix 2x2

set.seed(1000)
r <- random_profile_of_rankings(n,11)
condorcet(r, seePoints = T)
condorcet_winner(r)
condorcet_loser(r)
votrix(r)
rowSums(votrix(r))

# Condorcet winner -------------------------------------------------------------


# Condorcet loser --------------------------------------------------------------
set.seed(1)
r <- random_profile_of_rankings(n,10)
condorcet(r)
condorcet_winner(r)
condorcet_loser(r)
votrix(r)

# Condorcet winner y loser -----------------------------------------------------
set.seed(2)
r <- random_profile_of_rankings(n,10)
condorcet(r)
condorcet_winner(r)
condorcet_loser(r)
votrix(r)


to_explore <- rep(TRUE, n)
solution <- rep(0, n)
all_solutions <- numeric()
om <- votrix(r)
azzini(0)


r <- random_profile_of_rankings(4, 10)
sum(rowSums(votrix(r)) > colSums(votrix(r)))
rowSums(votrix(r))
kemeny(r)

votrix(r)
condorcet(r)
condorcet_winner(r)
condorcet_loser(r)

# 1
#     C1 C2 C3 C4
# C1  0  5  4  6
# C2  5  0  6  6
# C3  6  4  0  4
# C4  4  4  6  0
# 3 
#     C1 C2 C3 C4
# C1  0  3  2  2
# C2  7  0  6  5
# C3  8  4  0  7
# C4  8  5  3  0
