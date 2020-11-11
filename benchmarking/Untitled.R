r <- random_profile_of_rankings(5)
scorix(r)
kemeny(r)

r <- parse_profile_of_rankings(c("1, a > b > c,
                      1, b > a > c,
                      1, b > c > a"))

p <- parse_profile_of_rankings(
c("
2, C1 ≻ C3 ≻ C4 ≻ C2,
1, C4 ≻ C1 ≻ C2 ≻ C3,
1, C1 ≻ C2 ≻ C4 ≻ C3,
2, C2 ≻ C3 ≻ C4 ≻ C1,
1, C4 ≻ C3 ≻ C1 ≻ C2,
1, C2 ≻ C1 ≻ C4 ≻ C3,
1, C1 ≻ C3 ≻ C2 ≻ C4,
1, C3 ≻ C2 ≻ C1 ≻ C4"))

tideman(p)
