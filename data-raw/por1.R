# Table 2.1 

library(consensus)
library(usethis)
por1 <- parse_ranking("6, c > b > a > d,
                       5, a > d > b > c,
                       3, b > a > d > c")
use_data(por1)
