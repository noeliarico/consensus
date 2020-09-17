library(consensus)
library(tidyverse)

# Tibble to store the execution time of the votrix for a concrete 
# number of rankings and number of candidates.
# nr is the number of rankings in the profile of rankings
# nc is the number of candidates
# time is the average time that takes to compute the votrix for a profile 
# of rankings with that number of candidates and number of rankings
results <- tribble(~nr, ~nc, ~time, ~matrix)

# Times that each experiment is going to be repeated in order to get the average
reps <- 5
nrankings <- c(seq(10,100,10), 100, 200, 300, 400, 500)
ncandidates <- c(seq(5,50,5), 100, 200, 300, 400, 500)
pors <- vector(mode = "list", length = length(nrankings) * length(ncandidates) * reps)

set.seed(123)
i <- 0
# Number of rankings tested: 10, 20, 30, 40, ..., 100 rankings
for(nr in nrankings) {
  # Number of candidates tested: 5, 10, 15, 20, 25, 30, ..., 50
  for(nc in ncandidates) {
    total <- 0 # total time of this experiment is initially 0
    for(times in 1:reps) { # repeat the experiment 'reps' times
      # create a random profile of rankings
      por <- random_profile_of_rankings(nr, nc) 
      i <- i + 1
      pors[[i]] <- por
    }
  }
}


