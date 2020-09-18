library(tidyverse)
library(consensus)

results_kemeny <- tribble(~nr, ~nc, ~time)

# Times that each experiment is going to be repeated in order to get the average
reps <- 1
nrankings <- c(10, 50, 100)
ncandidates <- 1:6
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
      total <- total + system.time(kemeny(por))['elapsed']
      i <- i + 1
      
      pors[[i]] <- por
    }
    results <- results %>% add_row(nr = nr, 
                                   nc = nc, 
                                   time = total/reps)
  }
}


