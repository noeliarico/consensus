library(tictoc)
library(tidyverse)

# Tibble to store the execution time of the votrix for a concrete 
# number of rankings and number of candidates.
# nr is the number of rankings in the profile of rankings
# nc is the number of candidates
# time is the average time that takes to compute the votrix for a profile 
# of rankings with that number of candidates and number of rankings
results <- tribble(~nr, ~nc, ~time)

# Times that each experiment is going to be repeated in order to get the average
reps <- 10

# Number of rankings tested: 10, 20, 30, 40, ..., 100 rankings
for(nr in seq(10,100,10)) {
  # Number of candidates tested: 5, 10, 15, 20, 25, 30, ..., 50
  for(nc in seq(5,50,5)) {
    total <- 0 # total time of this experiment is initially 0
    for(times in 1:reps) { # repeat the experiment 'reps' times
      # create a random profile of rankings
      por <- random_profile_of_rankings(nr, nc) 
      # add the time of this iter to the total time of the experiment
      total <- total + system.time(votrix(por))['elapsed']
    }
    # Add to the results tibble the time of this experiment: this is, the
    # total time divided by the number of reps in order to get the average time
    results <- results %>% add_row(nr = nr, nc = nc, time = total/reps)
  }
}

ggplot(results, aes(nc, time, col = as.factor(nr))) +
  geom_line() +
  geom_point()

  