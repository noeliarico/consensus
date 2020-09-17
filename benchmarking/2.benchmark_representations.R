library(consensus)
library(tidyverse)

i <- 1
# Number of rankings tested: 10, 20, 30, 40, ..., 100 rankings
for(nr in nrankings) {
  # Number of candidates tested: 5, 10, 15, 20, 25, 30, ..., 50
  for(nc in ncandidates) {
    total <- 0 # total time of this experiment is initially 0
    for(times in 1:reps) { # repeat the experiment 'reps' times
      por <- pors[[i]] 
      i <- i + 1
      # add the time of this iter to the total time of the experiment
      total <- total + system.time(beatpath(por))['elapsed']
    }
    # Add to the results tibble the time of this experiment: this is, the
    # total time divided by the number of reps in order to get the average time
    results <- results %>% add_row(nr = nr, 
                                   nc = nc, 
                                   time = total/reps,
                                   matrix = "beatpath1")
  }
}

# SCORIX ------------------------------------------------------------------

i <- 1
# Number of rankings tested: 10, 20, 30, 40, ..., 100 rankings
for(nr in nrankings) {
  # Number of candidates tested: 5, 10, 15, 20, 25, 30, ..., 50
  for(nc in ncandidates) {
    total <- 0 # total time of this experiment is initially 0
    for(times in 1:reps) { # repeat the experiment 'reps' times
      por <- pors[[i]] 
      i <- i + 1
      # add the time of this iter to the total time of the experiment
      total <- total + system.time(scorix(por))['elapsed']
    }
    # Add to the results tibble the time of this experiment: this is, the
    # total time divided by the number of reps in order to get the average time
    results <- results %>% add_row(nr = nr, 
                                   nc = nc, 
                                   time = total/reps,
                                   matrix = "scorix")
  }
}

# VOTRIX ------------------------------------------------------------------


i <- 1
# Number of rankings tested: 10, 20, 30, 40, ..., 100 rankings
for(nr in nrankings) {
  # Number of candidates tested: 5, 10, 15, 20, 25, 30, ..., 50
  for(nc in ncandidates) {
    total <- 0 # total time of this experiment is initially 0
    for(times in 1:reps) { # repeat the experiment 'reps' times
      por <- pors[[i]] 
      i <- i + 1
      # add the time of this iter to the total time of the experiment
      total <- total + system.time(votrix(por))['elapsed']
    }
    # Add to the results tibble the time of this experiment: this is, the
    # total time divided by the number of reps in order to get the average time
    results <- results %>% add_row(nr = nr, 
                                   nc = nc, 
                                   time = median(total),
                                   matrix = "votrix")
  }
}



# BEATPATH ----------------------------------------------------------------

i <- 1
# Number of rankings tested: 10, 20, 30, 40, ..., 100 rankings
for(nr in nrankings) {
  # Number of candidates tested: 5, 10, 15, 20, 25, 30, ..., 50
  for(nc in ncandidates) {
    total <- 0 # total time of this experiment is initially 0
    for(times in 1:reps) { # repeat the experiment 'reps' times
      por <- pors[[i]] 
      i <- i + 1
      # add the time of this iter to the total time of the experiment
      total <- total + system.time(beatpath(por))['elapsed']
    }
    # Add to the results tibble the time of this experiment: this is, the
    # total time divided by the number of reps in order to get the average time
    results <- results %>% add_row(nr = nr, 
                                   nc = nc, 
                                   time = total/reps,
                                   matrix = "beatpath2")
  }
}