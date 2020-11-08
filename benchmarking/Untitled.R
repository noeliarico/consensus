pors <- vector(mode = "list", 10*10)
i <- 1
for(ncandidates in seq(10, 100, 10)) {
  for(nrankings in seq(10, 100, 10)) {
    pors[[i]] <- random_profile_of_rankings(ncandidates, nrankings)
    i <- i + 1
  }
}

reps <- 10

i <- 1
times_plurality <- vector(mode = "list", 10*10)
for(i in 1:length(pors)) {
  time <- 0
  for(i in 1:reps) {
    time <- time + system.time(plurality(pors[[i]]))['elapsed']
  }
  i <- i + 1
  times_plurality[[i]] <- time/reps
}

times_borda <- vector(mode = "list", 10*10)
