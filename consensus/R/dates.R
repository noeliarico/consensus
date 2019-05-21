library(lubridate)

test_time_ranking_creation <- function() {
  random_vector <- sample(seq(0,1,0.0001), 50)
  ranking(random_vector)
}

start_time <- Sys.time()
replicate(1000, test_time_ranking_creation())
end_time <- Sys.time()
cat('\nExecution time 100 rankings:', seconds((end_time-start_time))/1000)

create_random_por <- function() {
  random_matrix <- matrix(ranking(sample(seq(0,1,0.01), 50)), ncol = 50)
  for(i in 1:50) {
    random_vector <- sample(seq(0,1,0.01), 50)
    random_matrix <- rbind(random_matrix, ranking(random_vector))
  }
  return(random_matrix)
}
the_matrix <- as.data.frame(create_random_por())

borda_count(the_matrix)
