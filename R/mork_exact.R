# Define matrix
# Recursive method
# If dimensions of the matrix > 2x2 --> stop
# else:
# check the next elements to evaluate rowSums > colSums
# add to the queue
# call recursively the first element in the queue
# results is a list of the rankings

pw <- votrix(por04)
ranking <- rep(0, 4)
names(ranking) <- colnames(pw)
to_explore_pos <- 0
candidates_to_explore <- 0
mork_exact <- function(pw, ranking, level = 1) {
  pre <- paste(rep("-", level), collapse = "")
  cat(paste0(pre, " executing mork_exact in level ", level, "\n"))
  print(ranking)
  # Stop condition: the matrix has dimensions 2x2
  if(ncol(pw) == level) {
    # Return the candidate
    #return(ifelse(pw[1,2] > 2, 1, 2))
    return(FALSE)
  }
  # Elements to add to the queue to evaluate
  rows <- rowSums(pw)
  cols <- colSums(pw)
  to_explore_indexes <- length(which((rows != 0) & (rows >= cols)))
  to_explore_indexes <- sort((rows-cols)[1:to_explore_indexes], decreasing = TRUE, index.return=TRUE)$ix
  print(tibble::tibble(candidate = names(ranking), 
                       row = rows, 
                       col = cols, 
                       difference = rows-cols) %>% dplyr::arrange(desc(difference)))
  print(to_explore_indexes)
  # If there is any index left to explore
  while(length(to_explore_indexes) > 1) {
    next_index <- to_explore_indexes[1]
    # Add the index to the ranking
    ranking[next_index] <- level
    # Remove the index from the matrix
    pw[next_index,] <- 0
    pw[,next_index] <- 0
    # Call recursively
    mork_exact(pw, ranking, level+1)
    # Remove the index from the list of indexes to explore
    to_explore_indexes <- to_explore_indexes[-1]
    cat(paste(pre, "candidates to explore:\n"))
    print(to_explore_indexes)
  }
  cat(paste(pre, "no more candidates to explore in this level\n"))
}
mork_exact(pw, ranking)

