# Define matrix
# Recursive method
# If dimensions of the matrix > 2x2 --> stop
# else:
# check the next elements to evaluate rowSums > colSums
# add to the queue
# call recursively the first element in the queue
# results is a list of the rankings

mork_exact <- function(profileOfRankings) {
  # pw contains the pairwise comparisons between all the candidates
  pw <- votrix(profileOfRankings)
  # por contains the matrix storing the rankings in the profile of rankings
  por <- profileOfRankings$profileOfRankings
  # votes contains the number of votes for each ranking in the profile
  voter <- profileOfRankings$numberOfVoters
  # 
  
}
pw <- votrix(por04)
ranking <- rep(0, 4)
explored <- rep(FALSE, 4)
names(ranking) <- colnames(pw)
to_explore_pos <- 0
candidates_to_explore <- 0

mork_exact_rec <- function(pw, por, votes, ranking, explored, distance, level = 1) {
  
  pre <- paste(rep("-", level), collapse = "")
#  cat(paste0(pre, " executing mork_exact in level ", level, "\n"))
#  print(ranking)

  # Stop condition 1: the matrix has dimensions 2x2
  if(ncol(pw) == level) {
    # Return the candidate
    ranking[which(ranking==0)] <- ncol(pw)
    best = list(ranking, distance = 1)
    cat(paste(pre, format(consensus::ranking(ranking)), "distance:", distance, "\n"))
    return(FALSE)
  }

  # Elements to add to the queue to evaluate
  rows <- rowSums(pw)
  cols <- colSums(pw)
  to_explore_indexes <- length(which((rows != 0) & (rows >= cols)))
  to_explore_indexes <- (sort(rows-cols, decreasing = TRUE, index.return=TRUE)$ix)[1:to_explore_indexes]
  # print(tibble::tibble(candidate = names(ranking),
  #                      row = rows,
  #                      col = cols,
  #                      difference = rows-cols) %>% dplyr::arrange(desc(difference)))
  # print(to_explore_indexes)

  # If there is any index left to explore
  while(length(to_explore_indexes) >= 1) {
    next_index <- to_explore_indexes[1]
    # Add the index to the ranking
    new_ranking <- ranking
    if(length(new_ranking[new_ranking!=0]) > 1)
      cat(paste(pre, format(ranking(new_ranking[new_ranking!=0])), "\n"))
    else
      cat(paste(pre, names(new_ranking[new_ranking!=0]), "\n"))
    new_ranking[next_index] <- level
    # Remove the index from the matrix
    new_pw <- pw
    new_pw[next_index,] <- 0
    new_pw[,next_index] <- 0
    # Mark as explored
    new_explored <- explored
    new_explored[next_index] <- TRUE
    # Compute the distance
    new_distance <- distance + distance_rp(new_ranking, next_index, por, new_explored, votes)
    # Call recursively
    # mork_exact(pw, por04$profileOfRankings, ranking, explored, 0, level = 1)
    mork_exact(new_pw, por, votes, new_ranking, new_explored, new_distance, level+1)
    # Remove the index from the list of indexes to explore
    to_explore_indexes <- to_explore_indexes[-1]
    # cat(paste(pre, "candidates to explore:\n"))
    # print(to_explore_indexes)
  }
  # cat(paste(pre, "no more candidates to explore in this level\n"))
}
mork_exact(pw, por04$profileOfRankings, por04$numberOfVoters, ranking, explored, 0, level = 1)

distance_rr <- function(r1, r2, c1, c2, nvotes) {
  if(((r1[c1] < r1[c2]) && (r2[c1] > r2[c2])) ||
     ((r1[c1] > r1[c2]) && (r2[c1] > r2[c2])))
    return(1*nvotes)
  else if(((r1[c1] == r1[c2]) && (r2[c1] != r2[c2])) ||
          ((r1[c1] == r1[c2]) && (r2[c1] != r2[c2])))
    return(0.5*nvotes)
  else
    return(0)
}

distance_rp <- function(r1, c1, por, explored, nvotes) {
  print(explored)
  d <- 0
  for(i in 1:nrow(por)) {
    r2 <- por[i,]
    for(c2 in 1:ncol(por)) {
      if(c2 != c1 && !explored[c2]) {
        d <- d + distance_rr(r1, r2, c1, c2, nvotes[i])
      }
    }
  }
  return(d)
}

