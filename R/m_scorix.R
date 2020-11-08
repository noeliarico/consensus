#' Scorix
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
scorix <- function(profileOfRankings) {

  votes <- profileOfRankings$numberOfVoters
  candidates <- profileOfRankings$candidates
  por <- profileOfRankings$profileOfRankings

  has_ties <- any(apply(por, 1, function(x) length(unique(x))) != length(candidates))
  
  if(!has_ties) {
    s <- matrix(
      .C("scorix",
         profileOfRankings = as.integer(t(por)),
         votes = as.integer(votes),
         ncandidates = as.integer(ncol(por)),
         nrankings = as.integer(length(votes)),
         results = integer(ncol(por)^2)
      )$results, byrow = TRUE, nrow = ncol(por))
  }
  else {
    s <- matrix(rep(0, ncol(por)^2), ncol = ncol(por))
    for(i in 1:nrow(por)) {
      s <- s + (votes[i] * times_position(get_ranking(profileOfRankings, i)))
    }
  }
  colnames(s) <- paste0("p", 1:length(candidates))
  rownames(s) <- candidates
  
  arg <- as.character(sys.call())[2]
  profileOfRankings$scorix <- s
  assign(arg, profileOfRankings, envir = globalenv())
  
  return(s)
}

times_position <- function(ranking) {
  print("---------------------------")
  print(ranking)
  n <- length(ranking)
  counts <- as.numeric(table(ranking))
  results <- matrix(rep(0, n^2), ncol = n)
  
  print("counts:")
  print(counts)
  for(i in 1:n) {
    count <- counts[ranking[i]]
    share <- 1 / count
    print("count:")
    print(count)
    print("share:")
    print(share)
    
    if(ranking[i] > 1) {
      from <- sum(counts[1:(ranking[i]-1)])+1
    } else {
      from <- 1
    }
    
    print("from:")
    print(from)
    results[i, from:(from+count-1)] <- share
  }
  
  colnames(results) <- names(ranking)
  return(results)
}
