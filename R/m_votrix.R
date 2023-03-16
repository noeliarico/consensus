#' Votrix
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
votrix <- function(profileOfRankings) {
  
  if (!is.null(profileOfRankings$votrix)) {
    return(profileOfRankings$votrix)
  }
  
  votes <- profileOfRankings$numberOfVoters
  candidates <- profileOfRankings$candidates
  por <- profileOfRankings$profileOfRankings
  
  #print(as.integer(t(por)))
  #print(integer(ncol(por)^2))
  
  v <- matrix(.C("votrix",
     profileOfRankings = as.integer(t(por)),
     votes = as.integer(votes),
     totalvotes = as.integer(sum(votes)),
     ncandidates = as.integer(length(candidates)),
     nrankings = as.integer(length(votes)),
     votrix = double(ncol(por)^2)
  )$votrix, nrow = length(candidates))

  colnames(v) <- candidates
  rownames(v) <- candidates
  
  arg <- as.character(sys.call())[2]
  profileOfRankings$votrix <- v
  assign(arg, profileOfRankings, envir = parent.frame())
  
  return(v)
}

#' Writes votrix as numpy matrix
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
votrix_to_python <- function(profileOfRankings, name = "om") {
  v <- votrix(profileOfRankings)
  out <- paste0(name, " = np.array([\n")
  to_np <- function(v) {
    return(paste0("[", paste(v, collapse = ","), "]"))
  }
  rows <- apply(v, 1, to_np)
  rows[1:(length(rows)-1)] <- sapply(rows[1:(length(rows)-1)], function(x) {paste0(x, ",")})
  rows[(length(rows))] <- paste0(rows[(length(rows))], "])")
  rows <- sapply(rows, function(x) paste0("\t", x))
  rows <- paste(rows, collapse = "\n")
  out <- paste0(out, rows)
  out <- paste0(out, "\n\n")
  return(out)
}

#' Directed graph
#' 
#' Get the votrix as a directed graph, keeping only the preferences
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
directed_graph <- function(profileOfRankings, copy = TRUE) {
  v <- votrix(profileOfRankings)
  for(i in 1:ncol(v)) {
    for(j in i:nrow(v)) {
      if(i != j) {
        dif <- v[i,j] - v[j,i]
        if(dif > 0) {
          v[i,j] <- dif
          v[j,i] <- 0
        } else {
          v[i,j] <- 0
          v[j,i] <- -dif
        }
      }
    }
  }
  if(!copy)
    return(v)
  else {
    return(cat(paste(apply(v, 1, function(x) paste(x, collapse = ",")), collapse = "\n")))
  }
    
}


#' Copeland
#' 
#' @param profileOfRankings 
#'
#' @return
#' @export
#' @examples
copeland_matrix <- function(profileOfRankings, copy = TRUE) {
  v <- votrix(profileOfRankings)
  half <- (v[1,2]+v[2,1])/2
  return(t(apply(v, 1, function(x) ifelse(x > half, 1, ifelse(x == half, 1/2, 0)))))
}