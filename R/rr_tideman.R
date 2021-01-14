#' Tideman
#'
#' @param profileOfRankings 
#'
#' @return
#' @export
#'
#' @examples
tideman <- function(profileOfRankings, 
                    break_ties = "random", 
                    seePoints = FALSE,
                    seeTrace = FALSE) {
  
  votrix <- votrix(profileOfRankings)
  candidates <- profileOfRankings$candidates
  ncandidates <- nrow(votrix)
  half_nvoters <- sum(profileOfRankings$numberOfVoters) / 2
  
  # TODO arreglar esto para no ocupar memoria de más
  # En realidad si por ejemplo hay 3 rankings con 2, 3 y 4 votos
  # las combinaciones se 2, 3, 4, 5, 6, 7
  votes <- profileOfRankings$numberOfVoters
  # Cada posición de la lista contiene dos vectores, el vector from y el vector to
  
  # For each element of the upper diagonal of the matrix
  # If the element [i,j] is greater or equal than half of the voters:
  # - add them to the vectors
  # Otherwise add the element in [j,i]
  # This insertion must be sorted from highest to lowest value
  
  values <- integer()
  froms <- integer()
  tos <- integer()
  add_both <- FALSE
  
  if(!is.ranking(break_ties) && (break_ties %in% c("random", "max"))) {
    if(break_ties == "random") {
      break_ties <- ranking(sample(1:ncandidates), candidates)
      if(seeTrace) {
        cat("Breaking ties using the ranking\n")
        print(break_ties)
        print(as.numeric(break_ties))
      }
    }
    else { # break_ties == max
      max <- which.max(profileOfRankings$numberOfVoters)
      if(seeTrace) {
      print(paste0("Breaking ties using the ranking with max number of votes (", max, ")"))
      }
      break_ties <- get_ranking(profileOfRankings, max)
      if(seeTrace) {
      print(break_ties)
      }
    }
    
  } else { # the object is a ranking or an invalid argument
    if(!is.ranking(break_ties)) {
      stop("Argument break_ties must be a ranking")
    }
    else if(length(break_ties) != ncandidates) {
      stop("The ranking to break the ties must refer to the same 
           candidates than the profile of rankigs.")
    }
  }
  
  for(i in 1:(ncandidates-1)) {
    for(j in (i+1):ncandidates) {
      
      # Position is empty
      pos <- integer()
      # Value of the next cell of the matrix
      value <- votrix[i, j]
      # If candidate j beats candidate i add to the vector votrix[j, i]
      if(value < half_nvoters) {
        value <- votrix[j, i]
        from <- j
        to <- i
      # If candidate i beats candidate j add to the vector votrix[i, j]
      } else if (value > half_nvoters) { 
        from <- i
        to <- j
      # Both candidates has the same value: value == half_candidates
      # In this case only one pair is going to be added to the graph, otherwise
      # it would introduce a cycle. Then, the one with the candidate "from" in 
      # the best position in the untie ranking goes to the graph
      } else { 
        # The candidate i is in a worse position than the candidate to
        if(break_ties[i] > break_ties[j]) {
          from <- j
          to <- i
        }
        # The candidate j is in a worse position than the candidate i
        else {
          from <- i
          to <- j
        }
      }
      
      if(seeTrace) {
        cat(paste0("Evaluation (", i+j-2,"): ", candidates[from], "->", 
                   candidates[to], " with value ", votrix[from,to]), "\n")
        
      }
      
      
      # Is there a value in the list equal to the value to insert?
      v <- which(values == value)
      # There is a value equal to the value to insert
      if(length(v) > 0) {
        if(seeTrace) {
          cat("There is a pair with the same value \n")
        }
        # Is there any value that goes out from the same node?
        v <- which((froms == from) & (values == value))
        
        # There is a pair with the same value that goes out from the same node
        if(length(v) > 0) {
          if(seeTrace) {
            cat("There is a pair with the same value that goes out from the same node \n")
          }
          
          # Is there a pair that goes out to a node ranked in a worse position?
          c1 <- break_ties[tos] > break_ties[to]
          c2 <- (froms == from)
          c3 <- (values == value)
          v <- which(c1 & c2 & c3)
          if(length(v) > 0) {
            if(seeTrace) {
              cat("There is a pair that goes to a node ranked in a worse position \n")  
            }
            
            pos <- min(v) - 1
          }
          # It must be the first of the nodes with the same value going out of
          # the same node
          else {
            if(seeTrace) {
              cat("There isn't a pair that goes to a node ranked in a worse position \n")
            }
            
            # Then this is the worst
            pos <- max(which((froms == from) & (values == value))) 
          }
          
        # There is not a pair that goes out from the same node
        } else {
          if(seeTrace) {
          cat("There isn't a pair with the same value that goes out from the same node \n")
          }
          # Is there a pair that goes out from a node ranked in a worse position?
          v <- which((break_ties[froms] > break_ties[from]) & (values == value))
          if(length(v) > 0) {
            if(seeTrace) {
              cat("There is a pair that goes out from a node ranked in a worse position \n")
            }
            
            pos <- min(v) - 1
          }
          else {
            if(seeTrace) {
              cat("There isn't a pair that goes out from a node ranked in a worse position \n")
            }
            
            # Then this is the worst
            pos <- max(which(values == value)) 
          }
        }
      } else {
        if(seeTrace) {
          cat("There isn't a pair with the same value \n")
        }
        
        # Is there a value lower than this one?
        v <- which(values < value)
        # There is a value lower than this one
        if(length(v) > 0) {
          if(seeTrace) {
            cat("There is a pair with lower value \n")
          }
          
          # Then the value goes before the first element that is lower
          pos <- min(v) - 1
        } 
        # There is not a value lower than this one, the element is the lowest
        else {
          if(seeTrace) {
            cat("There isn't a pair with lower value \n")
          }
          
          # The element goes in the first position
          pos <- length(values)
        }
      }  
      
      if(seeTrace) {
        cat(paste("Pos:", pos, "\n"))
      }
      
      
      values <- append(values, value, pos)
      froms <- append(froms, from, pos)
      tos <- append(tos, to, pos)
      
      if(seeTrace) {
      cat(paste("Values:\t", paste(values, collapse = "\t"), "\n"))
      cat(paste("Froms:\t", paste(candidates[froms], collapse = "\t"), "\n"))
      cat(paste("Tos:\t", paste(candidates[tos], collapse = "\t"), "\n"))
      cat("-----------------------------------------------------------------\n")
      }
    }
  }
  
  if(seeTrace) {
    cat("=====================================================================\n")
  }
  
  
  # Adjacency matrix
  adjajency <- matrix(rep(FALSE, ncandidates^2), ncol = ncandidates)
  diag(adjajency) <- NA
  rownames(adjajency) <- candidates
  colnames(adjajency) <- candidates

  for(i in 1:length(values)) {
    from <- froms[i]
    to <- tos[i]
    
    if(!adjajency[to,from]) {
      # Update the corresponding cell
      adjajency[from, to] <- TRUE
      # A todos los que va to ahora también puede ir from
      adjajency[from, ] <- adjajency[from, ] | adjajency[to, ] 
      # Todos los que van a from ahora también van a todos los sitios que va from
      also <- which(adjajency[,from])
      if(length(also) > 0) {
        for(i in also) {
          adjajency[i, ] <- adjajency[i, ] | adjajency[from, ]
        }
      }
      if(seeTrace || seePoints) {
      cat(paste0("Adding ", candidates[from], ",", candidates[to], paste0("\t[", paste(rowSums(adjajency, na.rm = T), collapse = ","),"]"), "\n"))
        if(seeTrace) print(adjajency)
      }
    }
    else {
      if(seeTrace || seePoints) {
      cat(paste0("Skipping ", candidates[from], ",", candidates[to], paste0("\t[", paste(rowSums(adjajency, na.rm = T), collapse = ","),"]"), "\n"))
      }
      #stop("Descartando")
    }
  }
  
  if(seeTrace) {
  cat("Adjacency matrix:\n")
  print(adjajency)
  }
  
  position <- rowSums(adjajency, na.rm = TRUE)
  if(seePoints) {
  cat("Score of each candidate\n")
  print(position)
  }
  
  ranking(position)
  
  return(ranking(position, desc = TRUE))

}

