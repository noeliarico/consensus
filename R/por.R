#' @export
#'
#' @title Create Profile of Rankings
#'
#' @description `profile_of_rankings()` creates a profile of rankings.
#' These profile of rankings will be useful for all the functions
#' implementing ranking rules
#'
#'  # PROFILE OF RANKINGS: \cr
#'  ranking -- numberOfVoters -- candidate1 -- candidate2 ... \cr
#'  1             ..             rankingpos    rankingpos ... \cr
#'  2             ..             rankingpos    rankingpos ... \cr
#'
#' @param matrix matrix where each row reperesents a ranking or vector where
#' the candidates for each ranking are listed row by row
#' @param numberOfVoters the number of voters for each row
#' @param candidates the names of the candidates
#' @param ties boolean vector for representing ties when we are using indexes
#' for the matrix with the ranking
#'
#' @keywords profile of rankings
#'
#' @return A dataframe containing all the data needed to work with a profile
#'  of rankings
#'
#' # Create a profile of rankings (recommended way)
#' profileOfRankings <- profile.of.rankings(candidates = c('a','b','c','d'))
#' profileOfRankings <- add.ranking(profileOfRankings, c('c','b','a','d'), votes = 6)
#' profileOfRankings <- add.ranking(profileOfRankings, c('a','d','b','c'), votes = 5)
#' profileOfRankings <- add.ranking(profileOfRankings, c('b','a','d','c'), votes = 3)
#'
#' # Using the first parameter as vector
#' profileOfRankings <- profile.of.rankings(
#' c('a','b','c','d',
#'  'a','d','b','c',
#'  'd','a','c','b'),
#' candidates = c('a','b','c','d'))
#'
profile_of_rankings <- function(matrix = NULL, numberOfVoters = NULL,
                                candidates = NULL, ties = NULL) {
  
  if(tibble::is_tibble(matrix)) {
    matrix <- as.matrix(matrix)
  }
  
  if(is.data.frame(matrix)) {
    matrix <- as.matrix(matrix)
  }
  
  if(is.null(colnames(matrix))) {
    candidates <- paste0("C", 1:ncol(matrix))
  }
  else {
    candidates <- colnames(matrix)
  }
  colnames(matrix) <- NULL
  
  #colnames(matrix) <- NULL
  
  profileOfRankings <- data.frame()
  
  # evaluate the profile of ranking to get the rows which are unique
  # this means, the different rankings given by the voters
  unique_rankings <- unique.matrix(matrix,)
  is_ok <- apply(unique_rankings, 1, is.ranking)
  
  if(!all(is_ok)) {
    stop("Error creating the profile of rankings: At least one of the rows is not a ranking")
  }
  
  voters <- rep(0, nrow(unique_rankings))
  for(indexrow in 1:nrow(unique_rankings)) {
    v <- unique_rankings[indexrow, ]
    
    #row_is_a_match <- apply(matrix, 1, identical, v)
    # identical does not work for a matrix of 1x1 with names in rows and columns
    row_is_a_match <- apply(matrix, 1, function(x, v) all(x == v), v)
    match_idx <- which(row_is_a_match)
    total_matches <- length(match_idx)
    #profileOfRankings <- rbind(profileOfRankings, c(total_matches, v))
    voters[indexrow] <- total_matches 
    profileOfRankings <- rbind(profileOfRankings, v)
  }
  
  # TODO controlar esto porque una cosa es los que cuente repetidos, el numberOfVoters
  # solo debería de servir en caso
  if(!is.null(numberOfVoters)) {
    voters <- numberOfVoters
  }
  
  colnames(profileOfRankings) <- candidates
  #print(profileOfRankings)
  
  #print(profileOfRankings)
  
  profileOfRankings <- list(profileOfRankings = profileOfRankings, 
                            numberOfVoters = voters,
                            candidates = candidates)
  class(profileOfRankings) <- c("por", "list")
  
  return(profileOfRankings)
  
  # if(is.numeric(matrix) &&
  #    is.null(numberOfVoters) &&
  #    is.null(candidates) &&
  #    is.null(ties)) {
  #
  #   profileOfRankings <- data.frame()
  #
  #   unique.rankings <- unique.matrix(matrix) # rows of the profile of rankings
  #
  #   for(indexrow in 1:nrow(unique.rankings)) {
  #     v <- unique.rankings[indexrow, ]
  #     row.is.a.match <- apply(matrix, 1, identical, v)
  #     match.idx <- which(row.is.a.match)
  #     total.matches <- sum(row.is.a.match)
  #     profileOfRankings <- rbind(profileOfRankings, c(total.matches, v))
  #   }
  #   colnames(matrix) <- LETTERS[1:ncol(matrix)]
  #
  #   names(profileOfRankings) <- c('numberOfVoters',colnames(matrix))
  #
  #
  #   class(profileOfRankings) <- c("por", "data.frame")
  #   return(profileOfRankings)
  #
  # }
  #
  #
  # # PROFILE OF RANKINGS:
  # # ranking -- numberOfVoters -- candidate1 -- candidate2 ...
  # # 1             ..             rankingpos    rankingpos ...
  # # 2             ..             rankingpos    rankingpos ...
  #
  # # ERROR 0: It can do anything with the parameters available
  # if(is.null(matrix) && is.null(numberOfVoters) && is.null(candidates)) {
  #   #print('ERROR 0')
  #   stop("The function must contain one of these parameters at least: matrix | candidates")
  # }
  #
  # # The dataframe that is going to represent the profile of rankings
  # profileOfRankings <- data.frame()
  #
  # # OPC1 : The matrix contains one row for each ranking, the rankings could be
  # # repeated so this option groups them and sets the number of voters for
  # # each one. The rankings are already given in a proper format: columns are
  # # the candidates and each cell the position of the candidate column in the
  # # ranking of the row. So this option ignores "ties"
  # # Use for the function csv.to.profile.of.rankings
  # if(is.matrix(matrix) && is.null(numberOfVoters) && is.null(ties)) {
  #   #print('OPC1')
  #
  #   # ERROR, alguna de las filas tiene algo que no es un número o un número
  #   # mayor que el número de columnas (candidatos)
  #   # IMPLEMENTAR!!
  #
  #   unique.rankings <- unique.matrix(matrix) # rows of the profile of rankings
  #
  #   for(indexrow in 1:nrow(unique.rankings)) {
  #     v <- unique.rankings[indexrow, ]
  #     row.is.a.match <- apply(matrix, 1, identical, v)
  #     match.idx <- which(row.is.a.match)
  #     total.matches <- sum(row.is.a.match)
  #     profileOfRankings <- rbind(profileOfRankings, c(total.matches, v))
  #   }
  #
  #   names(profileOfRankings) <- c('numberOfVoters',colnames(matrix))
  # }
  #
  # # OPC2: The matrix contains one row for each unique ranking.
  # # The numberOfVoters vector contains the vector that represents the number
  # # of voters who has choosen the matching ranking.
  # # The rankings are already given in a proper format: columns are
  # # the candidates and each cell the position of the candidate column in the
  # # ranking of the row. So this option ignores "ties"
  # else if(is.matrix(matrix) && !is.null(numberOfVoters) && is.null(ties)) {
  #   #print('OPC2')
  #
  #   # ERROR: distinto numero de filas o rankings repetidos
  #
  #   profileOfRankings <- data.frame(numberOfVoters, as.matrix.data.frame(matrix))
  #   colnames(profileOfRankings) <- append(c('numberOfVoters'), colnames(matrix))
  #
  # }
  #
  # # OPC3: Basic structure
  # else if(is.null(matrix) && is.null(numberOfVoters) && is.null(ties)) {
  #   #print('OPC3')
  #
  #   profileOfRankings <- data.frame(matrix(ncol = length(candidates)+1, nrow = 0))
  #   colnames(profileOfRankings) <- append('numberOfVoters', candidates)
  # }
  #
  # # OPC4: Opc1 but Matrix as a vector
  # else if(!is.matrix(matrix) && !is.null(candidates) && is.null(numberOfVoters)) {
  #   #print('OPC4')
  #   profileOfRankings <- profile_of_rankings(
  #                           matrix = translate.matrix(matrix, candidates)
  #                         )
  # }
  #
  # # OPC5: Opc2 but Matrix as a vector
  # else if(!is.matrix(matrix) && !is.null(candidates) && !is.null(numberOfVoters)) {
  #   #print('OPC5')
  #   profileOfRankings <- profile_of_rankings(
  #                 matrix = translate.matrix(matrix, candidates),
  #                 numberOfVoters = numberOfVoters
  #               )
  # }
  #
  # else {
  #   print('Invalid combination of parameters')
  # }
  
  class(profileOfRankings) <- c("por", "data.frame")
  return(profileOfRankings)
}



#' For internal purpouse only
split_profile_of_rankings <- function(profileOfRankings) {
  
  if(!is.por(profileOfRankings)) {# || !is.data.frame(profileOfRankings)) {
    stop("The profile of rankings must belong to the classes por and data.frame")
  }
  
  # Get the votes for each ranking
  votes <- profileOfRankings$numberOfVoters
  
  # Remove the column with the votes from the matrix, to work just with rankings
  rankings <- profileOfRankings[,-1, drop = FALSE]
  
  # Get the names of the candidates
  candidates <- colnames(rankings)
  
  # Return a list with multiple values
  return(list("votes" = votes, "rankings" = rankings, "candidates" = candidates))
  
}


#' Create a random profile of rankings
#' 
#' Useful for testing purposes
#'
#' @param ncandidates Number of candidates. Default is 4.
#' @param nvoters Number of voters. Default is 10.
#' @param seed To fix a seed to replicate the results. Default is NULL.
#' @param withties Indicates whether the profile created can contain ties. Default is FALSE.
#' @param cnames Names of the candidates
#' @param distinct Num
#' @param distribution norm or uniform
#'
#' @return A profile of rankings
#' @export
random_profile_of_rankings <- function(ncandidates = 4,
                                       nvoters = 10,
                                       seed = NULL,
                                       withties = FALSE,
                                       cnames = NULL,
                                       distinct = NULL,
                                       distribution = "norm") {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  if(!withties) {
    # Number of distinct rankings in the profile is given
    if(!is.null(distinct)) {
      # It cannot be more distinct rankings than voters
      if(distinct > nvoters) {
        stop("The number of voters must be greater than the number of rankings")
      }
      if(distinct > factorial(ncandidates)) {
        stop(paste("There aren't", distinct, "different rankings with n candidates"))
      }
      ok <- 0
      rankings <- NULL
      while(ok < distinct) {
        # Create "distinct" random rankings. The results can contain repeated rankings
        # therefore the process is repeated until "distinct" different rankings are found
        new_rankings <- t(replicate(distinct, sample(1:ncandidates))) %>% 
          tibble::as_tibble(.name_repair =  ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
        if(!is.null(rankings)) {
          rankings <- dplyr::bind_rows(rankings, new_rankings)
        }
        else { # first time
          rankings <- new_rankings
        }
        rankings <- rankings %>% dplyr::distinct()
        ok <- nrow(rankings)
        # ok <- n_distinct(rankings)
      }
      rankings <- rankings[1:distinct,]
    }
    else { # the number of distinct rankings is not given
      rankings <- t(replicate(nvoters, sample(1:ncandidates))) %>% 
        tibble::as_tibble(.name_repair =  ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
    }
  }
  else { # ranking containing ties
    rankings <- t(replicate(distinct, ranking(sample(1:(sample(2:ncandidates, 1)), ncandidates, replace = TRUE)))) %>% tibble::as_tibble()
  }
  
  if(!is.null(cnames)) {
    if(length(cnames) == 1 && cnames == "letters") {
      if(ncandidates > length(letters)) {
        stop("Not enough letters to name the candidates.")
      }
      else {
        names(rankings) <- letters[1:ncol(rankings)]
      }
    }
    else {
      if(length(cnames) != ncandidates) {
        stop("A name must be given to each candidate.")
      }
      if(length(cnames) != length(unique(cnames))) {
        stop("The name of each candidate must be unique.")
      }
      names(rankings) <- cnames
    }
    
  }
  else {
    names(rankings) <- paste0("C", 1:ncol(rankings))
  }
  
  # If the number of distinct rankings is given, then a number of voters must
  # be assigned to each ranking. As it cannot be a ranking with 0 votes in the
  # profile of rankings this is repeated until a random vector without zeros
  # is found.
  if(!is.null(distinct)) {
    if(distinct==factorial(ncandidates)) {
      nvotersv <- rep(1, distinct)
    } 
    if(distinct==nvoters) {
      nvotersv <- rep(1, nvoters)
    }
    else {
      nvotersv <- 0
      while(any(nvotersv == 0)) {
        nvotersv <- rand_vect(nrow(rankings), nvoters, dist = distribution)
      }
    }
    por <- profile_of_rankings(rankings, numberOfVoters = nvotersv)
  }
  else {
    por <- profile_of_rankings(rankings)
  }
  
  if(!is.null(seed)) {
    if(!is.null(distinct)) {
      s <- c(seed, distinct)
      names(s) <- c("seed", "distinct")
      por$seed <- s
    }
    else {
      s <- seed
      names(s) <- "seed"
      por$seed <- s
    }
  }
  return(por)
}

rand_vect <- function(N, M, sd = 1, pos.only = TRUE, dist = "norm") {
  if(dist == "norm") {
    # create a vector with a value for each of the n rankings
    # there are m voters, so the elements must sum m
    # mean is m/n to make it as close as possible to perfect division
    vec <- stats::rnorm(N, M/N, sd) 
    # if all the values are negative add values
    if (abs(sum(vec)) < 0.01) vec <- vec + 1
    # get integer numbers based on the proportion of the number in the vector
    vec <- round(vec / sum(vec) * M)
    # deviation of the elements from M
    deviation <- M - sum(vec)
    # for each element increment or decrement in one unit
    for (. in seq_len(abs(deviation))) {
      vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
    }
    # ensure that there are only positive numbers > 0
    if (pos.only) while (any(vec <= 0)) {
      negs <- vec <= 0
      pos  <- vec > 0
      vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
      vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
    }
    return(vec)
  }
  else if(dist == "unif") {
    d <- N
    m <- M
    distrib <- round(stats::runif(d, min=1, max=m)) # m voters and d rankings
    v <- round(m*distrib/sum(distrib))
    # Check if there is any zero
    if(any(v==0)) {
      i <- which(v==0)
      v[i] <- 1
    }
    # Check the sum 
    s <- sum(v)
    if(s > m) { # Remove
      diff <- s - m
      for(i in 1:diff) {
        j <- sample(1:d, 1) 
        while(v[j] == 1) { # it must be > 1 to remove and avoid 0s
          j <- sample(1:d, 1) 
        }
        v[j] <- v[j] - 1
      }
    }
    else if(s < m) { # Add
      diff <- m - s
      for(i in 1:diff) {
        j <- sample(1:d, 1) 
        v[j] <- v[j] + 1
      }
    }
    if(sum(v) != m) stop("Error generating random unirform distribution of votes. Incorrect sum.")
    if(any(v == 0)) stop("Error generating random unirform distribution of votes. There is a 0.")
    return(v)
  } else {
    stop("Unkwown distribution")
  }
}

rv <- function(d,m) {
  if(d > m) stop("This function requires d <= m")
  
}

# -------------------------------------------------------------------------


#' Convert matrix to profile of rankings
#' 
#' It is different than the normal one as this can include any number
#' 
#' @param matrix Matrix to convert to a profile of rankings
#' @param critersion Ascending or descending.
#'
#' @return A profile of rankings object
#' 
#' @export
as_por <- function(matrix, criterion = NULL) {
  #print("Matrix before apply ranking")
  #print(matrix)
  if(!is.null(colnames(matrix))) {
    candidates <- colnames(matrix)
  }
  else {
    candidates <- paste0("C", 1:ncol(matrix))
  }
  #print("Matrix after apply ranking")
  if(is.null(criterion)) {
    matrix[] <- t(apply(matrix, 1, ranking))
  }
  else {
    if(length(criterion) > 1) {
      if(!all(criterion %in% c("asc", "desc"))) {
        stop("Unkown criterion. The vector must contain only 'asc' and 'desc'.")
      } 
      criterion <- criterion == "desc"
      matrix <- t(mapply(ranking, as.data.frame(t(matrix)), desc = criterion))
    } else if(criterion == "asc") {
      matrix[] <- t(apply(matrix, 1, ranking))
    } else if (criterion == "desc") {
      matrix[] <- t(apply(matrix, 1, ranking, desc = TRUE))
    } else {
      stop("Unkown criterion. It must be 'asc', 'desc' or a vector 
           containing one of these values for each row.")
    }
  }
  
  
  #print(matrix)
  colnames(matrix) <- candidates
  #print("Por")
  por <- profile_of_rankings(matrix)
  #print(por)
  #print("-----------------------------------------")
  return(por)
}

#' Read rankings from file
#' 
#' Given a .csv file, read the rankings in the file and creates
#' a profile of rankings object.
#' 
#' @param file_path Path of the file storing the rankings
#' @param from_csv Default FALSE
#'
#' @export
read_rankings <- function(file_path, from_csv = FALSE) {
  conn <- file(file_path,open = "r")
  lines <- readLines(conn)
  if(from_csv) {
    ncandidates <- length(stringr::str_split(lines[1], ",", simplify = TRUE))
    #print(ncandidates)
    the_rankings <- matrix(ncol = ncandidates)
    for (line in lines) {
      r <- as.numeric(stringr::str_split(line, ",", simplify = TRUE))
      the_rankings <- the_rankings %>% rbind(r)
    }
  }
  else {
    the_rankings <- matrix(ncol = length(lines))
    for (line in lines){
      r <- parse_ranking(line)
      the_rankings <- the_rankings %>% rbind(r)
    }
  }
  
  close(conn)
  #print(the_rankings)
  # Remove the first row which is a row of NAs from the creation of the matrix
  the_rankings <- the_rankings[-1, ] 
  rownames(the_rankings) <- NULL
  the_rankings <- profile_of_rankings(the_rankings)
  return(the_rankings)
}

#' Parse profile of rankings
#' 
#' Create profile of rankings using its string representation.
#'
#' @param string A string containing a representation of the ranking in the form:
#' "number_of_voters_r1, ranking1,
#'  number_of_voters_r2, ranking2,
#'  ...,
#'  number_of_voters_rN, rankingN"
#'  The operators used for representing that one candidate is preferred to 
#'  another are ≻ and >. For representing ties use ~.
#'
#' @return A profile of rankings object containing the representation.
#'
#' @examples
#' parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
#'                            5, b ≻ c ≻ a ≻ d,
#'                            3, c ≻ d ≻ a ≻ b")
#' @export
parse_profile_of_rankings <- function(string) {
  
  string <- stringr::str_remove(string, "\n")
  string <- stringr::str_split(string, ",", simplify = TRUE)
  m <- NULL
  numberOfVoters <- numeric()
  for (i in seq(1, length(string), 2)) {
    numberOfVoters <- append(numberOfVoters, as.numeric(string[i]))
    if(is.null(m)) {
      m <- t(as.matrix(parse_ranking(string[i+1])))
    }
    else {
      m <- rbind(m, parse_ranking(string[i+1]))
    }
  }

  por <- profile_of_rankings(m, numberOfVoters)
  return(por)
}

#' Get a single ranking from a profile of rankings
#' 
#' The ranking is not deleted from the profile.
#' 
#' @param profileOfRankings object to get the ranking
#' @param index row of the ranking to extract
#'
#' @export
get_ranking <- function(profileOfRankings, index) {
  
  rankings <- profileOfRankings$profileOfRankings
  
  if(index > nrow(rankings))
    stop(paste("There are", nrow(profileOfRankings), "different rankings only."))
  
  # Get ranking from index
  ranking <- unlist(rankings[index,])
  class(ranking) <- "ranking"
  return(ranking)
}

#' Change the names of the candidates of a given profile
#' 
#' @param profileOfRankings Profile to update the name of the candidates.
#' @param newCandidates New name of the candidates.
#' 
#' @return The profile of rankings with the name of the candidates modified.
#'
#' @export
update_candidates <- function(profileOfRankings, newCandidates) {
  if(length(newCandidates) != length(profileOfRankings$candidate))
    stop("You must give the same number of candidates")
  else {
    profileOfRankings$candidates <- newCandidates
    names(profileOfRankings$profileOfRankings) <- newCandidates
  }
  return(profileOfRankings)
}

################################################################################

#' @export
is.por <- function(x, ...) inherits(x, "por")

#' @export
print.por <- function(x, ...) {
  
  gr <- apply(x$profileOfRankings, 1, format.ranking)
  gr <- as.data.frame(gr)
  gpor <- cbind(x$numberOfVoters, gr)
  
  colnames(gpor) <- c('numberOfVoters', 'ranking')
  print(gpor)
  invisible(gpor)
}

#' @export
set_candidates <- function(profileOfRankings, newCandidates) {
  if(length(newCandidates) != length(profileOfRankings$candidate))
    stop("You must give the same number of candidates")
  else {
    profileOfRankings$candidates <- newCandidates
    names(profileOfRankings$profileOfRankings) <- newCandidates
  }
  return(profileOfRankings)
}