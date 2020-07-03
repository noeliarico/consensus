#' @export
#'
#' @title Create Profile of Rankings
#'
#' @description `profile_of_rankings()` creates a profile of rankings.
#' These profile of rankings will be usefull for all the functions
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
  
  if(is_tibble(matrix)) {
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
  unique.rankings <- unique.matrix(matrix,)
  is_ok <- apply(unique.rankings, 1, is.ranking)
  
  if(!all(is_ok)) {
    stop("Error creating the profile of rankings: At least one of the rows is not a ranking")
  }
  
  for(indexrow in 1:nrow(unique.rankings)) {
    v <- unique.rankings[indexrow, ]
    
    #row_is_a_match <- apply(matrix, 1, identical, v)
    # identical does not work for a matrix of 1x1 with names in rows and columns
    row_is_a_match <- apply(matrix, 1, function(x, v) all(x == v), v)
    match_idx <- which(row_is_a_match)
    total_matches <- length(match_idx)
    profileOfRankings <- rbind(profileOfRankings, c(total_matches, v))
  }
  
  colnames(profileOfRankings) <- c('numberOfVoters', candidates)
  class(profileOfRankings) <- c("por", "data.frame")
  
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

#' @export
is.por <- function(x) inherits(x, "por")

#' @export
print.por <- function(profileOfRankings) {
  
  # Matrix that stores the number of votters of each ranking
  gpor <- matrix(data = profileOfRankings[,1], nrow = (nrow(profileOfRankings)))
  
  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get votes
  votes <- splittedPOF$votes
  # Get rankings
  profileOfRankings <- splittedPOF$rankings
  
  
  gr <- apply(profileOfRankings, 1, format.ranking)
  
  gr <- as.data.frame(gr)
  
  gpor <- cbind(gpor, gr)
  
  colnames(gpor) <- c('numberOfVoters', 'ranking')
  print(gpor)
  invisible(gpor)
  
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

#' @title Create random profile of rankings
#' @description Creates a ranfom profile of rankings
#' 
#' @param ncandidates Number of candidates
#' @param nranking Number of rankings in the profile of rankings
#' @param seed Fix a seed to hace reproducible code
#' @param withties The rankings are generated without ties by default.
#'
#' @export
random_profile_of_rankings <- function(ncandidates = 4,
                                       nranking = 10,
                                       seed = NULL,
                                       withties = FALSE) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  if(!withties) {
    rankings <- t(replicate(nranking, sample(1:ncandidates))) %>% as_tibble()
  }
  else {
    rankings <- t(replicate(nranking, ranking(sample(1:(sample(2:ncandidates, 1)), ncandidates, replace = TRUE)))) %>% as_tibble()
  }
  
  names(rankings) <- paste0("C", 1:ncol(rankings))
  por <- profile_of_rankings(rankings)

  return(por)
}

#' @title Latex table of profile of rankings
#' 
#' @param profileOfRankings
#'
#' @export
toLatex.por <- function(profileOfRankings) {
  # TODO probar con format en vez de con print para que no lo imprima
  xtable::xtable(print(profileOfRankings))
}

# -------------------------------------------------------------------------


#' @title Convert matrix to profile of rankings
#' 
#' @param matrix Matrix to convert to a profile of rankings
#'
#' @return A profile of rankings object
#' 
#' @export
as.por <- function(matrix) {
  #TODO takes a numeric matrix with numeric values and not rankings by row
  #print("-----------------------------------------")
  #print("Matrix before apply ranking")
  #print(matrix)
  if(!is.null(colnames(matrix))) {
    candidates <- colnames(matrix)
  }
  else {
    candidates <- paste0("C", 1:ncol(matrix))
  }
  #print("Matrix after apply ranking")
  matrix[] <- t(apply(matrix, 1, ranking))
  #print(matrix)
  colnames(matrix) <- candidates
  #print("Por")
  por <- profile_of_rankings(matrix)
  #print(por)
  #print("-----------------------------------------")
  return(por)
}

#' @title Read rankings from file
#' 
#' @description Given a .csv file, read the rankings in the file and creates
#' a profile of rankings object.
#' 
#' @param file_path Path of the file storing the rankings
#'
#' @export
read_rankings <- function(file_path, from_csv = FALSE) {
  conn <- file(file_path,open = "r")
  lines <- readLines(conn)
  if(from_csv) {
    ncandidates <- length(str_split(lines[1], ",", simplify = TRUE))
    print(ncandidates)
    the_rankings <- matrix(ncol = ncandidates)
    for (line in lines) {
      r <- as.numeric(str_split(line, ",", simplify = TRUE))
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
  print(the_rankings)
  # Remove the first row which is a row of NAs from the creation of the matrix
  the_rankings <- the_rankings[-1, ] 
  rownames(the_rankings) <- NULL
  the_rankings <- profile_of_rankings(the_rankings)
  return(the_rankings)
}

#' @title Parse profile of rankings
#' @description Create profile of rankings using its string representation.
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
#' @export
#'
#' @examples
#' 
#' parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
#'                            5, b ≻ c ≻ a ≻ d,
#'                            3, c ≻ d ≻ a ≻ b")

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

  m <- data.frame(cbind(numberOfVoters, m))
  class(m) <- c("por", "data.frame")
  return(m)
}

#' @export
get_ranking <- function(profileOfRankings, index) {
  
  if(index > nrow(profileOfRankings))
    stop(paste("There are ", nrow(profileOfRankings), "different rankings only."))
  
  # Split votes and rankings
  splittedPOF <- split_profile_of_rankings(profileOfRankings)
  # Get rankings
  rankings <- splittedPOF$rankings
  
  class(rankings) <- "data.frame"
  # Get ranking from index
  ranking <- unlist(rankings[index,])
  class(ranking) <- "ranking"
  return(ranking)
}

