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
  
  if(is.tibble(matrix)) {
    matrix <- as.matrix(matrix)
  }
  
  if(is.null(colnames(matrix))) {
    colnames(matrix) <- paste0("C", 1:ncol(matrix))
  }
  candidates <- colnames(matrix)
  
  rownames(matrix) <- NULL
  
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
    total_matches <- sum(row_is_a_match)
    profileOfRankings <- rbind(profileOfRankings, c(total_matches, v))
  }
  #print(profileOfRankings)
  names(profileOfRankings) <- c('numberOfVoters', candidates)
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

#' @export
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

#' @export
random_profile_of_rankings <- function(ncandidates = 4,
                                       nranking = 10,
                                       seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  rankings <- t(replicate(nranking, sample(1:ncandidates))) %>% as.tibble()
  names(rankings) <- paste0("C", 1:ncol(rankings))
  por <- profile_of_rankings(rankings)
  return(por)
}

#' @export
toLatex.por <- function(x) {
  xtable(print(x))
}

#' @export
as.por <- function(matrix) {
  #TODO takes a numeric matrix with numeric values and not rankings by row
  print("-----------------------------------------")
  print("Matrix after apply ranking")
  print(matrix)
  print("Matrix after apply ranking")
  matrix[] <- apply(matrix, 1, ranking)
  print(matrix)
  print("Por")
  por <- profile_of_rankings(matrix)
  #print(por)
  print("-----------------------------------------")
  return(por)
}

#' @export
read_rankings <- function(file_path, verbose = FALSE) {
  conn <- file(file_path,open="r")
  lines <- readLines(conn)
  the_rankings <- matrix(ncol = length(lines))
  for (line in lines){
    r <- parse_ranking(line)
    if(verbose) {
      print(r)
    }
    the_rankings <- the_rankings %>% rbind(r)
  }
  close(conn)
  the_rankings <- the_rankings[-1, ]
  rownames(the_rankings) <- NULL
  the_rankings <- profile_of_rankings(the_rankings)
  return(the_rankings)
}

