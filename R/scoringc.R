#' #' Scoring ranking rules
#' #'
#' #' @param profileOfRankings object of the class profile of rankings
#' #' @param method
#' #' \itemize{
#' ##'  \item{"plurality"}{Stuff}
#' ##'  \item{"borda"}{Stuff}
#' ##'  \item{"t"}{Stuff}
#' ##' }
#' #' @param t only necessary when the \code{method} choosen is
#' #' @param verbose by default FALSE. Change to TRUE for seeing on the screen
#' #'                the workflow of the function
#' #'
#' #' @return the ranking generated after applying the ranking rule
#' #'
#' #' @examples
#' #' @export
#' #'
#' #' @useDynLib consensus pointsToRanking
#' scoringc <- function(profileOfRankings, method = NULL, t = 1, verbose = FALSE, seePoints = FALSE) {
#' 
#'   if(verbose) {
#'     cat('Executing a scoring ranking rule...\n')
#'   }
#' 
#'   attname <- deparse(substitute(profileOfRankings))
#' 
#'   # Split votes and rankings
#'   splittedPOF <- split_profile_of_rankings(profileOfRankings)
#'   # Get votes
#'   votes <- splittedPOF$votes
#'   # Get rankings
#'   profileOfRankings <- splittedPOF$rankings
#'   # Get the candidates
#'   candidates <- splittedPOF$candidates
#' 
#'   # Result vectors
#'   v <- vector(length = ncol(profileOfRankings))
#' 
#' 
#'   # For each ranking in the profile of rankings
#'   for(i in 1:nrow(profileOfRankings)) {
#'     numVotersRow <- votes[i]
#'     ranking <- profileOfRankings[i,]
#' 
#'     p <- calculatePoints(ranking, method, t, verbose, seePoints)
#'     v <- v + (numVotersRow * p)
#' 
#'     if(verbose) {
#'       cat("-> The points for this ranking\n")
#'       print(p)
#'       cat("-> This ranking has ",numVotersRow," voters\n")
#'       cat("-> The current value of the total points is:")
#'       print(v)
#'     }
#' 
#'   }
#' 
#'   #if(seePoints) {
#' 
#'   #}
#' 
#'   ranking <- .C("pointsToRanking",
#'                 ranking = integer(length(v)),
#'                 nranking = as.integer(length(v)),
#'                 points = as.double(v))$ranking
#'   names(ranking) <- names(profileOfRankings)
#' 
#'   return(ranking(ranking))
#' 
#' }
#' 
#' #' @export
#' calculatePoints <- function(ranking, method = NULL, t = 0, verbose = F, seePoints = F) {
#' 
#'   switch(method,
#' 
#'          plurality = {
#'            points <- as.integer(ranking == 1)
#'            total <- sum(ranking == 1)
#'            if(verbose) print(points/total)
#'            if(seePoints) print(points/total)
#'            return(points/total)
#'          },
#' 
#'          veto = {
#'            last_pos <- max(ranking)
#'            points <- as.integer(ranking == last_pos)
#'            total <- sum(ranking == last_pos)
#'            if(verbose) print(points/total)
#'            return(points/total)
#'          },
#' 
#'          t = {
#' 
#'            if(t >= length(ranking)) {
#'              if(seePoints) print(rep(1, length(ranking)))
#'              return(rep(1, length(ranking)))
#'            }
#'            else {
#'              points <- rep(0, length(ranking))
#'              num_of_rewarded_candidates <- 0
#'              i <- 1
#'              while(num_of_rewarded_candidates < t) {
#'                candidates_in_pos_i <- sum(ranking == i)
#'                if((num_of_rewarded_candidates+candidates_in_pos_i) <= t) {
#'                  points[ranking == i] <- 1
#'                }
#'                else {
#'                  real_pos <- num_of_better_candidates <- sum(ranking < i) + 1
#'                  points[ranking == i] <- (t - real_pos + 1)/(candidates_in_pos_i)
#'                }
#'                num_of_rewarded_candidates <- num_of_rewarded_candidates + candidates_in_pos_i
#'                i <- i+1
#'              }
#'              if(seePoints) print(points)
#'              return(points)
#'            }
#' 
#' 
#'            # old implementation
#'            # points <- as.integer(ranking %in% 1:t)
#'            # total <- sum(ranking %in% 1:t)
#'            # if(verbose) print(points/total)
#'            # return(points/total)
#'          },
#' 
#'          borda = {
#' 
#'            # number of different positions in the rankings
#'            n_of_candidates <- length(ranking)
#'            max_pos <- max(ranking)
#' 
#'            if(n_of_candidates == max_pos) {
#'              # the same result would be achieve with the following code
#'              # but here the operation is vectorized so execution time decreases
#'              if(verbose)
#'                cat("No ties for this ranking\n")
#'              return(n_of_candidates - ranking)
#'            }
#' 
#'            # vector that will store the puntuation for each category
#'            points_by_pos <- rep(-1, max_pos)
#' 
#'            # for each category...
#'            for(i in 1:max_pos) {
#'              max_interval <- n_of_candidates - 1 - sum(ranking < i)
#'              min_interval <- max_interval - (sum(ranking == i) - 1)
#'              points_by_pos[i] <- (max_interval+min_interval)/2
#'            }
#' 
#'            points_for_each_candidate <- ranking
#'            points_for_each_candidate <- sapply(ranking,
#'                                                function(x) {points_by_pos[x]})
#' 
#'            if(verbose) {
#'              cat("The points for the ranking...\n")
#'              cat(paste0(format(ranking), "\n"))
#'              cat("...are:\n")
#'              print(points_for_each_candidate)
#'            }
#'          },
#' 
#'          {
#'            stop(paste(method, "is not a valid scoring ranking rule"))
#'          }
#' 
#'   )
#' 
#' 
#'   if(seePoints) print(points_for_each_candidate)
#'   return(points_for_each_candidate)
#' }
#' 
#' 
#' #' Title
#' #'
#' #' @param profileOfRankings
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' plurality <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
#'   scoring(profileOfRankings, "plurality", verbose = verbose, seePoints = seePoints)
#' }
#' 
#' #' Title
#' #'
#' #' @param profileOfRankings
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' veto <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
#'   scoring(profileOfRankings, "veto", verbose = verbose, seePoints = seePoints)
#' }
#' 
#' #' Title
#' #'
#' #' @param profileOfRankings
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' two <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
#'   scoring(profileOfRankings, "t", t = 2, verbose = verbose, seePoints = seePoints)
#' }
#' 
#' #' Title
#' #'
#' #' @param profileOfRankings
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' three <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
#'   scoring(profileOfRankings, "t", t = 3, verbose = verbose, seePoints = seePoints)
#' }
#' 
#' #' Title
#' #'
#' #' @param profileOfRankings
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' five <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
#'   scoring(profileOfRankings, "t", t = 5, verbose = verbose, seePoints = seePoints)
#' }
#' 
#' #' Title
#' #'
#' #' @param profileOfRankings
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' seven <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
#'   scoring(profileOfRankings, "t", t = 7, verbose = verbose, seePoints = seePoints)
#' }
#' 
#' #' Title
#' #'
#' #' @param profileOfRankings
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' borda_count <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
#'   scoring(profileOfRankings, "borda", verbose = verbose)
#' }
#' 
#' #' Title
#' #'
#' #' @param profileOfRankings
#' #' @param verbose
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' borda_winner <- function(profileOfRankings, verbose = FALSE, seePoints = FALSE) {
#'   ranking <- scoring(profileOfRankings, "borda", verbose = verbose)
#'   names(ranking[which.max(ranking)])
#' }
#' 
