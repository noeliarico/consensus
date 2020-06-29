#' Calculate points for scoring ranking ruless
#'
#' @param ranking
#'
#' @param method
#' @param t
#' @param verbose
#' @param seePoints
#'
#' @export
calculatePoints <- function(ranking, method = NULL, t = 0, verbose = F, seePoints = F) {

  switch(method,

# plurality ---------------------------------------------------------------

    plurality = {
       points <- as.integer(ranking == 1)
       total <- sum(ranking == 1)
       if(verbose) print(points/total)
       if(seePoints) print(points/total)
       return(points/total)
     },

# veto --------------------------------------------------------------------

    veto = {
      last_pos <- max(ranking)
      points <- as.integer(ranking == last_pos)
      total <- sum(ranking == last_pos)
      if(verbose) print(points/total)
      return(points/total)
    },


# tapproval ---------------------------------------------------------------

    t = {

      if(t >= length(ranking)) {
        if(seePoints) print(rep(1, length(ranking)))
        return(rep(1, length(ranking)))
      }
      else {
        points <- rep(0, length(ranking))
        num_of_rewarded_candidates <- 0
        i <- 1
        while(num_of_rewarded_candidates < t) {
          candidates_in_pos_i <- sum(ranking == i)
          if((num_of_rewarded_candidates+candidates_in_pos_i) <= t) {
            points[ranking == i] <- 1
          }
          else {
            real_pos <- num_of_better_candidates <- sum(ranking < i) + 1
            points[ranking == i] <- (t - real_pos + 1)/(candidates_in_pos_i)
          }
          num_of_rewarded_candidates <- num_of_rewarded_candidates + candidates_in_pos_i
          i <- i+1
        }
        if(seePoints) print(points)
        return(points)
      }

           # old implementation
           # points <- as.integer(ranking %in% 1:t)
           # total <- sum(ranking %in% 1:t)
           # if(verbose) print(points/total)
           # return(points/total)
    },

# borda -------------------------------------------------------------------

    borda = {

      # number of different positions in the rankings
      n_of_candidates <- length(ranking)
      max_pos <- max(ranking)

      # For the rankings without ties the result is
      if(n_of_candidates == max_pos) { # ranking without ties
        # the same result would be achieve with the following code
        # but here the operation is vectorized so execution time decreases

        if(verbose)
          cat("-- Ranking without ties -- \n")

        return(n_of_candidates - as.numeric(ranking))
      }
      else { # ranking with ties
        # vector that will store the puntuation for each category
        points_by_pos <- rep(-1, max_pos)

        # for each category...
        for(i in 1:max_pos) {
          max_interval <- n_of_candidates - 1 - sum(ranking < i)
          min_interval <- max_interval - (sum(ranking == i) - 1)
          points_by_pos[i] <- (max_interval+min_interval)/2 # center points
        }

        points_for_each_candidate <- ranking
        points_for_each_candidate <- sapply(ranking,
                                            function(x) {points_by_pos[x]})
        if(verbose) {
          cat("The points for the ranking...\n")
          print(ranking)
          cat("...are:\n")
          print(points_for_each_candidate)
        }
      }
    },


# others ------------------------------------------------------------------

    {
      stop(paste(method, "is not a valid scoring ranking rule"))
    }

  )

  if(seePoints) print(points_for_each_candidate)
  return(points_for_each_candidate)
}
