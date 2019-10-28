calculate_distances <- function(data, distance_methods) {
  # distances <- as.matrix(dist(data, distance))
  # return(distances)

  # list that has a empty profile of rankings for each distance of the test set
  test_list <- rep(list(por), rows_test)

  dist_index <- 0
 for(distance_method in distance_methods) {

    if(developer) {
      cat(paste('\nCalculating the distances for method =',
                distance_method, '\n'))
    }

    dist_index <- dist_index + 1

    # calculate the distance between all the pairs of instances
    distance_values <- dist(data, method = distance_method)

    # create a matrix object cause the distances object is not a full matrix
    # if it is not a matrix we cannot extract rows out of it
    distance_values <- as.matrix(distance_values)

    # subset the distance_values matrix to keep only
    # the distances from objects of the training set to objects of the test set
    # distance_values <- distance_values[-(1:nrow(train)),
    #                                    -((nrow(train)+1):ncol(distance_values))]

    if(developer) {
      cat(paste('\n--> Distances: \n'))
      print(distance_values)
    }
 }
}

ranking_distances <- function(data, distance) {
  # translate those distances to rankings
  rankings <- t(apply(distance_values, 1, as_ranking))

  if(developer) {
    cat(paste('\n--> Rankings: \n'))
    print(rankings)
  }

  # add the ranking to the list
  # each instance has a different matrix
  # that matrix (for each object of the training set)
  # will have one row for each distance_method used
  test_list <- lapply(seq_along(test_list),
                      function(x){ test_list[[x]][dist_index, ] <- rankings[x, ]
                      return(test_list[[x]])
                      })



# create a real profile of rankings for the ranking matrix
# of each instance of the test set
# this means that if two or more different instance methods have given
# the same ranking of instance of the training set for an instance of the test
# set, these rankigs will be merged in only one row and the number of voters
# for that row will be increase. Otherwise the number of voters of each
# row (ranking given by a distance method) will be one
por <- lapply(test_list, profile_of_rankings)

if(developer) {
  cat(paste('\nProfile of rankings: \n'))
  for(eachpor in por) {
    print(graphic_profile_of_rankings(eachpor))
  }
}

save(por, file="por.RData")

rr <- as.character(rr)
}
