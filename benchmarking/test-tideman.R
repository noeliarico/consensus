for(i in 1:1000) {
  set.seed(i)
  print("---------------------------------------------------------------------")
  print("---------------------------------------------------------------------")
  print("---------------------------------------------------------------------")
  print(i)
  por <- random_profile_of_rankings()
  if(ranking_has_ties(tideman(por, break_ties = "max")))
     stop("Ranking with ties")
}

