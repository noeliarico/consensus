## code to prepare `por14` dataset goes here

por14 <- structure(list(profileOfRankings = structure(list(C1 = c(1, 2, 
                                                                  1, 1, 3, 1, 2, 1, 1, 1, 2, 2, 3, 2, 3), C2 = c(1, 2, 1, 2, 1, 
                                                                                                                 2, 2, 1, 1, 4, 3, 3, 3, 1, 2), C3 = c(1, 1, 1, 1, 2, 2, 1, 1, 
                                                                                                                                                       3, 2, 3, 2, 3, 1, 3), C4 = c(3, 1, 1, 2, 4, 2, 2, 2, 3, 3, 1, 
                                                                                                                                                                                    2, 2, 3, 1), C5 = c(2, 2, 2, 1, 2, 1, 2, 2, 2, 4, 3, 1, 1, 2, 
                                                                                                                                                                                                        1)), row.names = c(NA, -15L), class = "data.frame"), numberOfVoters = c(1, 
                                                                                                                                                                                                                                                                                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), candidates = c("C1", 
                                                                                                                                                                                                                                                                                                                                          "C2", "C3", "C4", "C5")), class = c("por", "list"))
usethis::use_data(por14, overwrite = TRUE)
