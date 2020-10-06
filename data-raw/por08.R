## code to prepare `por08` dataset goes here

por08 <- structure(list(profileOfRankings = structure(list(a = c(4L, 
                                                                  1L, 2L, 3L, 2L, 4L, 1L, 3L, 3L), b = c(3L, 3L, 1L, 2L, 3L, 2L, 
                                                                                                          3L, 2L, 4L), c = c(2L, 2L, 3L, 1L, 1L, 1L, 4L, 4L, 1L), d = c(1L, 
                                                                                                                                                                          4L, 4L, 4L, 4L, 3L, 2L, 1L, 2L)), row.names = c(NA, -9L), class = "data.frame"), 
                        numberOfVoters = c(1, 1, 1, 1, 1, 1, 1, 2, 1), candidates = c("a", 
                                                                                      "b", "c", "d")), class = c("por", "list"))
usethis::use_data(por08, overwrite = TRUE)
