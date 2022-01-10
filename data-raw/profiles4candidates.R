################################################################################

por01 <- parse_profile_of_rankings("6, c ≻ b ≻ a ≻ d,
                                    5, a ≻ d ≻ b ≻ c, 
                                    3, b ≻ a ≻ d ≻ c")
por01 <- set_candidates(por01, paste0("C", 1:4))
usethis::use_data(por01, overwrite = TRUE)

################################################################################

por02 <- parse_profile_of_rankings("6, a ∼ b ∼ c ≻ d,
                                    5, a ∼ b ∼ d ≻ c, 
                                    3, a ∼ c ∼ d ≻ b")
por02 <- set_candidates(por02, paste0("C", 1:4))
usethis::use_data(por02, overwrite = TRUE)

################################################################################

por03 <- parse_profile_of_rankings("6, c ≻ b ≻ a ≻ d,
                                    5, a ≻ d ≻ c ≻ b, 
                                    3, b ≻ a ≻ d ≻ c")
por03 <- set_candidates(por03, paste0("C", 1:4))
usethis::use_data(por03, overwrite = TRUE)

################################################################################

por04 <- parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
                                    5, b ≻ c ≻ a ≻ d, 
                                    3, c ≻ d ≻ a ≻ b")
por04 <- set_candidates(por04, paste0("C", 1:4))
usethis::use_data(por04, overwrite = TRUE)

################################################################################

por05 <- parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
                                    3, b ≻ c ≻ a ≻ d, 
                                    3, c ≻ d ≻ a ≻ b,
                                    2, b ≻ a ≻ c ≻ d")
por05 <- set_candidates(por05, paste0("C", 1:4))
usethis::use_data(por05, overwrite = TRUE)

################################################################################

por06 <- parse_profile_of_rankings("2, a ~ b ≻ c ~ d,
                                    3, a ~ b ~ d ≻ c, 
                                    1, a ≻ c ~ d ≻ b,
                                    2, a ≻ d ≻ b ~ c,
                                    2, b ~ d ≻ c ≻ a,
                                    4, b ≻ d ≻ a ≻ c")
por06 <- set_candidates(por06, paste0("C", 1:4))
usethis::use_data(por06, overwrite = TRUE)

################################################################################

por07 <- parse_profile_of_rankings("4, c ~ b ≻ a ≻ d,
                                    7, a ≻ d ≻ b ≻ c, 
                                    3, b ~ a ~ d ≻ c")
por07 <- set_candidates(por07, paste0("C", 1:4))
usethis::use_data(por07, overwrite = TRUE)

################################################################################

por08 <- parse_profile_of_rankings(c("    
    1, C1 > C3 > C2 > C4, 
    1, C4 > C3 > C1 > C2, 
    2, C3 > C2 > C4 > C1, 
    1, C2 > C1 > C4 > C3, 
    2, C1 > C4 > C2 > C3, 
    1, C2 > C4 > C1 > C3, 
    1, C2 > C4 > C3 > C1, 
    1, C4 > C1 > C3 > C2"))
usethis::use_data(por08, overwrite = TRUE)

################################################################################

por09 <- parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
                                    4, b ≻ c ≻ a ≻ d, 
                                    3, c ≻ d ≻ a ≻ b,
                                    1, a ≻ c ≻ d ≻ b")
por09 <- set_candidates(por09, paste0("C", 1:4))
usethis::use_data(por09, overwrite = TRUE)

################################################################################

por10 <- parse_profile_of_rankings("4, a ≻ d ≻ b ≻ c,
                                    4, c ≻ a ≻ b ≻ d,
                                    4, b ≻ c ≻ d ≻ a, 
                                    1, d ≻ a ≻ b ≻ c,
                                    1, d ≻ c ≻ a ≻ b")
por10 <- set_candidates(por10, paste0("C", 1:4))
usethis::use_data(por10, overwrite = TRUE)

################################################################################

por11 <- parse_profile_of_rankings("5, a ≻ c ≻ b ≻ d,
                                    3, b ≻ d ≻ a ≻ c,
                                    2, c ≻ a ≻ b ≻ d, 
                                    2, d ≻ b ≻ c ≻ a,
                                    1, c ≻ b ≻ a ≻ d,
                                    1, c ≻ d ≻ b ≻ a")
por11 <- set_candidates(por11, paste0("C", 1:4))
usethis::use_data(por11, overwrite = TRUE)

################################################################################

por12 <- parse_profile_of_rankings("5, a ≻ c ≻ b ≻ d,
                                    4, b ≻ c ≻ a ≻ d,
                                    3, c ≻ d ≻ b ≻ a, 
                                    2, d ≻ c ≻ b ≻ a")
por12 <- set_candidates(por12, paste0("C", 1:4))
usethis::use_data(por12, overwrite = TRUE)

################################################################################

por13 <- parse_profile_of_rankings("5, a ≻ b ≻ c ≻ d,
                                    4, b ≻ c ≻ d ≻ a,
                                    2, c ≻ d ≻ b ≻ a,
                                    1, d ≻ c ≻ b ≻ a")
por13 <- set_candidates(por13, paste0("C", 1:4))
usethis::use_data(por13, overwrite = TRUE)

################################################################################

por14 <- structure(list(profileOfRankings = structure(list(C1 = c(1, 2, 
                                                                  1, 1, 3, 1, 2, 1, 1, 1, 2, 2, 3, 2, 3), C2 = c(1, 2, 1, 2, 1, 
                                                                                                                 2, 2, 1, 1, 4, 3, 3, 3, 1, 2), C3 = c(1, 1, 1, 1, 2, 2, 1, 1, 
                                                                                                                                                       3, 2, 3, 2, 3, 1, 3), C4 = c(3, 1, 1, 2, 4, 2, 2, 2, 3, 3, 1, 
                                                                                                                                                                                    2, 2, 3, 1), C5 = c(2, 2, 2, 1, 2, 1, 2, 2, 2, 4, 3, 1, 1, 2, 
                                                                                                                                                                                                        1)), row.names = c(NA, -15L), class = "data.frame"), numberOfVoters = c(1, 
                                                                                                                                                                                                                                                                                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), candidates = c("C1", 
                                                                                                                                                                                                                                                                                                                                          "C2", "C3", "C4", "C5")), class = c("por", "list"))
usethis::use_data(por14, overwrite = TRUE)

################################################################################


por15 <- structure(list(profileOfRankings = structure(list(a = c(3, 2, 
                                                                 2, 1, 1, 2, 1, 2, 3, 4), b = c(1, 2, 1, 4, 5, 5, 2, 3, 5, 4), 
                                                           c = c(3, 4, 3, 4, 5, 4, 4, 2, 4, 2), d = c(1, 5, 1, 3, 2, 
                                                                                                      6, 3, 3, 2, 2), e = c(4, 1, 3, 5, 3, 2, 6, 2, 2, 3), f = c(1, 
                                                                                                                                                                 4, 2, 6, 3, 5, 1, 3, 2, 3), g = c(2, 5, 1, 6, 4, 3, 3, 1, 
                                                                                                                                                                                                   5, 5), h = c(2, 2, 2, 7, 1, 2, 6, 3, 2, 1), i = c(3, 3, 1, 
                                                                                                                                                                                                                                                     7, 5, 1, 5, 1, 2, 3), j = c(4, 2, 1, 2, 1, 3, 3, 3, 1, 4)), row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                               -10L), class = "data.frame"), numberOfVoters = c(1, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                1, 1, 1, 1, 1, 1), candidates = c("a", "b", "c", "d", "e", "f", 
                                                                                                                                                                                                                                                                                                                                                                                                                  "g", "h", "i", "j")), class = c("por", "list"))
usethis::use_data(por15, overwrite = TRUE)

################################################################################

por16 <- structure(list(profileOfRankings = structure(list(C1 = c(4L, 
                                                                  3L, 3L, 2L), C2 = c(1L, 2L, 4L, 3L), C3 = c(2L, 1L, 1L, 4L), 
                                                           C4 = c(3L, 4L, 2L, 1L)), row.names = c(NA, -4L), class = "data.frame"), 
                        numberOfVoters = c(4, 2, 4, 5), candidates = c("C1", "C2", 
                                                                       "C3", "C4")), class = c("por", "list"))
usethis::use_data(por16, overwrite = TRUE)

