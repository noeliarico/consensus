## code to prepare `por06` dataset goes here
por06 <- parse_profile_of_rankings("2, a ~ b ≻ c ~ d,
                                    3, a ~ b ~ d ≻ c, 
                                    1, a ≻ c ~ d ≻ b,
                                    2, a ≻ d ≻ b ~ c,
                                    2, b ~ d ≻ c ≻ a,
                                    4, b ≻ d ≻ a ≻ c")
por06 <- set_candidates(por06, paste0("C", 1:4))
usethis::use_data(por06, overwrite = TRUE)
