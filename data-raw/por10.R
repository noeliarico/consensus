por10 <- parse_profile_of_rankings("4, a ≻ d ≻ b ≻ c,
                                    4, c ≻ a ≻ b ≻ d,
                                    4, b ≻ c ≻ d ≻ a, 
                                    1, d ≻ a ≻ b ≻ c,
                                    1, d ≻ c ≻ a ≻ b")
por10 <- set_candidates(por10, paste0("C", 1:4))
usethis::use_data(por10, overwrite = TRUE)
