por11 <- parse_profile_of_rankings("5, a ≻ c ≻ b ≻ d,
                                    3, b ≻ d ≻ a ≻ c,
                                    2, c ≻ a ≻ b ≻ d, 
                                    2, d ≻ b ≻ c ≻ a,
                                    1, c ≻ b ≻ a ≻ d,
                                    1, c ≻ d ≻ b ≻ a")
por11 <- set_candidates(por11, paste0("C", 1:4))
usethis::use_data(por11, overwrite = TRUE)