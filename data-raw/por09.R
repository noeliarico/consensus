por09 <- parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
                                    4, b ≻ c ≻ a ≻ d, 
                                    3, c ≻ d ≻ a ≻ b,
                                    1, a ≻ c ≻ d ≻ b")
por09 <- set_candidates(por09, paste0("C", 1:4))
usethis::use_data(por09, overwrite = TRUE)
