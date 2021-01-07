por01 <- parse_profile_of_rankings("6, c ≻ b ≻ a ≻ d,
                                    5, a ≻ d ≻ b ≻ c, 
                                    3, b ≻ a ≻ d ≻ c")
por01 <- set_candidates(por01, paste0("C", 1:4))
usethis::use_data(por01, overwrite = TRUE)
