por05 <- parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
                                    3, b ≻ c ≻ a ≻ d, 
                                    3, c ≻ d ≻ a ≻ b,
                                    2, b ≻ a ≻ c ≻ d")
por05 <- set_candidates(por05, paste0("C", 1:4))
usethis::use_data(por05, overwrite = TRUE)
