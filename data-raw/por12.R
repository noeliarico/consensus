por12 <- parse_profile_of_rankings("5, a ≻ c ≻ b ≻ d,
                                    4, b ≻ c ≻ a ≻ d,
                                    3, c ≻ d ≻ b ≻ a, 
                                    2, d ≻ c ≻ b ≻ a")

usethis::use_data(por12, overwrite = TRUE)
