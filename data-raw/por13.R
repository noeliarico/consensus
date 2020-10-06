## code to prepare `por13` dataset goes here

por13 <- parse_profile_of_rankings("5, a ≻ b ≻ c ≻ d,
                                    3, b ≻ c ≻ d ≻ a,
                                    2, c ≻ d ≻ b ≻ a,
                                    1, d ≻ c ≻ b ≻ a")

usethis::use_data(por13, overwrite = TRUE)
