## code to prepare `por13` dataset goes here

por13 <- parse_profile_of_rankings("5, a ≻ b ≻ c ≻ d,
                                    4, b ≻ c ≻ d ≻ a,
                                    2, c ≻ d ≻ b ≻ a,
                                    1, d ≻ c ≻ b ≻ a")
por13 <- set_candidates(por13, paste0("C", 1:4))
usethis::use_data(por13, overwrite = TRUE)
