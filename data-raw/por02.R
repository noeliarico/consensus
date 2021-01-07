por02 <- parse_profile_of_rankings("6, a ∼ b ∼ c ≻ d,
                                    5, a ∼ b ∼ d ≻ c, 
                                    3, a ∼ c ∼ d ≻ b")
por02 <- set_candidates(por02, paste0("C", 1:4))
usethis::use_data(por02, overwrite = TRUE)
