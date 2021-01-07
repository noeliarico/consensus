por04 <- parse_profile_of_rankings("6, a ≻ b ≻ c ≻ d,
                                    5, b ≻ c ≻ a ≻ d, 
                                    3, c ≻ d ≻ a ≻ b")
por04 <- set_candidates(por04, paste0("C", 1:4))
usethis::use_data(por04, overwrite = TRUE)
