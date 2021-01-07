por03 <- parse_profile_of_rankings("6, c ≻ b ≻ a ≻ d,
                                    5, a ≻ d ≻ c ≻ b, 
                                    3, b ≻ a ≻ d ≻ c")
por03 <- set_candidates(por03, paste0("C", 1:4))
usethis::use_data(por03, overwrite = TRUE)
