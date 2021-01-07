## code to prepare `por07` dataset goes here

por07 <- parse_profile_of_rankings("4, c ~ b ≻ a ≻ d,
                                    7, a ≻ d ≻ b ≻ c, 
                                    3, b ~ a ~ d ≻ c")
por07 <- set_candidates(por07, paste0("C", 1:4))
usethis::use_data(por07, overwrite = TRUE)
