## code to prepare `por08` dataset goes here

por08 <- parse_profile_of_rankings(c("    
    1, C1 > C3 > C2 > C4, 
    1, C4 > C3 > C1 > C2, 
    2, C3 > C2 > C4 > C1, 
    1, C2 > C1 > C4 > C3, 
    2, C1 > C4 > C2 > C3, 
    1, C2 > C4 > C1 > C3, 
    1, C2 > C4 > C3 > C1, 
    1, C4 > C1 > C3 > C2"))
  
usethis::use_data(por08, overwrite = TRUE)
