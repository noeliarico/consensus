## code to prepare `perm3` dataset goes here
num <- 4
file_name <- paste0("perm", num, ".csv")
perm <- read.csv(file.path("perms", file_name), header = FALSE)
colnames(perm) <- letters[1:num]
perm4 <- profile_of_rankings(perm)
usethis::use_data(perm4, overwrite = TRUE)
