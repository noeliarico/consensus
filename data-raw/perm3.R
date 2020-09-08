## code to prepare `perm3` dataset goes here
num <- 3
file_name <- paste0("perm", num, ".csv")
perm <- read.csv(file.path("perms", file_name), header = FALSE)
colnames(perm) <- letters[1:num]
perm3 <- profile_of_rankings(perm)
usethis::use_data(perm3, overwrite = TRUE)
