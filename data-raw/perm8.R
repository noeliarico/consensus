## code to prepare `perm8` dataset goes here
num <- 8
file_name <- paste0("perm", num, ".csv")
perm <- read.csv(file.path("perms", file_name), header = FALSE)
colnames(perm) <- letters[1:num]
perm8 <- profile_of_rankings(perm)
usethis::use_data(perm8, overwrite = TRUE)
