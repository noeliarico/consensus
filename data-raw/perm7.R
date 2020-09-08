## code to prepare `perm7` dataset goes here
num <- 7
file_name <- paste0("perm", num, ".csv")
perm <- read.csv(file.path("perms", file_name), header = FALSE)
colnames(perm) <- letters[1:num]
perm7 <- profile_of_rankings(perm)
usethis::use_data(perm7, overwrite = TRUE)
