## code to prepare `perm5` dataset goes here
num <- 5
file_name <- paste0("perm", num, ".csv")
perm <- read.csv(file.path("perms", file_name), header = FALSE)
colnames(perm) <- letters[1:num]
perm5 <- profile_of_rankings(perm)
usethis::use_data(perm5, overwrite = TRUE)
