## code to prepare `perm6` dataset goes here
num <- 6
file_name <- paste0("perm", num, ".csv")
perm <- read.csv(file.path("perms", file_name), header = FALSE)
colnames(perm) <- letters[1:num]
perm6 <- profile_of_rankings(perm)
usethis::use_data(perm6, overwrite = TRUE)
