num <- 10
file_name <- paste0("perm", num, ".csv")
perm <- read.csv(file.path("perms", file_name), header = FALSE)
candidates <- letters[1:num]
names(perm) <- candidates
perm10 <- list(profileOfRankings = perm,
              numberOfVoters = rep(1, nrow(perm)),
              candidates = candidates)
class(perm10) <- c("por", "list")
usethis::use_data(perm10, overwrite = TRUE)
