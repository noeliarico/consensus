num <- 9
file_name <- paste0("perm", num, ".csv")
perm <- read.csv(file.path("perms", file_name), header = FALSE)
candidates <- letters[1:num]
names(perm) <- candidates
perm9 <- list(profileOfRankings = perm,
              numberOfVoters = rep(1, nrow(perm)),
              candidates = candidates)
class(perm9) <- c("por", "list")
usethis::use_data(perm9, overwrite = TRUE)
