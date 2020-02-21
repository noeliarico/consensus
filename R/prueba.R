.onAttach <- function(libname, pkgname) {
  packageStartupMessage("#CONSENSUS#")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Noelia",
    devtools.desc.author = "Noelia Rico <noeliarico@uniovi.es> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

#
# set.seed(123)
# por <- random_profile_of_rankings(6, 4)
# toLatex(por)
#
# por <- tribble(~A, ~B, ~C, ~D, ~E, ~F,
#                2, 4, 6, 3, 5, 1,
#                2, 3, 2, 2, 4, 1,
#                4, 3, 2, 5, 4, 1,
#                1, 3, 2, 1, 3, 1)
# por <- profile_of_rankings(por)
# borda_count(por, seePoints = T)
