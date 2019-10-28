.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my package")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Your name goes here",
    devtools.desc.author = "First Last <first.last@example.com> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}


set.seed(123)
por <- random_profile_of_rankings(6, 4)
toLatex(por)

por <- tribble(~A, ~B, ~C, ~D, ~E, ~F,
       2, 4, 6, 3, 5, 1,
       2, 3, 2, 2, 4, 1,
       4, 3, 2, 5, 4, 1,
       1, 3, 2, 1, 3, 1)
profile_of_rankings(as.data.frame(por))
