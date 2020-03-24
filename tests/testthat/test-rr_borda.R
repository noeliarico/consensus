test_that("Borda points without ties", {
  
})

test_that("Borda points with ties", {
  
  r <- parse_ranking("C2 ∼ C3 ∼ C4 ≻ C1 ∼ C5")
  p <- calculatePoints(r, "borda")
  expect_equal(p, c(0.5, 3.0, 3.0, 3.0, 0.5 ))
  
  r <- parse_ranking("C3 ∼ C5 ≻ C4 ≻ C1 ∼ C2")
  p <- calculatePoints(r, "borda")
  expect_equal(p, c(0.5, 0.5, 3.5, 2.0, 3.5 ))
  
  r <- parse_ranking("C1 ≻ C3 ∼ C4 ∼ C5 ≻ C2")
  p <- calculatePoints(r, "borda")
  expect_equal(p, c(4.0, 0.0, 2.0, 2.0, 2.0 ))
  
})
