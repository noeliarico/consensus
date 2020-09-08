test_that("multiplication works", {
  
  r <- parse_ranking("a = b > c = d")
  expect_equivalent(calculatePoints(r, "t", 1), c(0.5, 0.5, 0.0, 0.0))
  expect_equivalent(calculatePoints(r, "t", 2), c(1, 1, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 3), c(1, 1, 0.5, 0.5))
  
  r <- parse_ranking("a~b>c~d~e")
  expect_equivalent(calculatePoints(r, "t", 1), c(0.5, 0.5, 0, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 2), c(1, 1, 0, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 3), c(1, 1, 0.25, 0.25, 0.25))
  
  r <- parse_ranking("a~b~c~d>e")
  expect_equivalent(calculatePoints(r, "t", 1), c(0.25, 0.25, 0.25, 0.25, 0))
  expect_equivalent(calculatePoints(r, "t", 2), c(0.4, 0.4, 0.4, 0.4, 0))
  expect_equivalent(calculatePoints(r, "t", 3), c(0.6, 0.6, 0.6, 0.6, 0))
  
  r <- parse_ranking("a>b>c~d>e>f")
  expect_equivalent(calculatePoints(r, "t", 1), c(1, 0, 0, 0, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 2), c(1, 1, 0, 0, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 3), c(1, 1, 1/2, 1/2, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 4), c(1, 1, 1, 1, 0, 0))
  
  r <- parse_ranking("a>b>c~d~e>f")
  expect_equivalent(calculatePoints(r, "t", 1), c(1, 0, 0, 0, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 2), c(1, 1, 0, 0, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 3), c(1, 1, 1/2, 1/2, 1/2, 0))
  expect_equivalent(calculatePoints(r, "t", 4), c(1, 1, 1, 1, 0, 0))
  expect_equivalent(calculatePoints(r, "t", 5), c(1, 1, 1, 1, 1, 0))
  
})
