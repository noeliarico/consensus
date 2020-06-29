test_that("Letters without ties", {
  expect_equal(as.numeric(parse_ranking("c ≻ a ≻ b ≻ d")), c(2, 3, 1, 4))
  expect_equal(as.numeric(parse_ranking("a ≻ d ≻ b ≻ c")), c(1, 3, 4, 2))
  expect_equal(as.numeric(parse_ranking("b ≻ a ≻ d ≻ c")), c(2, 1, 4, 3))
})

test_that("Letters with ties", {
  expect_equal(as.numeric(parse_ranking("c ~ a ≻ b ≻ d")), c(1, 2, 1, 3))
  expect_equal(as.numeric(parse_ranking("a ~ d ~ b ~ c")), c(1, 1, 1, 1))
  expect_equal(as.numeric(parse_ranking("b ≻ a ≻ d ~ c")), c(2, 1, 3, 3))
})

test_that("Letters and number with ties", {
  expect_equal(as.numeric(parse_ranking("C2 ~ C3 ~ C4 ≻ C1 ~ C5")), c(2, 1, 1, 1, 2))
  expect_equal(as.numeric(parse_ranking("C3 ∼ C5 ≻ C4 ≻ C1 ∼ C2")), c(3, 3, 1, 2, 1))
  expect_equal(as.numeric(parse_ranking("C3 ∼ C5 ≻ C2 ≻ C1 ∼ C4")), c(3, 2, 1, 3, 1))
})

