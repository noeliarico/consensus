test_that("Kendall distance", {
  
  expect_equal(
    kendall(parse_ranking("a ≻ b ≻ c ≻ d"), parse_ranking("a ≻ b ≻ c ≻ d")),
    0
  )
  
  expect_equal(
    kendall(parse_ranking("b ≻ c ≻ a ≻ d"), parse_ranking("a ≻ b ≻ c ≻ d")),
    2
  )
  
  expect_equal(
    kendall(parse_ranking("c ≻ d ≻ a ≻ b"), parse_ranking("a ≻ b ≻ c ≻ d")),
    4
  )
  
  expect_equal(
    kendall(parse_ranking("a ≻ b ≻ c ≻ d"), parse_ranking("c ≻ d ≻ a ≻ b")),
    4
  )
  
  expect_equal(
    kendall(ranking(c(1, 2, 4, 3)), ranking(c(3, 4, 1, 2))),
    5
  )
  
})


test_that("Kendall distance - all permutations 3 candidates", {
  expect_equal(kendall(parse_ranking(c("X1 ≻ X3 ≻ X2")), parse_ranking(c("X1 ≻ X3 ≻ X2"))), 0)
  expect_equal(kendall(parse_ranking(c("X1 ≻ X3 ≻ X2")), parse_ranking(c("X2 ≻ X1 ≻ X3"))), 2)
  expect_equal(kendall(parse_ranking(c("X1 ≻ X3 ≻ X2")), parse_ranking(c("X3 ≻ X1 ≻ X2"))), 1)
  expect_equal(kendall(parse_ranking(c("X1 ≻ X3 ≻ X2")), parse_ranking(c("X3 ≻ X2 ≻ X1"))), 2)
  expect_equal(kendall(parse_ranking(c("X1 ≻ X3 ≻ X2")), parse_ranking(c("X2 ≻ X3 ≻ X1"))), 3)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X1 ≻ X3")), parse_ranking(c("X1 ≻ X3 ≻ X2"))), 2)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X1 ≻ X3")), parse_ranking(c("X2 ≻ X1 ≻ X3"))), 0)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X1 ≻ X3")), parse_ranking(c("X3 ≻ X1 ≻ X2"))), 3)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X1 ≻ X3")), parse_ranking(c("X3 ≻ X2 ≻ X1"))), 2)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X1 ≻ X3")), parse_ranking(c("X2 ≻ X3 ≻ X1"))), 1)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X1 ≻ X2")), parse_ranking(c("X1 ≻ X3 ≻ X2"))), 1)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X1 ≻ X2")), parse_ranking(c("X2 ≻ X1 ≻ X3"))), 3)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X1 ≻ X2")), parse_ranking(c("X3 ≻ X1 ≻ X2"))), 0)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X1 ≻ X2")), parse_ranking(c("X3 ≻ X2 ≻ X1"))), 1)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X1 ≻ X2")), parse_ranking(c("X2 ≻ X3 ≻ X1"))), 2)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X2 ≻ X1")), parse_ranking(c("X1 ≻ X3 ≻ X2"))), 2)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X2 ≻ X1")), parse_ranking(c("X2 ≻ X1 ≻ X3"))), 2)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X2 ≻ X1")), parse_ranking(c("X3 ≻ X1 ≻ X2"))), 1)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X2 ≻ X1")), parse_ranking(c("X3 ≻ X2 ≻ X1"))), 0)
  expect_equal(kendall(parse_ranking(c("X3 ≻ X2 ≻ X1")), parse_ranking(c("X2 ≻ X3 ≻ X1"))), 1)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X3 ≻ X1")), parse_ranking(c("X1 ≻ X3 ≻ X2"))), 3)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X3 ≻ X1")), parse_ranking(c("X2 ≻ X1 ≻ X3"))), 1)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X3 ≻ X1")), parse_ranking(c("X3 ≻ X1 ≻ X2"))), 2)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X3 ≻ X1")), parse_ranking(c("X3 ≻ X2 ≻ X1"))), 1)
  expect_equal(kendall(parse_ranking(c("X2 ≻ X3 ≻ X1")), parse_ranking(c("X2 ≻ X3 ≻ X1"))), 0)
})

