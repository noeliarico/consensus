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
