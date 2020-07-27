library(tidyverse)
library(tibble)

# set.seed(2020)
# m1 <- t(replicate(5, sample(1:10, 5)))
# m2 <- matrix(sample(1:10, 50, replace = TRUE), ncol = 5)


m1 <- structure(c(7L, 4L, 2L, 2L, 7L, 6L, 6L, 6L, 8L, 4L, 8L, 1L, 5L,
                  9L, 2L, 1L, 5L, 10L, 4L, 9L, 10L, 2L, 3L, 10L, 5L), .Dim = c(5L,
                                                                               5L))
m2 <- structure(c(7L, 6L, 8L, 1L, 1L, 4L, 10L, 6L, 1L, 8L, 8L, 10L,
                  2L, 6L, 2L, 3L, 2L, 8L, 8L, 4L, 2L, 7L, 4L, 2L, 4L, 5L, 4L, 6L,
                  2L, 10L, 6L, 8L, 2L, 1L, 6L, 3L, 9L, 9L, 5L, 10L, 6L, 2L, 9L,
                  3L, 8L, 2L, 1L, 6L, 7L, 8L), .Dim = c(10L, 5L))


test_that("Without criterion, without ties", {
  por_m1 <- as_por(m1)
  expect_equal(format(get_ranking(por_m1, 1)), "C4 ≻ C2 ≻ C1 ≻ C3 ≻ C5")
  expect_equal(format(get_ranking(por_m1, 2)), "C3 ≻ C5 ≻ C1 ≻ C4 ≻ C2")
  expect_equal(format(get_ranking(por_m1, 3)), "C1 ≻ C5 ≻ C3 ≻ C2 ≻ C4")
  expect_equal(format(get_ranking(por_m1, 4)), "C1 ≻ C4 ≻ C2 ≻ C3 ≻ C5")
  expect_equal(format(get_ranking(por_m1, 5)), "C3 ≻ C2 ≻ C5 ≻ C1 ≻ C4")
})

test_that("Descending criterion, without ties", {
  por_m1 <- as_por(m1, criterion = "desc")
  expect_equal(format(get_ranking(por_m1, 1)), "C5 ≻ C3 ≻ C1 ≻ C2 ≻ C4")
  expect_equal(format(get_ranking(por_m1, 2)), "C2 ≻ C4 ≻ C1 ≻ C5 ≻ C3")
  expect_equal(format(get_ranking(por_m1, 3)), "C4 ≻ C2 ≻ C3 ≻ C5 ≻ C1")
  expect_equal(format(get_ranking(por_m1, 4)), "C5 ≻ C3 ≻ C2 ≻ C4 ≻ C1")
  expect_equal(format(get_ranking(por_m1, 5)), "C4 ≻ C1 ≻ C5 ≻ C2 ≻ C3")
})

test_that("Mixed criterion, without ties", {
  por_m1 <- as_por(m1, criterion = c("desc", "asc", "asc", "desc", "desc"))
  expect_equal(format(get_ranking(por_m1, 1)), "C5 ≻ C3 ≻ C1 ≻ C2 ≻ C4")
  expect_equal(format(get_ranking(por_m1, 2)), "C3 ≻ C5 ≻ C1 ≻ C4 ≻ C2")
  expect_equal(format(get_ranking(por_m1, 3)), "C1 ≻ C5 ≻ C3 ≻ C2 ≻ C4")
  expect_equal(format(get_ranking(por_m1, 4)), "C5 ≻ C3 ≻ C2 ≻ C4 ≻ C1")
  expect_equal(format(get_ranking(por_m1, 5)), "C4 ≻ C1 ≻ C5 ≻ C2 ≻ C3")
  
  por_m1 <- as_por(m1, criterion = c("asc", "desc", "asc", "desc", "asc"))
  expect_equal(format(get_ranking(por_m1, 1)), "C4 ≻ C2 ≻ C1 ≻ C3 ≻ C5")
  expect_equal(format(get_ranking(por_m1, 2)), "C2 ≻ C4 ≻ C1 ≻ C5 ≻ C3")
  expect_equal(format(get_ranking(por_m1, 3)), "C1 ≻ C5 ≻ C3 ≻ C2 ≻ C4")
  expect_equal(format(get_ranking(por_m1, 4)), "C5 ≻ C3 ≻ C2 ≻ C4 ≻ C1")
  expect_equal(format(get_ranking(por_m1, 5)), "C3 ≻ C2 ≻ C5 ≻ C1 ≻ C4")
})
