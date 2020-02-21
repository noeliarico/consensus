context("create ranking")

test_that("Create numeric ranking with names", {

  # Ordered elements

  r <- ranking(c(1, 1, 2, 2), letters[1:4])
  expect_output(print(r), "a ∼ b ≻ c ∼ d")

  r <- ranking(c(1, 1, 2, 2, 2), letters[1:5])
  expect_output(print(r), "a ∼ b ≻ c ∼ d ∼ e")

  r <- ranking(c(1, 1, 1, 1, 2), letters[1:5])
  expect_output(print(r), "a ∼ b ∼ c ∼ d ≻ e")

  r <- ranking(c(1, 2, 3, 3, 4, 5), letters[1:6])
  expect_output(print(r), "a ≻ b ≻ c ∼ d ≻ e ≻ f")

  r <- ranking(c(1, 2, 3, 3, 3, 4), letters[1:6])
  expect_output(print(r), "a ≻ b ≻ c ∼ d ∼ e ≻ f")

  r <- ranking(c(1, 2, 3, 3, 3, 4), letters[1:6])
  expect_output(print(r), "a ≻ b ≻ c ∼ d ∼ e ≻ f")

  r <- ranking(c(1, 2, 3, 3, 3, 3), letters[1:6])
  expect_output(print(r), "a ≻ b ≻ c ∼ d ∼ e ∼ f")

  r <- ranking(c(1, 2, 2, 2, 2, 3), letters[1:6])
  expect_output(print(r), "a ≻ b ∼ c ∼ d ∼ e ≻ f")

  r <- ranking(c(1, 1, 1, 2, 2, 3, 4, 5), letters[1:8])
  expect_output(print(r), "a ∼ b ∼ c ≻ d ∼ e ≻ f ≻ g ≻ h")

  # Not ordered elements

  r <- ranking(c(3, 5, 2, 4, 1), letters[1:5])
  expect_output(print(r), "e ≻ c ≻ a ≻ d ≻ b")

  r <- ranking(c(10, 7, 4, 3, 6, 1, 5, 9, 8, 2), letters[1:10])
  expect_output(print(r), "f ≻ j ≻ d ≻ c ≻ g ≻ e ≻ b ≻ i ≻ h ≻ a")

  # Non consequtive
  r <- ranking(c(18, 11, 10, 13, 17), letters[1:5])
  expect_output(print(r), "c ≻ b ≻ d ≻ e ≻ a")

  r <- ranking(c(27, 23, 43, 50, 29, 44, 37, 17, 20, 41), letters[1:10])
  expect_output(print(r), "h ≻ i ≻ b ≻ a ≻ e ≻ g ≻ j ≻ c ≻ f ≻ d")

})

test_that("Create numeric ranking with names", {

  r <- ranking(c("p", "t", "i", "r", "l"), letters[1:5])
  expect_output(print(r), "p ≻ t ≻ i ≻ r ≻ l ")

})


