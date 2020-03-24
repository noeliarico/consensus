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

test_that("Parse ranking", {
  
  # TODO numeric 
  # TODO candidates names start with number
  # TODO with letters
  # TODO without ties
  
  r <- parse_ranking("C2 ∼ C3 ∼ C4 ≻ C1 ∼ C5")
  expect_s3_class(r, "ranking")
  expect_equal(as.numeric(r), c(2, 1, 1, 1, 2))
  
  r <- parse_ranking("C3 ∼ C5 ≻ C4 ≻ C1 ∼ C2")
  expect_s3_class(r, "ranking")
  expect_equal(as.numeric(r), c(3, 3, 1, 2, 1))
  
  r <- parse_ranking("C1 ≻ C3 ∼ C4 ∼ C5 ≻ C2")
  expect_s3_class(r, "ranking")
  expect_equal(as.numeric(r), c(1, 3, 2, 2, 2))
  
})