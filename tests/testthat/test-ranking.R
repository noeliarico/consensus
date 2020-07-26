context("create ranking")

test_that("Create numeric ranking with names", {
  
  # Ordered elements
  
  r <- ranking(c(1, 1, 2, 2), letters[1:4])
  expect_output(print(r), "a \u223C b \u227B c \u223C d")
  
  r <- ranking(c(1, 1, 2, 2, 2), letters[1:5])
  expect_output(print(r), "a \u223C b \u227B c \u223C d \u223C e")
  
  r <- ranking(c(1, 1, 1, 1, 2), letters[1:5])
  expect_output(print(r), "a \u223C b \u223C c \u223C d \u227B e")
  
  r <- ranking(c(1, 2, 3, 3, 4, 5), letters[1:6])
  expect_output(print(r), "a \u227B b \u227B c \u223C d \u227B e \u227B f")
  
  r <- ranking(c(1, 2, 3, 3, 3, 4), letters[1:6])
  expect_output(print(r), "a \u227B b \u227B c \u223C d \u223C e \u227B f")
  
  r <- ranking(c(1, 2, 3, 3, 3, 4), letters[1:6])
  expect_output(print(r), "a \u227B b \u227B c \u223C d \u223C e \u227B f")
  
  r <- ranking(c(1, 2, 3, 3, 3, 3), letters[1:6])
  expect_output(print(r), "a \u227B b \u227B c \u223C d \u223C e \u223C f")
  
  r <- ranking(c(1, 2, 2, 2, 2, 3), letters[1:6])
  expect_output(print(r), "a \u227B b \u223C c \u223C d \u223C e \u227B f")
  
  r <- ranking(c(1, 1, 1, 2, 2, 3, 4, 5), letters[1:8])
  expect_output(print(r), "a \u223C b \u223C c \u227B d \u223C e \u227B f \u227B g \u227B h")
  
  # Not ordered elements
  
  r <- ranking(c(3, 5, 2, 4, 1), letters[1:5])
  expect_output(print(r), "e \u227B c \u227B a \u227B d \u227B b")
  
  r <- ranking(c(10, 7, 4, 3, 6, 1, 5, 9, 8, 2), letters[1:10])
  expect_output(print(r), "f \u227B j \u227B d \u227B c \u227B g \u227B e \u227B b \u227B i \u227B h \u227B a")
  
  # Non consequtive
  r <- ranking(c(18, 11, 10, 13, 17), letters[1:5])
  expect_output(print(r), "c \u227B b \u227B d \u227B e \u227B a")
  
  r <- ranking(c(27, 23, 43, 50, 29, 44, 37, 17, 20, 41), letters[1:10])
  expect_output(print(r), "h \u227B i \u227B b \u227B a \u227B e \u227B g \u227B j \u227B c \u227B f \u227B d")
  
})

test_that("Create numeric ranking with names", {
  
  r <- ranking(c("p", "t", "i", "r", "l"), letters[1:5])
  expect_output(print(r), "p \u227B t \u227B i \u227B r \u227B l ")
  
})

test_that("Parse ranking", {
  
  # TODO numeric 
  # TODO candidates names start with number
  # TODO with letters
  # TODO without ties
  
  r <- parse_ranking("a \u223C b \u223C c \u227B d \u223C e")
  expect_s3_class(r, "ranking")
  expect_equal(as.numeric(r), c(1, 1, 1, 2, 2))
  
  
  r <- parse_ranking("C2 \u223C C3 \u223C C4 \u227B C1 \u223C C5")
  expect_s3_class(r, "ranking")
  expect_equal(as.numeric(r), c(2, 1, 1, 1, 2))
  
  r <- parse_ranking("C3 \u223C C5 \u227B C4 \u227B C1 \u223C C2")
  expect_s3_class(r, "ranking")
  expect_equal(as.numeric(r), c(3, 3, 1, 2, 1))
  
  r <- parse_ranking("C1 \u227B C3 \u223C C4 \u223C C5 \u227B C2")
  expect_s3_class(r, "ranking")
  expect_equal(as.numeric(r), c(1, 3, 2, 2, 2))
  
})