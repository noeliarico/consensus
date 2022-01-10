test_that("2approval profiles of 4 candidates", {
  expect_output(print(tapproval(por01, 2)), "C2 ≻ C1 ≻ C3 ≻ C4")
  expect_output(print(tapproval(por02, 2)), "C1 ≻ C2 ≻ C3 ≻ C4")
  
})

test_that("3approval profiles of 4 candidates", {
  expect_output(print(tapproval(por01, 3)), "C1 ∼ C2 ≻ C4 ≻ C3")
  expect_output(print(tapproval(por02, 3)), "C1 ≻ C2 ≻ C3 ≻ C4")
})
