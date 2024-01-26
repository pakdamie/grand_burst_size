test_that("Does it give two vectors back?", {
  suppressWarnings(
  expect_equal(length(Identifier_Burst_Size_Order(MAM_Merged)),
    2))
})

